# Direct replacement

direct_replace <- function(obs, est) {
  ix <- is.na(obs)
  obs[ix] <- est[ix]
  obs
}

# Threshold adjustment
thresh <- function(obs, est, obs_thresh = 0.85) {
  id <- !is.na(obs) & !is.na(est)
  p_obs <- mean(obs[id] > obs_thresh, na.rm = TRUE)
  quantile(est[id], 1 - p_obs)[[1]]
}

s_loci <- function(obs, est, obs_thresh = 0.85, est_thresh = NULL) {
  if (is.null(est_thresh)) est_thresh <- thresh(obs, est, obs_thresh)
  id <- !is.na(obs) & !is.na(est)
  s_obs <- mean(obs[id & obs > obs_thresh]) - obs_thresh
  s_est <- mean(est[id & est > est_thresh]) - est_thresh
  s <- s_obs / s_est
  s
}

# Local Intensity Scaling
loci <- function(obs, est, obs_thresh = 0.85) {
  est_thresh <- thresh(obs, est, obs_thresh)
  s <- s_loci(obs, est, obs_thresh, est_thresh)
  est_loci <- obs_thresh + s * (est - est_thresh)
  est_loci <- pmax(est_loci, 0)
  est_loci
}

# Quantile Mapping
fit_gamma <- function(obs, obs_thresh = 0.85, 
                      method = c("mle", "mme", "qme", "mge", "mse")) {
  obs <- obs[obs > obs_thresh] - obs_thresh
  obs <- as.numeric(na.omit(obs))
  if(length(obs) > 5) fitdistrplus::fitdist(obs, distr = "gamma", method = method)
  else NULL
}

fit_empirical <- function(obs, obs_thresh = 0.85) {
  obs <- obs[obs > obs_thresh]
  ecdf(obs)
}

quantile_mapping <- function(obs, est, obs_thresh = 0.85, 
                             qm_method = c("gamma", "empirical"), 
                             gamma_method = c("mle", "mme", "qme", 
                                              "mge", "mse")) {
  est_thresh <- thresh(obs, est, obs_thresh)
  qm_method <- match.arg(qm_method)
  if (qm_method == "gamma") {
    obs_gamma <- fit_gamma(obs, obs_thresh, method = gamma_method)
    est_gamma <- fit_gamma(est, est_thresh, method = gamma_method)
    obs_shape = obs_gamma[["estimate"]][["shape"]]
    obs_rate = obs_gamma[["estimate"]][["rate"]]
    est_shape = est_gamma[["estimate"]][["shape"]]
    est_rate = est_gamma[["estimate"]][["rate"]]
    est_qm <- if_else(est <= est_thresh, 0,
                      qgamma(pgamma(est - est_thresh, shape = est_shape, 
                                    rate = est_rate),
                             shape = obs_shape, rate = obs_rate) + obs_thresh)
  } else if (qm_method == "empirical") {
    obs_rain <- obs[obs > obs_thresh]
    est_ecdf <- fit_empirical(est, est_thresh)
    est_qm <- if_else(est <= est_thresh, 0,
                      as.numeric(quantile(obs_rain, est_ecdf(est), 
                                          na.rm = TRUE)))
  }
  return(est_qm)
}

# Calculate logical wet/dry on x using t_w and t_d
conditional_wd <- function(x, t_w, t_d, t0, y0) {
  n <- length(x)
  y <- logical(n)
  # TODO Update to also use last value from previous month as parameter, y0
  # Set first value based only on today's rain and t0
  y[1] <- x[1] > t0
  for (i in 2:n) {
    if (is.na(y[i - 1])) y[i] <- x[i] > t0
    else y[i] <- if(y[i - 1]) x[i] > t_w else x[i] > t_d
  }
  y
}

markov_thresholds <- function(data, obs_col = "obs", est_col = "est", 
                              date_col = "date", season_col,
                              obs_thr = 0.85, tol = 1e-3, max_it = 20) {
  
  data[["year"]] <- lubridate::year(data[[date_col]])
  # NExT rename season_col to "season" in all cases for simplicity
  if (missing(season_col)) {
    data[["season"]] <- factor(lubridate::month(data[[date_col]]))
  } else {
    data[["season"]] <- data[[season_col]]
  }
  season_col <- "season"
  data[["day"]] <- lubridate::day(data[[date_col]])
  # Not needed?
  # season_list <- apply(tidyr::crossing(unique(data[["year"]]), 
  #                                      levels(data[[season_col]])), 
  #                      1, paste, collapse="-")
  # data[["ym"]] <- factor(paste(data[["year"]], data[[season_col]], sep = "-"),
  #                        levels = season_list)
  obs <- data[[obs_col]]
  est <- data[[est_col]]
  # logical wet/dry from observations
  # Calculate lags before subsetting to ensure correct previous days
  data[["obs_wd"]] <- obs > obs_thr
  data[["obs_wd_prev"]] <- dplyr::lag(data[["obs_wd"]])
  
  results <- tibble(
    season = character(),
    t_w = numeric(),
    t_d = numeric(),
    p_obs = numeric(),
    p_est = numeric(),
    p_w_obs = numeric(),
    p_w_est = numeric(),
    p_d_obs = numeric(),
    p_d_est = numeric(),
    converged = logical(),
    iterations = integer(),
    n_days = integer()
  )

  for (m in levels(data[[season_col]])) {
    data_m <- dplyr::filter(data, season == m)
    est_m <- data_m[[est_col]]
    obs_wd <- data_m[["obs_wd"]]
    obs_wd_prev <- data_m[["obs_wd_prev"]]
    
    # Target probabilities
    p_obs <- mean(obs_wd, na.rm = TRUE)
    p_w_obs <- mean(obs_wd[obs_wd_prev], na.rm = TRUE)
    p_d_obs <- mean(obs_wd[!obs_wd_prev], na.rm = TRUE)
    
    # Initial threshold based on p_obs
    t0 <- quantile(est_m, probs = 1 - p_obs, na.rm = TRUE, 
                   names = FALSE)
    t_d <- t0
    t_w <- t0
    print(m)
    converged <- FALSE
    # Iterate to converge to target probabilities
    for (i in 1:max_it) {
      print(t_w)
      print(t_d)
      res <- data_m %>%
        group_by(year, season) %>%
        group_modify(~{
          y0 <- .x$obs_wd_prev[1]
          if (is.na(y0)) y0 <- FALSE
          y  <- conditional_wd(.x[[est_col]], t_w, t_d, t0, y0)
          tibble(.x, est_wd = y, est_wd_prev = dplyr::lag(y, default = y0))
        }) %>%
        ungroup()
      # Calculate current probabilities
      p_est <- mean(res$est_wd, na.rm = TRUE)
      p_w_est <- mean(res$est_wd[res$est_wd_prev], na.rm = TRUE)
      p_d_est <- mean(res$est_wd[!res$est_wd_prev], na.rm = TRUE)
      print(abs(p_w_est - p_w_obs))
      print(abs(p_d_est - p_d_obs))
      print(sum(res$est_wd_prev, na.rm = TRUE))
      print(sum(!res$est_wd_prev, na.rm = TRUE))
      # Compare with target probabilities and stop if sufficiently converged
      # Use a tolerance based on number of observations
      # TODO If t0 = 0 then separate case, may only be able to have
      # one of t_w or t_d > 0. Should converge if one probability is close.
      tol_w <- 1 / max(sum(res$est_wd_prev, na.rm = TRUE), 1)
      tol_d <- 1 / max(sum(!res$est_wd_prev, na.rm = TRUE), 1)
      if (is.na(p_w_est) || is.na(p_w_obs) || 
          is.na(p_d_est) || is.na(p_d_obs)) break
      else if (abs(p_w_est - p_w_obs) < max(tol, tol_w) && 
               abs(p_d_est - p_d_obs) < max(tol, tol_d)) {
        converged <- TRUE
        break
      }
      
      # Update thresholds based on current wet/dry and target probabilities
      if (i != max_it) {
        rho <- 0.4
        t_w_new <- quantile(res[[est_col]][res$est_wd_prev],
                            probs = 1 - p_w_obs, na.rm = TRUE, names = FALSE)
        t_w <- (1 - rho) * t_w + rho * t_w_new
        t_d_new <- quantile(res[[est_col]][!res$est_wd_prev],
                            probs = 1 - p_d_obs, na.rm = TRUE, names = FALSE)
        t_d <- (1 - rho) * t_d + rho * t_d_new
      }
    }
    results <- results %>% 
      tibble::add_row(season = m, t_w = t_w, t_d = t_d, p_obs = p_obs, 
                      p_est = p_est, p_w_obs = p_w_obs, p_w_est = p_w_est, 
                      p_d_obs = p_d_obs, p_d_est = p_d_est, 
                      converged = converged,
                      iterations = i,
                      n_days = nrow(data_m))
  }
  results
}
