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
  # TODO Split out the case of thresholds as a column and single number
  # Probably inefficient to do this:
  if (length(t_w) == 1) t_w <- rep(t_w, n)
  if (length(t_d) == 1) t_d <- rep(t_d, n)
  if (length(t0) == 1) t0 <- rep(t0, n)
  
  # TODO Update to also use last value from previous month as parameter, y0
  # Set first value based only on today's rain and t0
  y[1] <- x[1] > t0[1]
  for (i in 2:n) {
    # If rain on previous day is NA
    # then use today's rain only with overall threshold
    if (is.na(y[i - 1])) y[i] <- x[i] > t0[i]
    else y[i] <- if(y[i - 1]) x[i] > t_w[i] else x[i] > t_d[i]
  }
  y
}

markov_thresholds <- function(data, obs_col = "obs", est_col = "est", 
                              date_col = "date", season_col, station_col,
                              obs_thr = 0.85, tol = 1e-2, max_it = 20, 
                              n_conv = 5, damping = 0.4, loci = TRUE,
                              qm_empirical = TRUE) {
  
  data[["year"]] <- lubridate::year(data[[date_col]])
  if (missing(season_col)) {
    data[["season"]] <- factor(lubridate::month(data[[date_col]]))
  } else {
    data <- data %>% rename(season = all_of(season_col))
  }
  season_col <- "season"
  data[["day"]] <- lubridate::day(data[[date_col]])
  obs <- data[[obs_col]]
  est <- data[[est_col]]
  # logical wet/dry from observations
  # Calculate lags before subsetting to ensure correct previous days
  data[["obs_wd"]] <- obs > obs_thr
  data[["obs_wd_prev"]] <- dplyr::lag(data[["obs_wd"]])
  
  results <- tibble(
    station = character(),
    season = character(),
    t0 = numeric(),
    t_w = numeric(),
    t_d = numeric(),
    p_obs = numeric(),
    p_est = numeric(),
    p_w_obs = numeric(),
    p_w_est = numeric(),
    p_d_obs = numeric(),
    p_d_est = numeric(),
    converged = logical(),
    within_tol = logical(),
    iterations = integer(),
    n_days = integer(),
    s_all = numeric(),
    s_wet = numeric(),
    s_dry = numeric()
  )
  if (!missing(station_col)) {
    stations <- unique(data[[station_col]])
    data <- data %>% rename(station = all_of(station_col))
    station_col <- "station"
  } else stations <- NA
  
  for (s in stations) {
    for (m in levels(data[[season_col]])) {
      if (!is.na(s)) data_m <- dplyr::filter(data, station == s & season == m)
      else data_m <- dplyr::filter(data, season == m)
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
      #print(m)
      # Set to TRUE if probabilities converged to within tolerance of targets
      converged <- FALSE
      # Set to TRUE if probabilities converged to a value but not within tolerance
      # of targets
      within_tol <- FALSE
      n_same <- 0
      # Iterate to converge to target probabilities
      for (i in 1:max_it) {
        #print(t_w)
        #print(t_d)
        res <- data_m %>%
          group_by(year, season) %>%
          group_modify(~{
            y0 <- .x$obs_wd_prev[1]
            if (is.na(y0)) y0 <- FALSE
            y  <- conditional_wd(.x[[est_col]], t_w, t_d, t0, y0)
            tibble(.x, est_wd = y, est_wd_prev = dplyr::lag(y, default = y0))
          }) %>%
          ungroup()
        # Update probabilities
        est_wd <- res$est_wd
        est_wd_prev <- res$est_wd_prev
        p_est_new <- mean(est_wd, na.rm = TRUE)
        p_w_est_new <- mean(est_wd[est_wd_prev], na.rm = TRUE)
        p_d_est_new <- mean(est_wd[!est_wd_prev], na.rm = TRUE)
        # Calculate difference between previous probabilities 
        # if not first iteration
        if (i > 1) {
          diff_p <- abs(p_est_new - p_est)
          diff_p_w <- abs(p_w_est_new - p_w_est)
          diff_p_d <- abs(p_d_est_new - p_d_est)
        }
        p_est <- p_est_new
        p_w_est <- p_w_est_new
        p_d_est <- p_d_est_new
        # Compare with target probabilities and stop if sufficiently converged
        # Use a tolerance based on number of observations
        tol_w <- 1 / max(sum(res$est_wd_prev, na.rm = TRUE), 1)
        tol_d <- 1 / max(sum(!res$est_wd_prev, na.rm = TRUE), 1)
        # Tolerance to check if probabilities have changed from previous iteration
        tol_diff_w <- tol_w / 2
        tol_diff_d <- tol_d / 2
        if (i > 1 && diff_p_w < tol_diff_w && diff_p_d < tol_diff_d) {
          n_same <- n_same + 1
        }
        if (is.na(p_w_est) || is.na(p_w_obs) || 
            is.na(p_d_est) || is.na(p_d_obs)) break
        # Stop if probabilities are within tolerance of targets
        else if ((abs(p_w_est - p_w_obs) < max(tol, tol_w) && 
                  abs(p_d_est - p_d_obs) < max(tol, tol_d))) {
          converged <- TRUE
          break
        # Stop if probabilities are converging within a tolerance 
        # after n_conv iterations
        } else if (n_same >= n_conv) {
          converged <- TRUE
          within_tol <- TRUE
          break
        }
        
        # Update thresholds based on current wet/dry and target probabilities
        if (i != max_it) {
          t_w_new <- quantile(res[[est_col]][res$est_wd_prev],
                              probs = 1 - p_w_obs, na.rm = TRUE, names = FALSE)
          t_w <- (1 - damping) * t_w + damping * t_w_new
          t_d_new <- quantile(res[[est_col]][!res$est_wd_prev],
                              probs = 1 - p_d_obs, na.rm = TRUE, names = FALSE)
          t_d <- (1 - damping) * t_d + damping * t_d_new
        }
      }
      s_wet <- NA
      s_dry <- NA
      if (loci) {
        obs_prev_wet <- data_m[[obs_col]][obs_wd_prev]
        obs_prev_dry <- data_m[[obs_col]][!obs_wd_prev]
        est_prev_wet <- data_m[[est_col]][est_wd_prev]
        est_prev_dry <- data_m[[est_col]][!est_wd_prev]
        s_obs_all <- mean(data_m[[obs_col]][data_m[[obs_col]] > obs_thr], 
                      na.rm = TRUE) - obs_thr
        s_est_all <- mean(data_m[[est_col]][data_m[[est_col]] > t0], 
                      na.rm = TRUE) - t0
        s_all <- s_obs_all / s_est_all
        
        s_obs_wet <- mean(obs_prev_wet[obs_prev_wet > obs_thr], 
                          na.rm = TRUE) - obs_thr
        s_obs_dry <- mean(obs_prev_dry[obs_prev_dry > obs_thr], 
                          na.rm = TRUE) - obs_thr
        # TODO Is it always correct to subtract t_w/t_d?
        # Could previous day rain have come from t0 or initial state?
        # What happens to rainfall days with no previous value - adjust by s_all?
        s_est_wet <- mean(est_prev_wet[est_prev_wet > t_w], 
                          na.rm = TRUE) - t_w
        s_est_dry <- mean(est_prev_dry[est_prev_dry > t_d], 
                          na.rm = TRUE) - t_d
        
        s_wet <- s_obs_wet / s_est_wet
        s_dry <- s_obs_dry / s_est_dry
      }
      if (qm_empirical) {
        
      }
      
      results <- results %>% 
        tibble::add_row(station = s, season = m, t0 = t0, t_w = t_w, t_d = t_d,
                        p_obs = p_obs, p_est = p_est, p_w_obs = p_w_obs, 
                        p_w_est = p_w_est, p_d_obs = p_d_obs, 
                        p_d_est = p_d_est, converged = converged, 
                        within_tol = within_tol, iterations = i,
                        n_days = nrow(data_m), 
                        s_all = s_all, s_wet = s_wet, s_dry = s_dry)
    }
  }
  if (all(is.na(stations))) results$station <- NULL
  if (all(is.na(results$s_all))) results$s_all <- NULL
  if (all(is.na(results$s_wet))) results$s_wet <- NULL
  if (all(is.na(results$s_dry))) results$s_dry <- NULL
  results
}

markov_loci <- function(data, obs_col = "obs", est_col = "est",
                        date_col = "date", season_col, station_col,
                        obs_thr = 0.85, tol = 1e-2, max_it = 20, n_conv = 5,
                        damping = 0.4, blocks) {
  #TODO Modify to use updated markov_thresholds function
  
  data <- data %>% 
    rename(date = all_of(date_col),
           season = all_of(season_col),
           station = all_of(station_col))
  result_list <- list()
  stations <- unique(data$station)
  c <- 1
  for (st in stations) {
    data_st <- data %>% filter(station == st)
    for (b in 1:(length(blocks) - 1)) {
      # data to calibrate parameters
      data_cal <- data_st %>% 
        filter(date >= blocks[b] & date < blocks[b + 1])
      # data to apply bias correction to
      data_apply <- data_st %>%
        filter(!(date >= blocks[b] & date < blocks[b + 1]))
      
      # Calculate thresholds on calibration data
      m_thresh <- markov_thresholds(data_cal, obs_col = obs_col, est_col = est_col, 
                                    date_col = date_col, season_col = season_col, 
                                    station_col = station_col, obs_thr = obs_thr, 
                                    tol = tol, max_it = max_it, n_conv = n_conv, 
                                    damping = damping)
      
      data_apply <- dplyr::left_join(data_apply, m_thresh,
                                     by = c("station", "season"))
      n <- nrow(data_apply)
      est <- data_apply[[est_col]]
      t0 <- data_apply[["t0"]]
      t_w <- data_apply[["t_w"]]
      t_d <- data_apply[["t_d"]]
      s <- data_apply[["s_all"]]
      s_wet <- data_apply[["s_wet"]]
      s_dry <- data_apply[["s_dry"]]
      est_loci <- numeric(n)
      est_loci[1] <- obs_thr + s[1] * (est[1] - t0[1])
      # NEXT check this is correct
      for (i in 2:n) {
        if (is.na(est_loci[i - 1])) {
          est_loci[i] <- obs_thr + s[i] * (est[i] - t0[i])
        } else if (est_loci[i - 1] > obs_thr) {
          est_loci[i] <- obs_thr + s_wet[i] * (est[i] - t_w[i])
        } else {
          est_loci[i] <- obs_thr + s_dry[i] * (est[i] - t_d[i])
        }
      }
      est_loci <- pmax(est_loci, 0)
      data_apply$est_loci <- est_loci
      result_list[[c]] <- data_apply
      c <- c + 1
    }
  }
  bind_rows(result_list)
}