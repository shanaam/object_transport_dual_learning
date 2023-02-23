library(psych) # for descriptives
library(effsize) # for Cohen's d
library(BayesFactor) # to compute Bayes factors


# ----
# Analysis
# input = a vector
# works well with group_by %>% summarise()
vector_confint <- function(vector, interval = 0.95) {
  # Standard deviation of sample
  vec_sd <- sd(vector, na.rm = TRUE)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector, na.rm = TRUE)
  # Error according to t distribution
  error <- qt((interval + 1) / 2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  # result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(error)
}

# atan2 using x and y coordinates
applyAtan2 <- function(df) {
  x <- df[1] - df[3]
  y <- df[2] - df[4]
  ang <- df[5] * -1 * pi / 180 # convert to rads

  x_r <- (x * cos(ang)) - (y * sin(ang))
  y_r <- (x * sin(ang)) + (y * cos(ang))

  return(atan2(y_r, x_r) * 180 / pi) # atan2(y,x) -- atan2 takes y first
}

# bootstrapped confidence intervals
# input = a vector
bootstr_confint <- function(vector, interval = 0.95) {
  # incomplete
}

# slope fitting function for the decay data
decay_fit <- function(x_vec, y_vec) {
  # Fit a line to the data
  lm_fit <- lm(y_vec ~ x_vec)

  # Get the slope and intercept
  coefs <- coef(lm_fit)

  # return the slope and intercept
  return(coefs)
}


reg_confints <- function(x, y) {
  n <- length(y) # Find length of y to use as sample size
  lm.model <- lm(y ~ x) # Fit linear model

  # Extract fitted coefficients from model object
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]

  # Find SSE and MSE
  sse <- sum((y - lm.model$fitted.values)^2)
  mse <- sse / (n - 2)

  t.val <- qt(0.995, n - 2) # Calculate critical t-value

  # Fit linear model with extracted coefficients
  x_new <- 1:max(x)
  y.fit <- b1 * x_new + b0

  # Find the standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))

  # Fit a new linear model that extends past the given data points (for plotting)
  # x_new2 <- 1:max(x + 100)
  # y.fit2 <- b1 * x_new2 + b0

  # Warnings of mismatched lengths are suppressed
  slope.upper <- suppressWarnings(y.fit + t.val * se)
  slope.lower <- suppressWarnings(y.fit - t.val * se)

  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(slope.lower, slope.upper))
  colnames(bands) <- c("Lower Confidence Band", "Upper Confidence Band")

  # Plot the fitted linear regression line and the computed confidence bands
  # plot(x, y, cex = 1.75, pch = 21, bg = 'gray')
  # lines(y.fit, col = 'black', lwd = 2)
  # lines(bands[1], col = 'blue', lty = 2, lwd = 2)
  # lines(bands[2], col = 'blue', lty = 2, lwd = 2)

  return(bands)
}


# get the magnitude (euclidian normal) of a vector (this is faster than R's built in norm)
norm_vec <- function(vector) {
  sqrt(crossprod(vector))
}

# load data using fread
loadData <- function(path) {
  data_df <- fread(path, stringsAsFactors = TRUE)

  return(data_df)
}

# load in the baseline corrected data
load_bl_corrected <- function(path, type = "nocursor") {
  # load in the large df
  rot_all <- read_delim(path,
    delim = ",",
    col_types = cols(
      .default = col_double(),
      targetangle_deg = col_factor(),
      strat_use = col_factor(),
      ppt = col_factor(),
      exp = col_factor(),
      block_num = col_factor(),
      reach_type = col_factor()
    )
  )

  # separate the nocursor and training data
  rot_nocur <- rot_all %>%
    filter(
      reach_type == type
    )

  return(rot_nocur)
}

# Plotting functions
plot_desc_group_density <- function(df, group, y, subgroup = NULL, title = "Distributions within Groups") {
  # plot some density things for visualizing data in groups
  # group, and y are strings
  if (is.null(subgroup)) {
    p <- df %>%
      ggplot(aes(.data[[group]], .data[[y]])) +
      geom_violin(aes(fill = .data[[group]]), alpha = 0.2, draw_quantiles = c(.25, .5, .75), scale = "count") +
      geom_beeswarm(alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", size = 3, color = "red")
  } else {
    p <- df %>%
      ggplot(aes(.data[[group]], .data[[y]], colour = .data[[subgroup]])) +
      geom_beeswarm(dodge.width = .9, alpha = 0.3) +
      geom_violin(aes(fill = .data[[subgroup]]), alpha = 0.2, draw_quantiles = c(.25, .5, .75), scale = "count")
  }

  p <- p +
    theme_minimal() +
    theme(panel.grid.major.y = element_line(colour = "#CCCCCC")) +
    ggtitle(title)

  return(p)
}

plot_nice_group_density <- function(df, desc_df, group, y, subgroup = NULL, title = "Distributions within Groups") {
  # plot some density things for visualizing data in groups
  # group, and y are strings
  # plot some density things for visualizing data in groups
  # group, and y are strings

  desc_temp <- desc_df

  if (is.null(subgroup)) {
    p <- df %>%
      ggplot(aes(.data[[group]], .data[[y]], colour = .data[[group]])) +
      geom_beeswarm(alpha = 0.2) +
      geom_linerange(
        data = desc_temp,
        aes(.data[[group]], means,
          ymin = means - ci, ymax = means + ci
        ),
        lwd = 2, alpha = 0.8
      ) +
      geom_point(
        data = desc_temp,
        aes(y = means),
        size = 2, alpha = 1
      )
  } else {
    p <- df %>%
      ggplot(aes(!!group_tidy, !!y_tidy, colour = !!subgroup_tidy)) +
      geom_beeswarm(dodge.width = .6, alpha = 0.2) +
      geom_linerange(
        data = desc_temp,
        aes(!!group_tidy, means,
          colour = !!subgroup_tidy,
          ymin = means - ci, ymax = means + ci
        ),
        lwd = 3, alpha = 0.5,
        position = position_dodge(width = .6)
      )
  }

  p <- p +
    theme_minimal() +
    theme(panel.grid.major.y = element_line(colour = "#CCCCCC")) +
    ggtitle(title)

  return(p)
}

# ----
# from paper (find)
bayes_t_test <- function(df, group_title, group1, group2, dv) {
  # bayes t-test

  data <- df %>%
    group_by(NULL) %>%
    filter(.data[[group_title]] == group1 | .data[[group_title]] == group2) %>%
    select(all_of(group_title), all_of(dv))

  res.bayes <- data.frame(matrix(NA, nrow = 1, ncol = 4))
  rownames(res.bayes) <- colnames(data)[-1]
  colnames(res.bayes) <- c("BF01", "Postd.Median", "Postd.LB", "Postd.UB")

  # the test

  i <- 1 # this is useful for iterating
  # Note: code from https://osf.io/gfdjy/
  # Removing NAs
  data.noNA <- data[!is.na(data[, i + 1]), ]

  # Identify independent groups,
  # BF01:
  tmp <- ttestBF(pull(filter(data.noNA, .data[[group_title]] == group1)[, dv]),
    pull(filter(data.noNA, .data[[group_title]] == group2)[, dv]),
    rscale = .707
  )
  BF01 <- 1 / extractBF(tmp)$bf

  # Posterior distribution for BF01:
  set.seed(i)
  tmp <- ttestBF(pull(filter(data.noNA, .data[[group_title]] == group1)[, dv]),
    pull(filter(data.noNA, .data[[group_title]] == group2)[, dv]),
    posterior = TRUE,
    iterations = 10000,
    rscale = .707
  )
  CI.bayes <- quantile(tmp[, "delta"], probs = c(.03, .97))

  res.bayes[i, ] <- c(BF01, median(tmp[, "delta"]), CI.bayes)

  # show
  # res.bayes

  #    2.6 Posterior model probabilities for equal prior odds ----
  # post.H0:
  null <- res.bayes$BF01 / (1 + res.bayes$BF01)
  # post.H1:
  alt <- 1 / (1 + res.bayes$BF01)

  bf10 <- 1 / res.bayes$BF01

  return(sprintf("BF10: %.3f, P(D|H0): %.3f, P(D|H1): %.3f, Groups: %s vs %s", bf10, null, alt, group1, group2))
}