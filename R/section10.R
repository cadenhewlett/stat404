#' Generate ANCOVA Analysis
#'
#' This function performs an ANCOVA analysis based on the input data. We construct a scalar ANCOVA model of the following form:
#' \deqn{y_{ij} = \eta + \tau_i + \beta (x_{ij} - \bar{x}_{..}) + \epsilon_{ij}}
#'
#' Where:
#' \itemize{
#'   \item \eqn{y_{i,j}}{y_{ij}} is the \eqn{i,j}{i,j}-th response.
#'   \item \eqn{\eta}{\eta} is the Grand Mean.
#'   \item \eqn{\tau_i}{\tau_i} is the effect of being assigned treatment \eqn{i}{i}.
#'   \item \eqn{\beta}{\beta} is the regression coefficient for the covariate.
#'   \item \eqn{x_{i, j}}{x_{ij}} is the covariate associated with \eqn{y_{i,j}}{y_{ij}}.
#'   \item \eqn{\bar{x}_{..}}{\bar{x}_{..}} is the overall mean of \eqn{x}{x} observations.
#'   \item \eqn{\epsilon_{i,j}}{\epsilon_{ij}} is assumed to be iid \eqn{N(0, \sigma^2)}{N(0, \sigma^2)} with \eqn{\sigma^2}{\sigma^2} the same for all levels.
#' }
#'
#' @param input A data frame, or three vectors corresponding to (x, y, treatment)
#' @return Returns the ANCOVA Table for the inputs.
#' @examples
#' ## EXAMPLE 1 : Three Inputs: x, y, and treatment
#' all_ancova(x = iris$Sepal.Length,
#'            y = iris$Sepal.Width,
#'            treat = iris$Species)
#' ## EXAMPLE 2 : Data Frame,  x, y, treatment inferred
#' all_ancova( iris[3:5] )
#' @export
all_ancova <- function(...) {
  # capture all inputs
  inputs <- list(...)

  # determine if inputs are a single dataframe or three vectors
  if (length(inputs) == 1 && is.data.frame(inputs[[1]])) {
    # single dataframe input
    df <- inputs[[1]]
    # ensure the dataframe has at least 3 columns
    if (ncol(df) < 3) {
      stop("Input dataframe must have at least 3 columns.")
    }
    # assign x, y, treatment from the first 3 columns of the dataframe
    x <- df[[1]]
    y <- df[[2]]
    treatment <- df[[3]]
  } else if (length(inputs) == 3) {
    # three individual vector inputs
    x <- inputs[[1]]
    y <- inputs[[2]]
    treatment <- inputs[[3]]
  } else {
    stop("Input must be either a single dataframe or three vectors (x, y, treatment).")
  }

  # TODO: additional prerequisite analytics for inputs

  # re-assign inputs to local data frame
  df = data.frame(x = x, y = y, treat = treatment)
  # define local variables
  N = length(df$x)
  levels = unique(df$treat)
  k = length(levels)
  # sum of squares
  sum_sq_x <- sapply(levels, function(lvl){
    # sum ( xi - bar(xi) )^2
    sum( (df$x[df$treat == lvl] - mean(df$x[df$treat == lvl]) )^2 )
  })
  sum_sq_xy <- sapply(levels, function(lvl){
    # sum (xi - bar(x))*(yi - bar(y)) for all levels
    sum( (df$x[df$treat == lvl] - mean(df$x[df$treat == lvl]))*
           (df$y[df$treat == lvl] - mean(df$y[df$treat == lvl])))
  })
  # beta hat coefficient
  sx = sum(sum_sq_x)
  sxy = sum(sum_sq_xy)
  beta_hat = sxy/sx
  # group-wise grand means
  xdotdot = mean(df$x)
  ydotdot = mean(df$y)
  # treatment level-wise means
  y_idot <- sapply(levels, function(lvl) mean(df$y[df$treat == lvl]))
  x_idot <- sapply(levels, function(lvl) mean(df$x[df$treat == lvl]))
  # estimated mean response for treatment level i
  # estimate mu hats by the equation above
  mus <- sapply(1:length(levels), function(i) {
    y_idot[i] - beta_hat*(x_idot[i] - xdotdot)
  } )
  # add the levels to our mu estimates for easier selection
  mu_df <- data.frame(levels, est = mus)
  # eta hat estimate
  eta_hat = ydotdot
  # predictions: map all treatments to their effects
  mu_vec = (sapply(df$treat, function(lvl) mu_df$est[mu_df$levels == lvl]))
  preds = mu_vec  +  beta_hat*(df$x - xdotdot)
  # residuals
  resids = df$y - preds
  # SSR directly from resids
  sserr <- sum(resids^2)
  # SS(reg) from derivation
  ssreg = (beta_hat^2)*sx
  # total sums of squares
  sum_tot_x = sum((df$x - xdotdot)^2)
  sum_tot_y = sum((df$y - ydotdot)^2)
  sum_tot_xy = sum ( (df$x - xdotdot)*(df$y - ydotdot) )
  # null regression params
  beta_hat_null = sum_tot_xy/sum_tot_x
  alpha_hat_null = mean(df$y) - beta_hat_null*mean(df$x)
  # null predictions
  df$null_preds =  alpha_hat_null + beta_hat_null*df$x
  # null ss(err)
  sserr_null = sum((df$y - df$null_preds)^2)
  # ancova ss(trt)
  sstrt =  sserr_null - sserr

  ## Begin Output Preparation ##
  dfs = c(1, k-1, N-k-1)
  # MSS from degfs
  mssreg = ssreg/(1)
  msstrt = sstrt/(k-1)
  msserr = sserr/(N-k-1)
  # f-stats
  f_r = mssreg / msserr
  f_s = msstrt / msserr
  # format ANCOVA table
  ancovatbl  <- data.frame(
    degf <- c(dfs, N-1),
    ss  <- c(ssreg, sstrt, sserr, sum_tot_y),
    mss <- c(mssreg, msstrt, msserr, NA),
    fst <- c(f_r, f_s, NA, NA)
  )

  colnames(ancovatbl) <-  c("Degrees of Freedom (df)", "Sum of Squares (SS)",
                            "Mean Square (MS)", "F-Statistic (F)")
  rownames(ancovatbl) <- c("Regression", "Treatment", "Residual/Error", "Total")

  return(signif(ancovatbl, digits = 4))
}
