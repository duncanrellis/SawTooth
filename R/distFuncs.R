##' Probility density function for normal distribution
##'
##' Normal PDF, supports fill-rate calculator of [estimateFR()]
##' @param x numeric, quantile
##' @param mu numeric, mean
##' @param var numeric, variance
##' @return numeric
##' @keywords internal
##' @md
normalPDF <- function(x, mu = 1, var = 1) {
  1 / sqrt(2 * pi * var) * exp(-((x - mu)^2)/(2 * var))
}
##' Problem mass function for poisson distribution
##'
##' Poisson PMF, supports fill-rate calculator of [estimateFR()]
##' @param x numeric, quantile
##' @param mu numeric, mean
##' @param var numeric, variance
##' @return numeric
##' @keywords internal
##' @md
poissonPDF <- function(x, mu, var) {

  lamdba <- mu
  dpois(x, lamdba = lambda)
}
##' Probability mass function for negative binomial distribution
##'
##' Negative binomial PMF, supports fill-rate calculator of [estimateFR()]
##' @param x numeric, quantile
##' @param mu numeric, mean
##' @param var numeric, variance
##' @return numeric
##' @keywords internal
##' @md
negBinPDF <- function(x, mu, var) {
  p <- 1 - (mu / var)
  r <- mu^2 / (var - mu) #failure

  ## return NULL and allow calling function to pick a more suitable distribution
  if (r < 1) return (NULL)

  RMKdiscrete::binegbin(x, r, p)
}
##' Probability mass function for binomial distribution
##'
##' Binomial PMF, supports fill-rate calculator of [estimateFR()]
##' @param x numeric, quantile
##' @param mu numeric, mean
##' @param var numeric, variance
##' @return numeric
##' @keywords internal
##' @md
binPDF <- function(x, mu, var) {
  n0 = mu^2 / (mu - var)
  n <- ceiling(n0)
  p <- mu / n

  ## If adjustment to binPDF n is too large, don't accept
  if ((abs(n - n0)/n0) > .25) return (NULL)

  dbinom(x, n, p)

}
##' Probability mass function for gamma distribution
##'
##' Gamma PDF, supports fill-rate calculator of [esimateFR()]
##' @param x numeric, quantile
##' @param mu numeric, mean
##' @param var numeric, variance
##' @return numeric
##' @keywords internal
##' @md
gammaPDF <- function(x, mu, var) {
  k = mu^2 / var
  theta = var / mu

  dgamma(x, shape = k, scale = theta)
}
##' Deterministic probability function
##'
##' Returns sample mean, supports fill-rate calculator of [estimateFR()]
##' @param x numeric, quantile
##' @param mu numeric, mean
##' @param var numeric, variance
##' @return numeric
##' @keywords internal
##' @md
detPDF <- function(x, mu, var) {
  return(mu)
}
##' Returns appropriate probability distribution for [estimateFR()]
##'
##' Returns best probability distribution for use in [estimateFR()]
##' @param mu numeric, mean
##' @param var numeric, variance
##' @return numeric
##' @keywords internal
##' @md
getFunc <- function(mu, var) {

  if (sqrt(var) / mu < .01) {
    return (detPDF)
  } else if (mu >= 20) {
    if (sqrt(var) / mu < 2.325) {
      return (normalPDF)
    } else {
      return (gammaPDF)
    }
  } else if (mu <= 20) {
    if (var >= .9 && var <= 1.1) {
      return (poissonPDF)
    } else if (var / mu > 1.1) {
      return (negBinPDF)
    }
  } else if (var / mu < .9) {
    return (binPDF)
  } else {
    return (gammaPDF)
  }
}

