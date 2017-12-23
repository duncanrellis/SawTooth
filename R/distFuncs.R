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
  pdf <- 1 / sqrt(2 * pi * var) * exp(-((x - mu)^2)/(2 * var))
  attr(pdf, "dist") <- "normalPDF"

  return (pdf)
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
  pdf <- dpois(x, lamdba = lambda)
  attr(pdf, "dist") <- "poissonPDF"

  return (pdf)
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

  pdf <- RMKdiscrete::binegbin(x, r, p)
  attr(pdf, "dist") <- "negBinPDF"

  return (pdf)
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
  if (abs((n - n0)/n0) > .25) return (NULL)

  pdf <- dbinom(x, n, p)
  attr(pdf, "dist") <- "binPDF"

  return (pdf)
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

  pdf <- dgamma(x, shape = k, scale = theta)
  attr(pdf, "dist") <- "gammaPDF"

  return (pdf)
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

  pdf <- mu
  attr(pdf, "dist") <- "detPDF"

  return (pdf)
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
    pdf <- detPDF
    attr(pdf, "dist") <- "detPDF"

    return (pdf)
  } else if (mu >= 20) {
    if (sqrt(var) / mu < 2.325) {
      pdf <- normalPDF
      attr(pdf, "dist") <- "normalPDF"
      return (pdf)
    } else {
      pdf <- gammaPDF
      attr(pdf, "dist") <- "gammaPDF"
      return (pdf)
    }
  } else if (mu <= 20) {
    if (var >= .9 && var <= 1.1) {
      pdf <- poissonPDF
      attr(pdf, "dist") <- "poissonPDF"
      return (pdf)
    } else if (var / mu > 1.1) {
      pdf <- negBinPDF
      attr(pdf, "dist") <- "negBinPDF"
      return (pdf)
    }
  } else if (var / mu < .9) {
    pdf <- binPDF
    attr(pdf, "binPDF") <- "binPDF"
    return (pdf)
  } else {
    pdf <- gammaPDF
    attr(pdf, "dist") <- "gammaPDF"
    return (pdf)
  }
}

