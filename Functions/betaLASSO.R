# code from Daniel Schlaepfer for a custom function to implement a quasi-beta regression in gmlnet

# quasi because variance is `mu * (1 - mu)` instead of `mu * (1 - mu) / (1 + phi)` because
# glm-families require variance to be a function of `mu` only
quasibeta_family <- function(link = "logit") {
  linkobj <- stats::make.link(link)
  structure(
    list(
      family = "beta",
      link = link,
      linkfun = linkobj$linkfun,
      linkinv = linkobj$linkinv,
      variance = function(mu) mu * (1 - mu),
      dev.resids = function(y, mu, wt) {
        epsilon <- .Machine$double.eps
        y <- pmin(pmax(y, epsilon), 1 - epsilon)
        mu <- pmin(pmax(mu, epsilon), 1 - epsilon)
        -2 * wt * (y * log(y / mu) + (1 - y) * log((1 - y) / (1 - mu)))
      },
      mu.eta = linkobj$mu.eta,
      initialize = expression({
        if (any(y <= 0 | y >= 1)) {
          stop("Beta regression requires 0 < y < 1")
        }
        n <- rep(1, length(y))
        mustart <- (y * (length(y) - 1) + 0.5) / length(y)
      }),
      aic = function(y, n, mu, wt, dev) NA,
      validmu = function(mu) all(mu > 0 & mu < 1),
      valideta = linkobj$valideta
    ),
    class = "family"
  )
}

# testing
# nobs <- 100
# npreds <- 20
# x <- matrix(rnorm(nobs * npreds), nobs, npreds)
# y <- rbeta(n = nobs, shape1 = 1, shape2 = 2)

# m <- glmnet::glmnet(x, y, family = quasibeta_family())
