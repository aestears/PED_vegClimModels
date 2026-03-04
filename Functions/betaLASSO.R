# code from Daniel Schlaepfer for a custom function to implement a quasi-beta regression in gmlnet

# # quasi because variance is `mu * (1 - mu)` instead of `mu * (1 - mu) / (1 + phi)` because
# # glm-families require variance to be a function of `mu` only
# quasibeta_family <- function(link = "logit") {
#   linkobj <- stats::make.link(link)
#   structure(
#     list(
#       family = "beta",
#       link = link,
#       linkfun = linkobj$linkfun,
#       linkinv = linkobj$linkinv,
#       variance = function(mu) mu * (1 - mu),
#       dev.resids = function(y, mu, wt) {
#         epsilon <- .Machine$double.eps
#         y <- pmin(pmax(y, epsilon), 1 - epsilon)
#         mu <- pmin(pmax(mu, epsilon), 1 - epsilon)
#         -2 * wt * (y * log(y / mu) + (1 - y) * log((1 - y) / (1 - mu)))
#       },
#       mu.eta = linkobj$mu.eta,
#       initialize = expression({
#         if (any(y <= 0 | y >= 1)) {
#           stop("Beta regression requires 0 < y < 1")
#         }
#         n <- rep(1, length(y))
#         mustart <- (y * (length(y) - 1) + 0.5) / length(y)
#       }),
#       aic = function(y, n, mu, wt, dev) NA,
#       validmu = function(mu) all(mu > 0 & mu < 1),
#       valideta = linkobj$valideta
#     ),
#     class = "family"
#   )
# }
# 
# # testing
# # nobs <- 100
# # npreds <- 20
# # x <- matrix(rnorm(nobs * npreds), nobs, npreds)
# # y <- rbeta(n = nobs, shape1 = 1, shape2 = 2)
# 
# # m <- glmnet::glmnet(x, y, family = quasibeta_family())


# betareg::betar_family()
# glmmTMB::beta_family()

# updated version (note dev.resids() and initialize())
# quasi because variance is `mu * (1 - mu)` instead of `mu * (1 - mu) / (1 + phi)`
# because glm-families require variance to be a function of `mu` only
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
        2 * wt * (y * log(y / mu) + (1 - y) * log((1 - y) / (1 - mu)))
      },
      mu.eta = linkobj$mu.eta,
      initialize = expression({
        if (any(y <= 0 | y >= 1)) {
          stop("Beta regression requires 0 < y < 1")
        }
        mustart <- y
      }),
      aic = function(...) NA,
      validmu = function(mu) all(mu > 0 & mu < 1),
      valideta = linkobj$valideta
    ),
    class = "family"
  )
}

# beta variance with precision parameter
quasibetaPhi_family <- function(link = "logit", phi = 1) {
  linkobj <- stats::make.link(link)
  
  structure(
    list(
      family = "quasi-beta",
      link = link,
      linkfun = linkobj$linkfun,
      linkinv = linkobj$linkinv,
      variance = function(mu) mu * (1 - mu) / (1 + phi),
      mu.eta = linkobj$mu.eta,
      dev.resids = function(y, mu, wt) {
        epsilon <- .Machine$double.eps
        y <- pmin(pmax(y, epsilon), 1 - epsilon)
        mu <- pmin(pmax(mu, epsilon), 1 - epsilon)
        2 * wt * (y * log(y / mu) + (1 - y) * log((1 - y) / (1 - mu)))
      },
      initialize = expression({
        if (any(y <= 0 | y >= 1)) {
          stop("Beta regression requires 0 < y < 1")
        }
        mustart <- y
      }),
      aic = function(...) NA,
      validmu = function(mu) all(mu > 0 & mu < 1),
      valideta = linkobj$valideta
    ),
    class = "family"
  )
}

# ## testing
# #--- Simulated data ---
# set.seed(123)
# 
# # Sample size
# n <- 500
# 
# # Simulate predictors
# x <- cbind(
#   rnorm(n, 0, 1),
#   rnorm(n, 0, 1),
#   rbinom(n, 1, 0.5) # binary predictor
# )
# 
# # True coefficients
# betas <- c(-0.5, 1.0, -1.5, 0.8)
# 
# # Linear predictor
# eta <- betas[[1]] + drop(x %*% betas[-1])
# 
# # Inverse logit to map to (0,1)
# mu <- plogis(eta)
# 
# # Precision parameter
# phi <- exp(1 + 0.5 * x[, 1])   # log-link for precision
# 
# # Convert to Beta parameters
# alpha <- mu * phi
# beta <- (1 - mu) * phi
# 
# # Simulate response
# y <- rbeta(n, shape1 = alpha, shape2 = beta)
# y[y >= 1] <- 1 - sqrt(.Machine[["double.eps"]])
# 
# 
# #--- Fit old quasibeta with glmnet ---
# mq0 <- glmnet::glmnet(x, y, family = quasibetaOld_family())
# yhat0 <- predict(mq0, newx = x, s = min(mq0$lambda), type = "response")
# plot(yhat0, y, xlim = c(0, 1), ylim = c(0, 1), main = "Quasi-Beta (old)"); abline(0, 1, col = "red")
# 
# #--- Fit updated quasibeta with glmnet ---
# mq1 <- glmnet::glmnet(x, y, family = quasibeta_family())
# yhat1 <- predict(mq1, newx = x, s = min(mq1$lambda), type = "response")
# plot(yhat1, y, xlim = c(0, 1), ylim = c(0, 1), main = "Quasi-Beta (updated)"); abline(0, 1, col = "red")
# 
# 
# #--- Fit a (true) beta model ---
# mb <- betareg::betareg(y ~ x)
# yhatb <- predict(mb, newdata = data.frame(x), type = "response")
# plot(yhatb, y, xlim = c(0, 1), ylim = c(0, 1)); abline(0, 1, col = "red")
# 
# plot(yhatb, yhat1, xlim = c(0, 1), ylim = c(0, 1)); abline(0, 1, col = "red")
# 
# 
# #--- Fit a quasibeta model with glm (old) ---
# mg0 <- glm(y ~ x, family = quasibetaOld_family())
# yhatg0 <- predict(mg0, newdata = data.frame(x), type = "response")
# plot(yhatg0, y, xlim = c(0, 1), ylim = c(0, 1)); abline(0, 1, col = "red")
# 
# 
# #--- Fit a quasibeta model with glm (updated) ---
# mg1 <- glm(y ~ x, family = quasibeta_family())
# yhatg1 <- predict(mg1, newdata = data.frame(x), type = "response")
# plot(yhatg1, y, xlim = c(0, 1), ylim = c(0, 1)); abline(0, 1, col = "red")
# 
# plot(yhatg0, yhatg1, xlim = c(0, 1), ylim = c(0, 1)); abline(0, 1, col = "red")
# 
# 
# 
# #----------------------
# #--- Fit a quasibeta model with precision (v2) ---
# fit0 <- glm(y ~ x, family = quasibetaPhi_family(phi = 1))
# r <- residuals(fit0, type = "pearson")
# n <- nobs(fit0)
# p <- length(coef(fit0))
# phi_hat <- max(sum(r^2) / (n - p) - 1, 0.001)
# phi_hat
# mq2 <- glm(y ~ x, family = quasibetaPhi_family(phi = phi_hat))
# yhat <- predict(mq2, newdata = data.frame(x), type = "response")
# plot(yhat, y, xlim = c(0, 1), ylim = c(0, 1)); abline(0, 1, col = "red")

