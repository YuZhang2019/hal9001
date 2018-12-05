devtools::load_all()
library(Rsolnp)

n_obs <- 1000
W <- replicate(3, rbinom(n_obs, 1, 0.5))
A <- rbinom(n_obs, 1, plogis(rowSums(W[, -3])))
Y <- A - rowSums(W) + rnorm(n_obs)

hal_fit <- hal9001::fit_hal(X = as.matrix(cbind(W, A)), Y = Y,
                            return_x_basis = TRUE)

beta <- hal_fit$coefs[,1]

risk <- function(beta) {
  pred <- as.numeric(hal_fit$x_basis %*% beta[-1] + beta[1])
  mse <- mean((pred-Y)^2)
}

constraint <- function(beta){
  sum(abs(beta))
}

beta_init <- beta*0
res <- solnp(pars=beta_init, risk, ineqfun= constraint, ineqLB=0, ineqUB=constraint(beta))
new_coefs <- res$pars
