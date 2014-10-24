#' Simula e calcula os residuos para um modelo glm binomial
#'
#'
#'
#'
#'
calcula_e_bino <- function(modelo){
  nsim = 100
  s <- simulate(object = modelo, nsim = nsim)
  X <- model.matrix(modelo)
  e <- matrix(0,nrow(X),nsim)
  for(i in 1:100){
    fit <- glm(s[,i] ~ 0+ X, family=family(modelo))
    w <- fit$weights
    W <- diag(w)
    H <- solve(t(X)%*%W%*%X)
    H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
    h <- diag(H)
    e[,i] <- sort(resid(fit, type="deviance")/sqrt(1-h))
  }
  return(e)
}

#' Simula e calcula os residuos para um modelo glm binomial
#'
#'
#'
#'
#'
calcula_e_pois <- function(modelo){
  nsim = 100
  s <- simulate(object = modelo, nsim = nsim)
  X <- model.matrix(modelo)
  e <- matrix(0,nrow(X),nsim)
  for(i in 1:100){
    fit <- glm(s[,i] ~ 0+ X, family=family(modelo))
    w <- fit$weights
    W <- diag(w)
    H <- solve(t(X)%*%W%*%X)
    H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
    h <- diag(H)
    e[,i] <- sort(resid(fit, type="deviance")/sqrt(1-h))
  }
  return(e)
}