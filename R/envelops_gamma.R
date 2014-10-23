#' Data frame para o grafico quantil quantil com banda de confiança para o modelo
#' gama
#'
#'
#'
#'
#'
envel_gamma_ <- function(fit.model, link = "log"){
  X <- model.matrix(fit.model)
  n <- nrow(X)
  p <- ncol(X)
  w <- fit.model$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  ro <- resid(fit.model,type="response")
  fi <- (n-p)/sum((ro/(fitted(fit.model)))^ 2)
  td <- resid(fit.model,type="deviance")*sqrt(fi/(1-h))
  #
  e <- matrix(0,n,100)
  #
  for(i in 1:100){
    resp <- rgamma(n,fi)
    resp <- (fitted(fit.model)/fi)*resp
    fit <- glm(resp ~ X, family=Gamma(link=link))
    w <- fit$weights
    W <- diag(w)
    H <- solve(t(X)%*%W%*%X)
    H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
    h <- diag(H)
    ro <- resid(fit,type="response")
    phi <- (n-p)/sum((ro/(fitted(fit)))^ 2)
    e[,i] <- sort(resid(fit,type="deviance")*sqrt(phi/(1-h)))}
  #
  e1 <- numeric(n)
  e2 <- numeric(n)
  #
  for(i in 1:n){
    eo <- sort(e[i,])
    e1[i] <- (eo[2]+eo[3])/2
    e2[i] <- (eo[97]+eo[98])/2}
  #
  med <- apply(e,1,mean)
  faixa <- range(td,e1,e2)
  
  df <- data.frame(
    media = sort(med)[rank(td)],
    lim.inf = sort(e1)[rank(td)],
    lim.sup = sort(e2)[rank(td)],
    res = td,
    quant = qqnorm(td, plot.it = F)$x
  )
  
  return(df)
}


#' Envelope do qqplot para a distribuição gamma usando ggplot2
#'
#'
envel_gamma_gg <- function(modelo, link="log"){
  df <- envel_gamma_(modelo, link)
  p <- ggplot2::ggplot(df, aes(x = quant)) + 
    geom_ribbon(aes(ymin = lim.inf, ymax = lim.sup), alpha = 0.3) +
    geom_line(aes(y = media), linetype = "dashed") + 
    geom_point(aes(y = res)) + 
    xlab("Quantil da N(0,1)") +
    ylab("Componente do Desvio")
  return(p)
}


#' Gráfico Quantil-Quantil com envelope para a distribuição Gamma.
#'
#' @param modelo modelo normal ajustado 
#' 
#'
#' @examples
#' clotting <- data.frame(
#'    u = c(5,10,15,20,30,40,60,80,100),
#'    lot1 = c(118,58,42,35,27,25,21,19,18),
#'    lot2 = c(69,35,26,21,18,16,13,12,12))
#' modelo <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
#' envel_gamma(modelo)
#' modelo %>% envel_gamma()
#' 
#' @import magrittr
#'
#' @export
envel_gamma <- function(modelo, link= "log"){
  envel_gamma_gg(modelo, link)
}

