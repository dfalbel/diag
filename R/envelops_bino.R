#' Data frame para o grafico quantil quantil com banda de confiança para o modelo
#' poisson
#'
#'
#'
#'
#'
envel_pois_ <- function(fit.model, link = "log"){
  
  X <- model.matrix(fit.model)
  n <- nrow(X)
  p <- ncol(X)
  w <- fit.model$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  td <- resid(fit.model,type="deviance")/sqrt((1-h))
  e <- matrix(0,n,100)
  #
  for(i in 1:100){
    nresp <- rpois(n, fitted(fit.model))
    fit <- glm(nresp ~ X, family=poisson(link = link))
    w <- fit$weights
    W <- diag(w)
    H <- solve(t(X)%*%W%*%X)
    H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
    h <- diag(H)
    e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
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


#' Envelope do qqplot para a distribuição poisson usando ggplot2
#'
#'
envel_pois_gg <- function(modelo, link="log"){
  df <- envel_pois_(modelo, link)
  p <- ggplot2::ggplot(df, aes(x = quant)) + 
    geom_ribbon(aes(ymin = lim.inf, ymax = lim.sup), alpha = 0.3) +
    geom_line(aes(y = media), linetype = "dashed") + 
    geom_point(aes(y = res)) + 
    xlab("Quantil da N(0,1)") +
    ylab("Componente do Desvio")
  return(p)
}


#' Gráfico Quantil-Quantil com envelope para a distribuição Poisson.
#'
#' @param modelo modelo normal ajustado 
#' @param link ligacao utilizada no modelo, default é "log".
#'
#' @examples
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' modelo <- glm(counts ~ outcome + treatment, family = poisson())
#' modelo %>% envel_pois()
#' 
#' @import magrittr
#'
#' @export
envel_pois <- function(modelo, link= "log"){
  envel_pois_gg(modelo, link)
}

