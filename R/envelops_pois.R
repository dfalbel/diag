#' Data frame para o grafico quantil quantil com banda de confiança para o modelo
#' poisson
#'
#'
#'
#'
#'
envel_pois_ <- function(fit.model){
  
  X <- model.matrix(fit.model)
  k <- nrow(X)
  e <- matrix(0,k,100)
  tot <- numeric(k)
  w <- fit.model$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  td <- sort(resid(fit.model, type="deviance")/sqrt(1-h))
  #
  e <- calcula_e_pois(fit.model)
  #
  e1 <- numeric(k)
  e2 <- numeric(k)
  #
  for(i in 1:k){
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
envel_pois_gg <- function(modelo){
  df <- envel_pois_(modelo)
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
#' 
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
envel_pois <- function(modelo){
  envel_pois_gg(modelo)
}

