#' Data frame para o grafico quantil quantil com banda de confiança para o modelo
#' poisson
#'
#'
#'
#'
#'
envel_bino_ <- function(fit.model){
  
  X <- model.matrix(fit.model)
  n <- nrow(X)
  p <- ncol(X)
  w <- fit.model$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  td <- resid(fit.model,type="deviance")/sqrt((1-h))
  
  e <- calcula_e_bino(modelo = fit.model)
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
envel_bino_gg <- function(modelo){
  df <- envel_bino_(modelo)
  p <- ggplot2::ggplot(df, aes(x = quant)) + 
    geom_ribbon(aes(ymin = lim.inf, ymax = lim.sup), alpha = 0.3) +
    geom_line(aes(y = media), linetype = "dashed") + 
    geom_point(aes(y = res)) + 
    xlab("Quantil da N(0,1)") +
    ylab("Componente do Desvio")
  return(p)
}


#' Gráfico Quantil-Quantil com envelope para a distribuição Binomial.
#'
#' @param modelo modelo normal ajustado 
#' 
#'
#' @examples
#' cuse <- read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", 
#'     header=TRUE)
#' modelo <- glm(cbind(using, notUsing) ~age + education + wantsMore , 
#'                  family = binomial, data = cuse)
#' modelo %>% envel_bino()
#' 
#' @import magrittr
#'
#' @export
envel_bino <- function(modelo){
  envel_bino_gg(modelo)
}

