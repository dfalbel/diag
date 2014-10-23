#' Diagnóstico para o modelo normal
#'
#'
#'
#'
envel_norm_ <- function(fit.model, dados = fit.model$model){
  X <- model.matrix(fit.model)
  n <- nrow(X)
  p <- ncol(X)
  H <- X%*%solve(t(X)%*%X)%*%t(X)
  h <- diag(H)
  si <- lm.influence(fit.model)$sigma
  r <- resid(fit.model)
  tsi <- r/(si*sqrt(1-h))
  #
  ident <- diag(n)
  epsilon <- matrix(0,n,100)
  e <- matrix(0,n,100)
  e1 <- numeric(n)
  e2 <- numeric(n)
  #
  for(i in 1:100){
    epsilon[,i] <- rnorm(n,0,1)
    e[,i] <- (ident - H)%*%epsilon[,i]
    u <- diag(ident - H)
    e[,i] <- e[,i]/sqrt(u)
    e[,i] <- sort(e[,i]) }
  #
  for(i in 1:n){
    eo <- sort(e[i,])
    e1[i] <- (eo[2]+eo[3])/2
    e2[i] <- (eo[97]+eo[98])/2 }
  #
  med <- apply(e,1,mean)
  faixa <- range(tsi,e1,e2)
  
  df <- data.frame(
    media = med,
    lim.inf = e1,
    lim.sup = e2,
    res = tsi,
    quant = qqnorm(med, plot.it = F)$x
    )
  
  return(df)
  
  #
  #par(pty="s")
  #qqnorm(tsi,xlab="Percentil da N(0,1)",
  #       ylab="Residuo Studentizado", ylim=faixa, pch=16, main="")
  #par(new=T)
  #qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
  #par(new=T)
  #qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
  #par(new=T)
  #qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=2, main="")  
}

#' Envelope do qqplot para a distribuição normal usando ggplot2
#'
#'
envel_norm_gg <- function(fit.model, dados = fit.model$model){
  df <- envel_norm_(fit.model, dados)
  p <- ggplot2::ggplot(df, aes(x = quant)) + 
    geom_ribbon(aes(ymin = lim.inf, ymax = lim.sup), alpha = 0.3) +
    geom_line(aes(y = media), linetype = "dashed") + 
    geom_point(aes(y = sort(res)))
  return(p)
}


#' Gráfico Quantil-Quanntil com envelope.
#'
#' @param modelo modelo normal ajustado utilizando o comando lm
#' @param dados dados utilizados para ajustar o modelo, se não for especificado
#' o programa utilizará modelo$model
#'
#' @examples
#' modelo <- lm(mpg ~ cyl + disp, data = mtcars)
#' envel_norm(modelo)
#' modelo %>% envel_norm()
#' 
#' @import magrittr
#'
#' @export
envel_norm <- function(modelo, dados = fit.model$model){
  envel_norm_gg(modelo, dados)
}

