#' Gráfico de influência (data.frame)
#'
#'
#'
#'
infl_gamma_ <- function(modelo){
  
  X <- model.matrix(modelo)
  n <- nrow(X)
  p <- ncol(X)
  H <- X%*%solve(t(X)%*%X)%*%t(X)
  h <- diag(H)
  
  cut <- 2*p/n
  
  df <- data.frame(
    ind = fitted.values(modelo),
    h = h
    )
  
  return(df)
}

#' Gráfico de influencia feito no ggplot2
#'
#'
#'
infl_gamma_gg <- function(modelo){
  df <- infl_gamma_(modelo)
  p <- ggplot(df, aes(x = ind, y = h)) + geom_point() + 
    ylim(0, NA) +
    xlab("Valor Ajustado") +
    ylab("Medida h")
  return(p)
}


#' Gráfico de influência para o modelo gamma
#'
#' @param modelo modelo ajustado
#' 
#' @examples
#' clotting <- data.frame(
#'    u = c(5,10,15,20,30,40,60,80,100),
#'    lot1 = c(118,58,42,35,27,25,21,19,18),
#'    lot2 = c(69,35,26,21,18,16,13,12,12))
#' modelo <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
#' modelo %>% infl_gamma()
#' @export
infl_gamma <- function(modelo){
  infl_gamma_gg(modelo)
}