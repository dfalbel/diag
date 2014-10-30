#' Gráfico de influência (data.frame)
#'
#'
#'
#'
infl_pois_ <- function(modelo){
  
  X <- model.matrix(modelo)
  n <- nrow(X)
  p <- ncol(X)
  w <- modelo$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
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
infl_pois_gg <- function(modelo){
  df <- infl_pois_(modelo)
  p <- ggplot(df, aes(x = ind, y = h)) + geom_point() + 
    ylim(0, NA) +
    xlab("Valor Ajustado") +
    ylab("Medida h")
  return(p)
}


#' Gráfico de influência para o modelo Poisson
#'
#' @param modelo modelo ajustado
#' 
#' @examples
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' modelo <- glm(counts ~ outcome + treatment, family = poisson())
#' modelo %>% infl_pois()
#' @export
infl_pois <- function(modelo){
  infl_pois_gg(modelo)
}