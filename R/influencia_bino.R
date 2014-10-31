#' Gráfico de influência (data.frame)
#'
#'
#'
#'
infl_bino_ <- function(modelo){
  
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
infl_bino_gg <- function(modelo){
  df <- infl_bino_(modelo)
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
#' cuse <- read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", 
#'     header=TRUE)
#' modelo <- glm(cbind(using, notUsing) ~age + education + wantsMore , 
#'                  family = binomial, data = cuse)
#' modelo %>% infl_bino()
#' 
#' @export
infl_bino <- function(modelo){
  infl_bino_gg(modelo)
}