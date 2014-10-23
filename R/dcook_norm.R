#' Data frame para a distância de cook
#'
#'
#'
#'
dcook_norm_ <- function(modelo){
  X <- model.matrix(modelo)
  n <- nrow(X)
  p <- ncol(X)
  H <- X%*%solve(t(X)%*%X)%*%t(X)
  h <- diag(H)
  lms <- summary(modelo)
  s <- lms$sigma
  r <- resid(lms)
  ts <- r/(s*sqrt(1-h))
  di <- (1/p)*(h/(1-h))*(ts^2)
  
  df <- data.frame(
    ind = 1:length(di),
    di = di
    )
  return(df)
}

#' Gráfico da distância de cook em ggplot2
#'
#'
#'
dcook_norm_gg <- function(modelo){
  df <- dcook_norm_(modelo)
  
  p <- ggplot(df, aes(x = ind, y = di)) + geom_point() + xlim(0,NA)
  
  return(p)
}

#' Gráfico da distância de Cook para o modelo normal
#'
#' @param modelo modelo ajustado
#'
#' @examples
#' modelo <- lm(mpg ~ cyl + disp, data = mtcars)
#' modelo %>% dcook_norm()
#'
#'
dcook_norm <- function(modelo){
  dcook_norm_gg(modelo)
}