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
    di = di,
    cut = 4/length(di)
    )
  return(df)
}

#' Gráfico da distância de cook em ggplot2
#'
#'
#'
dcook_norm_gg <- function(modelo){
  df <- dcook_norm_(modelo)
  
  p <- ggplot(df, aes(x = ind, y = di)) + geom_point() + ylim(0,NA) +
    geom_hline(aes(yintercept = cut), linetype = "dashed") +
    xlab("Índice") +
    ylab("Distância de Cook")
  
  return(p)
}

#' Gráfico da distância de Cook para o modelo normal
#'
#' @param modelo modelo ajustado
#' 
#' @details
#' A linha tracejada representa o corte proposto por Bollen, Kenneth et al de 
#' 4/n em que n é o número de observações.
#'
#' @examples
#' modelo <- lm(mpg ~ cyl + disp, data = mtcars)
#' modelo %>% dcook_norm()
#' 
#' @export
dcook_norm <- function(modelo){
  dcook_norm_gg(modelo)
}