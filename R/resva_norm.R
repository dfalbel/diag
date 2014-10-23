#' Data frame para gerar o gráfico de valores ajustados pelos resíduos
#'
#'
#'
#'
#'
resva_norm_ <- function(modelo){
  
  X <- model.matrix(modelo)
  n <- nrow(X)
  p <- ncol(X)
  H <- X%*%solve(t(X)%*%X)%*%t(X)
  h <- diag(H)
  lms <- summary(modelo)
  s <- lms$sigma
  r <- resid(lms)
  si <- lm.influence(modelo)$sigma
  tsi <- r/(si*sqrt(1-h))  
  
  df <- data.frame(
    ordem = 1:length(tsi),
    res = tsi,
    lim.inf = -2,
    lim.sup = 2,
    va = fitted.values(modelo)
  )
  return(df)
  
}

#' Gráfico dos valores ajustados pelo resíduo feito em ggplot2
#'
#'
#'
#'
resva_norm_gg <- function(modelo){
  
  df <- resva_norm_(modelo)
  
  p <- ggplot(df, aes(x = va, y = res)) + geom_point() +
    geom_hline(aes(yintercept = lim.inf), linetype = "dashed") + 
    geom_hline(aes(yintercept = lim.sup), linetype = "dashed")
  
  return(p)

}

#' Gráfico do valor ajustado pelo resíduo padronizado
#'
#' @param modelo modelo ajustado
#'
#' @examples
#' modelo <- lm(mpg ~ cyl + disp, data = mtcars)
#' modelo %>% resva_norm()
#'
#'
#' @export
resva_norm <- function(modelo){
  resva_norm_gg(modelo)
}






