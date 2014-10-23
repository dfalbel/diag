#' Data frame para o gráfico do residuo padronizado pelo índice
#'
#'
#'
#'
#'
resind_norm_ <- function(modelo){
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
    lim.sup = 2
    )
  return(df)
}


#' Gráfico do resíduo padronizado pelo indice em ggplot2
#'
#'
#'
#'
resind_norm_gg <- function(modelo){
  
  df <- resind_norm_(modelo)
  
  p <- ggplot(df, aes(x = ordem, y = res)) + geom_point() + 
    geom_hline(aes(yintercept = lim.inf), linetype = "dashed") + 
    geom_hline(aes(yintercept = lim.sup), linetype = "dashed") +
    xlab("Índice") +
    ylab("Resíduo Studentizado")
  
  return(p)
}

#' Gráfico do índice pelo resíduo padronizado
#'
#' @param modelo modelo ajustado
#'
#' @examples
#' modelo <- lm(mpg ~ cyl + disp, data = mtcars)
#' modelo %>% resind_norm()
#'
#'
#' @export
resind_norm <- function(modelo){
  resind_norm_gg(modelo)
}




