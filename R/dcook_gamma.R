#' Data frame para a distância de cook
#'
#'
#'
#'
dcook_gamma_ <- function(modelo){
  X <- model.matrix(modelo)
  n <- nrow(X)
  p <- ncol(X)
  w <- modelo$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  fi <- MASS::gamma.shape(modelo)$alpha
  ts <- resid(modelo,type="pearson")*sqrt(fi/(1-h))
  td <- resid(modelo,type="deviance")*sqrt(fi/(1-h))
  di <- (h/(1-h))*(ts^2)
  
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
dcook_gamma_gg <- function(modelo){
  df <- dcook_gamma_(modelo)
  
  p <- ggplot(df, aes(x = ind, y = di)) + geom_point() + ylim(0,NA) +
    geom_hline(aes(yintercept = cut), linetype = "dashed") +
    xlab("Índice") +
    ylab("Distância de Cook")
  
  return(p)
}

#' Gráfico da distância de Cook para o modelo Gamma.
#'
#' @param modelo modelo ajustado
#' 
#' @details
#' A linha tracejada representa o corte proposto por Bollen, Kenneth et al de 
#' 4/n em que n é o número de observações.
#'
#' @examples
#' clotting <- data.frame(
#'    u = c(5,10,15,20,30,40,60,80,100),
#'    lot1 = c(118,58,42,35,27,25,21,19,18),
#'    lot2 = c(69,35,26,21,18,16,13,12,12))
#' modelo <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
#' modelo %>% dcook_gamma()
#' 
#' @export
dcook_gamma <- function(modelo){
  dcook_gamma_gg(modelo)
}