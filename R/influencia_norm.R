#' Gráfico de influência (data.frame)
#'
#'
#'
#'
infl_norm_ <- function(modelo){
  
  X <- model.matrix(modelo)
  n <- nrow(X)
  p <- ncol(X)
  H <- X%*%solve(t(X)%*%X)%*%t(X)
  h <- diag(H)
  
  cut <- 2*p/n
  
  df <- data.frame(
    ind = 1:length(h),
    h = h,
    cut = cut
    )
  
  return(df)
}

#' Gráfico de influencia feito no ggplot2
#'
#'
#'
infl_norm_gg <- function(modelo){
  df <- infl_norm_(modelo)
  p <- ggplot(df, aes(x = ind, y = h)) + geom_point() + 
    geom_hline(yintercept = df$cut[1], linetype = "dashed") +
    ylim(0, NA) +
    xlab("Índice") +
    ylab("Medida h")
  return(p)
}


#' Gráfico de influência para o modelo normal
#'
#' @param modelo modelo ajustado
#' 
#' @examples
#' modelo <- lm(mpg ~ cyl + disp, data = mtcars)
#' modelo %>% infl_norm()
#' 
#' @export
infl_norm <- function(modelo){
  infl_norm_gg(modelo)
}