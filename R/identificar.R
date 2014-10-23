#' Identificar observação pelo índice
#'
#' @param p um gráfico de diagnóstico de resíduos
#' @param id índice da observação que deseja marcar no gráfico
#' @param size tamanho da letra do text, default = 5
#'
#' @examples
#' modelo <- lm(mpg ~ cyl + disp, data = mtcars)
#' modelo %>% envel_norm() %>% idf_indice(5)
#' modelo %>% envel_norm() %>% idf_indice(c(5,6))
#' 
#' @export
idf_indice <- function(p, id, size = 5){
  
  stopifnot(is.ggplot(p), is.numeric(id)) # checa os argumentos
  
  p + geom_text(aes(y = res), label = id, hjust = 0, 
                vjust = 0, size = size, data = p$data[id,])
}


#' Identificar todas observações que estão fora do intervalo
#' 
#' @param p um gráfico de diagnóstico
#' @param size tamanho da letra do texto, default = 5
#'
#' @examples
#' modelo <- lm(mpg ~ cyl + disp, data = mtcars)
#' modelo %>% envel_norm() %>% idf_todas()
#'
#' @export
idf_todas <- function(p, size = 5){
  
  stopifnot(is.ggplot(p)) # checa os argumentos
  
  df <- p$data
  df$ordem <- 1:nrow(df)
  df <- df[df$res>df$lim.sup | df$res < df$lim.inf,]
  
  p + geom_text(aes(y = res, label = ordem) , hjust = 0, 
                size = size, vjust = 0, data = df)
}