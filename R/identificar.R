#' Identificar observação pelo índice
#'
#' @param p um gráfico de diagnóstico de resíduos
#' @param id índice da observação que deseja marcar no gráfico
#'
#' @examples
#' modelo <- lm(mpg ~ cyl + disp, data = mtcars)
#' envel_norm(modelo)
#' modelo %>% envel_norm() %>% idf_indice(5)
#' 
idf_indice <- function(p, id){
  p + geom_text(aes(y = res), label = id, hjust = 0, vjust = 0, data = p$data[id,])
}