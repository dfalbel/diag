#' Números aleatórios de uma distribuição
#'
#'
#'
#'
rdist <- function(dist, n, par){
  if(dist == "gamma"){
    rgamma(n, par)
  }
}