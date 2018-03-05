#' Wrapper function for ggplot2 for data d
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' myGGplot(d)
myGGplot<-function(x){
  library(ggplot2)
  library(magrittr)
  x %>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
}

#' dplyr Wrapper
#'
#' This is a wrapper for DPLYR
#'
#' @param x A data-frame
#'
#' @export
dplyrWrapper <- function(x) {
  xdf <- dplyr::data_frame(x)
  dplyr::count(xdf, x)
}

