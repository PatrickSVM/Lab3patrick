#' Find the greatest common divisor of two numbers.
#'
#' Pseudocode from: https://en.wikipedia.org/wiki/Euclidean_algorithm. 
#'
#' @param a A number
#' @param b A number
#' @return The greatest common divisor
#' @examples
#' euclidean(100, 1000)
#' @export

euclidean <- function(a, b) {
  if (!is.numeric(a) | !is.numeric(b)) {
    stop()
  }
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}


