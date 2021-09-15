#' Euclidean-Algorithm
#'
#' Find the greatest common divisor of two numbers.
#'
#' Pseudocode from: https://en.wikipedia.org/wiki/Euclidean_algorithm
#'
#' @param a A number (int)
#' @param b A number (int)
#' @return The greatest common divisor of both numbers.
#' @examples
#' euclidean(100, 1000)
#' euclidean(123, 567890)
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


