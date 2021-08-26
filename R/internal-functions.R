#' Sine in degrees
#'
#' @param x numeric vector.
#'
#' @return numeric vector.
#' @export
#'
#' @examples
#' sind(75)
sind <- function(x) {
  sin(x*pi/180)
}

#' Cosine in degrees
#'
#' @param x numeric vector.
#'
#' @return numeric vector.
#' @export
#'
#' @examples
#' cosd(75)
cosd <- function(x) {
  cos(x*pi/180)
}

#' Tangent in degrees
#'
#' @param x numeric vector.
#'
#' @return numeric vector.
#' @export
#'
#' @examples
#' tand(75)
tand <- function(x) {
  tan(x*pi/180)
}
