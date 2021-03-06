#' See the racing track
#'
#' @param walls LINESTRING (can be many connected lines).
#' @param finish LINESTRING.
#'
#' @return ggplot.
#' @export
#'
#' @examples
#' see_track(sf::st_linestring(matrix(c(40, 70, 10, 70, -3, 63, -10, 50, -10, -1, 10, -1, 10, 50, 40, 50), ncol=2, byrow=TRUE)),
#' finish)
see_track <- function(walls, finish) {

    if (!missing(walls) & !missing(finish)) {
      ggplot2::ggplot() +
        ggplot2::geom_sf(data=walls) +
        ggplot2::geom_sf(data=finish, col="red")
    } else if (!missing(walls) & missing(finish)) {
      ggplot2::ggplot() + ggplot2::geom_sf(data=walls)
    } else if (missing(walls) & !missing(finish)) {
      ggplot2::ggplot() + ggplot2::geom_sf(data=finish, col="red")
    }
}

#' See the car, the track and where the car has been
#'
#' @param car car list.
#' @param walls LINESTRING (can be many connected lines).
#' @param finish LINESTRING.
#' @param line list of every car in history.
#' @param ... (optional).
#'
#' @return ggplot.
#' @export
#'
#' @examples
#'
see_line <- function(car, walls, finish, line, ...) {
  p <- see_car_track(car, walls, finish)
  b_l <- matrix(unlist(lapply(line, function(x) x$outer[[1]][1, ])), ncol=2, byrow=TRUE)
  t_l <- matrix(unlist(lapply(line, function(x) x$outer[[1]][2, ])), ncol=2, byrow=TRUE)
  t_r <- matrix(unlist(lapply(line, function(x) x$outer[[1]][3, ])), ncol=2, byrow=TRUE)
  b_r <- matrix(unlist(lapply(line, function(x) x$outer[[1]][4, ])), ncol=2, byrow=TRUE)

  p <- p+ggplot2::geom_line(ggplot2::aes_string(x=b_l[, 1], y=b_l[, 2]), col="orange")
  p <- p+ggplot2::geom_line(ggplot2::aes_string(x=t_l[, 1], y=t_l[, 2]), col="orange")
  p <- p+ggplot2::geom_line(ggplot2::aes_string(x=t_r[, 1], y=t_r[, 2]), col="orange")
  p <- p+ggplot2::geom_line(ggplot2::aes_string(x=b_r[, 1], y=b_r[, 2]), col="orange")
  p
}
