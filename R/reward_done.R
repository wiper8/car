#' @include car.R
NULL

#' Get the observed reward accordingly to the state
#'
#' @param car car list.
#' @param walls LINESTRING (can be many connected lines).
#' @param finish LINESTRING.
#'
#' @return reward as a numeric value.
#' @export
#'
#' @examples reward(set(reset()),
#' walls=sf::st_linestring(matrix(c(40, 70, 10, 70, -3, 63, -10, 50, -10, -2, 10, -2, 10, 50, 40, 50), ncol=2, byrow=TRUE)),
#' finish=sf::st_linestring(matrix(c(40, 50, 40, 70), ncol=2, byrow=TRUE)))
reward <- function(car, walls, finish, t, ...) {
  track <- sf::st_polygon(list(rbind(walls[], finish[])))

  if(length(sf::st_intersects(car$outer, walls)[[1]])!=0L) {
    return(-100L)
  } else {
    if(length(sf::st_intersects(car$outer, track)[[1]])!=0L) {
      if(length(sf::st_intersects(car$outer, finish)[[1]]!=0L)) {
        return(10000L)
      } else {
        return(-1*t)
      }
    } else {
      return(-1*t)
    }
  }
}

#' Know if the car episode is done or not.
#'
#' @param reward numeric
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples done(-1)
done <- function(reward) {
  if(reward==10000L) print("**********FINISH**********")
  reward%in%c(-100L, 10000L)
}
