#' @include internal-functions.R
#' @include track.R
NULL

#' Add wheels and outer layer to car
#'
#' @param car car list.
#'
#' @return car list.
#'
#' @examples
#' wheels(reset())
wheels <- function(car) {
  car$wheels <- sf::st_polygon(list(matrix(c(car$x+cosd(car$orien+90)*car$inner_w/2, car$y+sind(car$orien+90)*car$inner_w/2,
                                             car$x_front+cosd(car$orien+90)*car$inner_w/2, car$y_front+sind(car$orien+90)*car$inner_w/2,
                                             car$x_front+cosd(car$orien-90)*car$inner_w/2, car$y_front+sind(car$orien-90)*car$inner_w/2,
                                             car$x+cosd(car$orien-90)*car$inner_w/2, car$y+sind(car$orien-90)*car$inner_w/2,
                                             car$x+cosd(car$orien+90)*car$inner_w/2, car$y+sind(car$orien+90)*car$inner_w/2
                                             ), ncol=2, byrow=TRUE)))
  car$outer <- sf::st_polygon(list(matrix(c(car$x+cosd(car$orien+180)*car$cen_to_back+cosd(car$orien+90)*car$outer_w/2, car$y+sind(car$orien+180)*car$cen_to_back+sind(car$orien+90)*car$outer_w/2,
                                            car$x+cosd(car$orien)*car$cen_to_front+cosd(car$orien+90)*car$outer_w/2, car$y+sind(car$orien)*car$cen_to_front+sind(car$orien+90)*car$outer_w/2,
                                            car$x+cosd(car$orien)*car$cen_to_front+cosd(car$orien-90)*car$outer_w/2, car$y+sind(car$orien)*car$cen_to_front+sind(car$orien-90)*car$outer_w/2,
                                            car$x+cosd(car$orien+180)*car$cen_to_back+cosd(car$orien-90)*car$outer_w/2, car$y+sind(car$orien+180)*car$cen_to_back+sind(car$orien-90)*car$outer_w/2,
                                            car$x+cosd(car$orien+180)*car$cen_to_back+cosd(car$orien+90)*car$outer_w/2, car$y+sind(car$orien+180)*car$cen_to_back+sind(car$orien+90)*car$outer_w/2
                                            ), ncol=2, byrow=TRUE)))
  car
}

#' Add radars to car
#'
#' @param car car list.
#'
#' @return car list with radars.
#'
#' @examples
#' radar(set(reset()))
radar <- function(car) {
  dot <- car$outer[[1]]
  car$radar <- sf::st_multilinestring(list(matrix(c(car$x, car$y, car$x+cosd(car$orien+90)*car$range, car$y+sind(car$orien+90)*car$range), ncol=2, byrow=TRUE),
                                           matrix(c(car$x, car$y, car$x+car$x+cosd(car$orien+45)*car$range, car$y+sind(car$orien+45)*car$range), ncol=2, byrow=TRUE),
                                           matrix(c(car$x, car$y, car$x+cosd(car$orien)*car$range, car$y+sind(car$orien)*car$range), ncol=2, byrow=TRUE),
                                           matrix(c(car$x, car$y, car$x+car$x+cosd(car$orien-45)*car$range, car$y+sind(car$orien-45)*car$range), ncol=2, byrow=TRUE),
                                           matrix(c(car$x, car$y, car$x+car$x+cosd(car$orien-90)*car$range, car$y+sind(car$orien-90)*car$range), ncol=2, byrow=TRUE),
                                           matrix(c(car$x, car$y, car$x+car$x+cosd(car$orien+180)*car$range, car$y+sind(car$orien+180)*car$range), ncol=2, byrow=TRUE)

  ))
  car
}

#' Stop the radar line at the first wall
#'
#' @param radar LINESTRING (1 line only).
#' @param walls LINESTRING (can be many connected lines).
#'
#' @return
#'
#' @examples
#' intersect_radar(radar=sf::st_linestring(matrix(c(0, 10, 0, 0), ncol=2, byrow=TRUE)), walls=sf::st_linestring(matrix(c(1, 7, -1, 4, 1, 0), ncol=2, byrow=TRUE)))
#' intersect_radar(radar=sf::st_linestring(matrix(c(-2, 10, -1, 5.5), ncol=2, byrow=TRUE)), walls=sf::st_linestring(matrix(c(1, 7, -1, 4, 1, 0), ncol=2, byrow=TRUE)))
intersect_radar <- function(radar, walls) {
  inter <- sf::st_intersects(radar, walls)[[1]]
  if(length(inter)!=0L) {
    intersection <- sf::st_intersection(radar, walls)
    sf::st_nearest_points(sf::st_point(radar[1, ]), intersection)[[1]]
  } else {
    radar
  }
}

#' Add distances to walls detected from radars to car
#'
#' @param car car list.
#'
#' @return car list.
#'
#' @examples
#' radar_distance(set(reset()), walls)
radar_distance <- function(car, walls) {
  for(i in 1:length(car$radar)) {
    car$radar[[i]] <- car:::intersect_radar(sf::st_linestring(car$radar[[i]]), walls)
  }
  car$dist <- lapply(car$radar, function(x) sqrt((x[2, 1]-x[1, 1])^2+(x[2, 2]-x[1, 2])^2))
  car$dist <- data.frame("l"=car$dist[[1]], "t_l"=car$dist[[2]], "t"=car$dist[[3]], "t_r"=car$dist[[4]], "r"=car$dist[[5]], "behind"=car$dist[[6]])
  car
}

#' Reset car object
#'
#' @param x numeric.
#' @param y numeric.
#' @param orien numeric.
#' @param range numeric.
#'
#' @return car list with initial parameters (center position, spped and orientation).
#' @export
#'
#' @examples reset()
reset <- function(x=0, y=0, orien=90, range=100) {
  car <- list(x=x, y=y, speed=0, orien=orien, range=range)
  car
}

#' Set every element to car (wheels, outer layer, radars, radars' distance).
#'
#' @param car car list
#'
#' @return car list
#' @export
#'
#' @examples
#' set(reset())
set <- function(car) {
  car$max_acc <- 8.944
  car$min_acc <- -10.868
  car$max_speed <- 212*1.61/3.6
  car$max_turning_radius_outer <- 11.48/2
  car$odo <- 0
  car$mu <- 0.7
  car$g <- 9.80665
  car$mass <- 3560 #lbs
  car$cen_to_back <- 1.004
  car$cen_to_front <- 3.657
  car$inner_w <- 1.595
  car$inner_l <- 2.69
  car$outer_w <- 1.955
  car$outer_l <- car$cen_to_back+car$cen_to_front
  car$x_front <- car$x+cosd(car$orien)*car$inner_l
  car$y_front <- car$y+sind(car$orien)*car$inner_l
  car$height <- 1.241
  car$mass_height <- 0.558 ########approximation
  car$pSC_2 <- car$max_acc/car$max_speed^2
  car$max_steer <- atan(car$inner_l/(car$max_turning_radius_outer+car$outer_w/2))*180/pi
  car <- wheels(car)
  car <- radar(car)
  car
}

#' See dimensions of a car
#'
#' @param car car list
#'
#' @return ggplot.
#' @export
#'
#' @examples
#' see_car(set(reset()))
see_car <- function(car, ...) {
ggplot2::ggplot()+
    ggplot2::geom_sf(data=car$outer)+
    ggplot2::geom_sf(data=car$wheels)+
    ggplot2::geom_point(ggplot2::aes(x=car$x, y=car$y))+
    ggplot2::labs(x = "x", y = "y")+
    ggplot2::coord_sf()
}

#' See dimensions of radars on the car
#'
#' @param car car list
#'
#' @return ggplot.
#' @export
#'
#' @examples
#' see_radar(set(reset()))
see_radar <- function(car) {
  ggplot2::ggplot()+
    ggplot2::geom_sf(data=car$outer)+
    ggplot2::geom_sf(data=car$wheels)+
    ggplot2::geom_sf(data=car$radar)+
    ggplot2::geom_point(ggplot2::aes(x=car$x, y=car$y))+
    ggplot2::labs(x = "x", y = "y")+
    ggplot2::coord_sf()
}
