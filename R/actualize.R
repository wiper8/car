#' @include internal-functions.R
#' @include car.R
NULL

#' Move the car
#'
#' @param car car list
#' @param actions actions list
#' @param t numeric : time epsilon
#'
#' @return car object updated.
#' @export
#'
#' @examples actualize(set(reset()), list(acc=2, steer=-20), 0.1, walls5)
actualize <- function(car, actions, t, walls, ...) {
  car$distance <- car$speed*t+actions$acc/2*t^2
  car$odo <- car$odo+car$distance
  car$turning_radius <- car$inner_l/tand(actions$steer)
  if(abs(car$turning_radius)>20000) {
    car$x <- car$x+cosd(car$orien)*car$distance
    car$y <- car$y+sind(car$orien)*car$distance
    } else {
    car$turning_cen_x <- car$x+cosd(car$orien+90)*car$turning_radius
    car$turning_cen_y <- car$y+sind(car$orien+90)*car$turning_radius
    car$turning_cen_orien <- car$orien-90*ifelse(car$turning_radius<0, -1, 1)
    car$turning_cen_new_orien <- car$turning_cen_orien+car$distance/car$turning_radius*180/pi
    car$x <- car$turning_cen_x+cosd(car$turning_cen_new_orien)*abs(car$turning_radius)
    car$y <- car$turning_cen_y+sind(car$turning_cen_new_orien)*abs(car$turning_radius)
    car$orien <- car$turning_cen_new_orien+90*ifelse(car$turning_radius<0, -1, 1)
    }

  car$speed <- car$speed+actions$acc*t

  car$x_front <- car$x+cosd(car$orien)*car$inner_l
  car$y_front <- car$y+sind(car$orien)*car$inner_l

  car <- wheels(car)
  car <- radar(car)
  car <- radar_distance(car, walls)

  car
}


#' Limit acceleration and steering according to physics
#'
#' @param car car list.
#' @param actions list of acc and steer.
#' @param t numeric time interval.
#'
#' @return data.frame of acc and steer.
#' @export
#'
#' @examples
#' limits(set(reset()), list(acc=30, steer=22), 1)
limits <- function(car, actions, t) {
  #limites physiques de l'auto
  actions$acc <- min(max(actions$acc, car$min_acc), car$max_acc)
  actions$steer <- min(max(actions$steer, -car$max_steer), car$max_steer)
  car$turning_radius <- car$inner_l/tand(actions$steer)

  #air drag
  actions$acc <- actions$acc-car$pSC_2*car$speed^2

  new_speed <- car$speed*t+actions$acc*t

  #si le char va trop vite, c'est le steering que je limite.
  if(new_speed>min(sqrt(car$mu*car$g*abs(car$turning_radius)), sqrt(car$g*car$outer_w*abs(car$turning_radius)/2/car$mass_height))) {
    if(which.min(c(sqrt(car$mu*car$g*abs(car$turning_radius)), sqrt(car$g*car$outer_w*abs(car$turning_radius)/2/car$mass_height)))==1) {
      print("almost slipping")
      actions$steer <- atan(car$mu*car$g*car$inner_l/new_speed^2)*180/pi
    } else {
      print("presque tonneau")
      actions$steer <- atan(car$g*car$inner_l*car$outer_w/new_speed^2/car$mass_height/2)*180/pi
    }
  }

  as.data.frame(actions)

}

#' Limit function fit to AI package
#'
#' @param actions_matrix matrix of acc and steer.
#' @param car car list.
#' @param t numeric time interval.
#' @param ...
#'
#' @return list of acc and steer.
#' @export
#'
#' @examples
#' limits_FUN(matrix(c(14, 23), ncol=2), set(reset()), 1)
limits_FUN <- function(actions_matrix, car, t, ...) {
  limits(car, list(acc=actions_matrix[1], steer=actions_matrix[2]), t)
}
