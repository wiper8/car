#' @include internal-functions.R
#' @import R6

#' @param walls LINESTRING (can be many connected lines).
#' @param finish LINESTRING.
NULL #for now until i get the time to set every param in documentation

#' @export
Car <- R6Class("Car", list(

  x = NULL,
  y = NULL,
  orien = NULL,
  range = NULL,
  walls = NULL,
  finish = NULL,
  t = NULL,
  speed = 0,
  acc = NULL,
  max_acc = 8.944,
  min_acc = -10.868,
  max_speed = 212 * 1.61 / 3.6,
  odo = 0,
  mu = 0.7,
  g = 9.80665,
  mass = 3560, #lbs
  pSC_2 = NULL,
  dimensions = list(
    max_turning_radius_outer = 11.48 / 2,
    cen_to_back = 1.004,
    cen_to_front = 3.657,
    inner_w = 1.595,
    inner_l = 2.69,
    outer_w = 1.955,
    outer_l = NULL,
    height = 1.241,
    mass_height = 0.558, #approximation
    max_steer = NULL
  ),

  wheels = NULL,
  outer = NULL,
  radars = NULL,
  radars_inside = NULL,
  radars_etat = NULL,
  dists = NULL,
  line = NULL,

  initialize = function(x = 0, y = 0, orien = 90, range = 50, t = 0.1) {
    self$x <- x
    self$y <- y
    self$orien <- orien
    self$range <- range
    self$t <- t
    self$actualize(TRUE)
    self$radars_etat = list()
    for (i in 1:length(self$radars)) self$radars_etat[[i]] <- TRUE
    self$intersect_radar()
  },

  actualize = function(initialize = FALSE) {
    self$pSC_2 <- self$max_acc / self$max_speed ^ 2
    self$dimensions$outer_l <- self$dimensions$cen_to_back + self$dimension$cen_to_front
    self$dimensions$max_steer <- atan(self$dimensions$inner_l / (self$dimensions$max_turning_radius_outer + self$dimensions$outer_w / 2)) * 180 / pi
    self$dimensions$x_front <- self$x + cosd(self$orien) * self$dimensions$inner_l
    self$dimensions$y_front <- self$y + sind(self$orien) * self$dimensions$inner_l
    self$place_wheels()
    self$place_outer()
    self$place_radars()
    self$place_radars_inside()
    if (initialize) self$line <- array(c(self$outer[[1]][1:5], self$speed, self$outer[[1]][6:10], self$acc), c(6, 2, 1))
    if (!initialize) {
      self$intersect_radar()
      self$radar_distance()
      self$line <- array(c(self$line, c(self$outer[[1]][1:5], self$speed, self$outer[[1]][6:10], self$acc)), c(6, 2, dim(self$line)[3] + 1))
    }
  },

  print = function(...) {
    cat("x : ", self$x, "\n")
    cat("y : ", self$y, "\n")
    cat("speed : ", self$speed, "\n")
    cat("orien : ", self$orien, "\n")
  },

  place_wheels = function() {
    self$wheels <- sf::st_polygon(list(matrix(c(self$x+cosd(self$orien+90)*self$dimensions$inner_w/2, self$y+sind(self$orien+90)*self$dimensions$inner_w/2,
                                 self$dimensions$x_front+cosd(self$orien+90)*self$dimensions$inner_w/2, self$dimensions$y_front+sind(self$orien+90)*self$dimensions$inner_w/2,
                                 self$dimensions$x_front+cosd(self$orien-90)*self$dimensions$inner_w/2, self$dimensions$y_front+sind(self$orien-90)*self$dimensions$inner_w/2,
                                 self$x+cosd(self$orien-90)*self$dimensions$inner_w/2, self$y+sind(self$orien-90)*self$dimensions$inner_w/2,
                                 self$x+cosd(self$orien+90)*self$dimensions$inner_w/2, self$y+sind(self$orien+90)*self$dimensions$inner_w/2
    ), ncol=2, byrow=TRUE)))
  },

  place_outer = function() {
    self$outer <- sf::st_polygon(list(matrix(c(self$x+cosd(self$orien+180)*self$dimensions$cen_to_back+cosd(self$orien+90)*self$dimensions$outer_w/2, self$y+sind(self$orien+180)*self$dimensions$cen_to_back+sind(self$orien+90)*self$dimensions$outer_w/2,
                                 self$x+cosd(self$orien)*self$dimensions$cen_to_front+cosd(self$orien+90)*self$dimensions$outer_w/2, self$y+sind(self$orien)*self$dimensions$cen_to_front+sind(self$orien+90)*self$dimensions$outer_w/2,
                                 self$x+cosd(self$orien)*self$dimensions$cen_to_front+cosd(self$orien-90)*self$dimensions$outer_w/2, self$y+sind(self$orien)*self$dimensions$cen_to_front+sind(self$orien-90)*self$dimensions$outer_w/2,
                                 self$x+cosd(self$orien+180)*self$dimensions$cen_to_back+cosd(self$orien-90)*self$dimensions$outer_w/2, self$y+sind(self$orien+180)*self$dimensions$cen_to_back+sind(self$orien-90)*self$dimensions$outer_w/2,
                                 self$x+cosd(self$orien+180)*self$dimensions$cen_to_back+cosd(self$orien+90)*self$dimensions$outer_w/2, self$y+sind(self$orien+180)*self$dimensions$cen_to_back+sind(self$orien+90)*self$dimensions$outer_w/2
    ), ncol=2, byrow=TRUE)))
  },

  place_radars = function() {
    self$radars <- sf::st_multilinestring(list(matrix(c(self$outer[[1]][1, ], self$outer[[1]][1, 1]+cosd(self$orien+90)*self$range, self$outer[[1]][1, 2]+sind(self$orien+90)*self$range), ncol=2, byrow=TRUE),
                                matrix(c(self$outer[[1]][2, ], self$outer[[1]][2, 1]+cosd(self$orien+45)*self$range, self$outer[[1]][2, 2]+sind(self$orien+45)*self$range), ncol=2, byrow=TRUE),
                                matrix(c(self$x+cosd(self$orien)*self$dimensions$cen_to_front, self$y+sind(self$orien)*self$dimensions$cen_to_front, self$x+cosd(self$orien)*self$dimensions$cen_to_front+cosd(self$orien)*self$range, self$y+sind(self$orien)*self$dimensions$cen_to_front+sind(self$orien)*self$range), ncol=2, byrow=TRUE),
                                matrix(c(self$outer[[1]][3, ], self$outer[[1]][3, 1]+cosd(self$orien-45)*self$range, self$outer[[1]][3, 2]+sind(self$orien-45)*self$range), ncol=2, byrow=TRUE),
                                matrix(c(self$outer[[1]][4, ], self$outer[[1]][4, 1]+cosd(self$orien-90)*self$range, self$outer[[1]][4, 2]+sind(self$orien-90)*self$range), ncol=2, byrow=TRUE),
                                matrix(c(self$x-cosd(self$orien)*self$dimensions$cen_to_back, self$y-sind(self$orien)*self$dimensions$cen_to_back, self$x-cosd(self$orien)*self$dimensions$cen_to_back-cosd(self$orien)*self$range, self$y-sind(self$orien)*self$dimensions$cen_to_back-sind(self$orien)*self$range), ncol=2, byrow=TRUE)
    ))
  },

  place_radars_inside = function() {
    range <- self$t * self$speed + self$max_acc / 2 * self$t ^ 2
    orien <- self$orien - 180
    self$radars_inside <- sf::st_multilinestring(list(matrix(c(self$outer[[1]][1, ], self$outer[[1]][1, 1]+cosd(orien+90)*range, self$outer[[1]][1, 2]+sind(orien+90)*range), ncol=2, byrow=TRUE),
                                matrix(c(self$outer[[1]][2, ], self$outer[[1]][2, 1]+cosd(orien+45)*range, self$outer[[1]][2, 2]+sind(orien+45)*range), ncol=2, byrow=TRUE),
                                matrix(c(self$x+cosd(self$orien)*self$dimensions$cen_to_front, self$y+sind(self$orien)*self$dimensions$cen_to_front, self$x+cosd(self$orien)*self$dimensions$cen_to_front+cosd(orien)*range, self$y+sind(self$orien)*self$dimensions$cen_to_front+sind(orien)*range), ncol=2, byrow=TRUE),
                                matrix(c(self$outer[[1]][3, ], self$outer[[1]][3, 1]+cosd(orien-45)*range, self$outer[[1]][3, 2]+sind(orien-45)*range), ncol=2, byrow=TRUE),
                                matrix(c(self$outer[[1]][4, ], self$outer[[1]][4, 1]+cosd(orien-90)*range, self$outer[[1]][4, 2]+sind(orien-90)*range), ncol=2, byrow=TRUE),
                                matrix(c(self$x-cosd(self$orien)*self$dimensions$cen_to_back, self$y-sind(self$orien)*self$dimensions$cen_to_back, self$x-cosd(self$orien)*self$dimensions$cen_to_back-cosd(orien)*range, self$y-sind(self$orien)*self$dimensions$cen_to_back-sind(orien)*range), ncol=2, byrow=TRUE)
    ))
  },

  intersect_radar = function() {
    if (!is.null(self$walls)) {

      for(i in (1:length(self$radars_inside))[unlist(self$radars_etat)]) {
        radar <- sf::st_linestring(self$radars_inside[[i]])

        inter <- sf::st_intersects(radar, self$walls)[[1]]
        self$radars_etat[[i]] <- length(inter) == 0L
      }


      for(i in (1:length(self$radars))[unlist(self$radars_etat)]) {
        radar <- sf::st_linestring(self$radars[[i]])

        inter <- sf::st_intersects(radar, self$walls)[[1]]
        if(length(inter) != 0L) {
          intersection <- sf::st_intersection(radar, self$walls)
          self$radars[[i]] <- sf::st_nearest_points(sf::st_point(radar[1, ]), intersection)[[1]]
        }
      }

      for(i in (1:length(self$radars))[!unlist(self$radars_etat)]) {
        self$radars[[i]] <- sf::st_linestring(rbind(self$radars[[i]][, 1], self$radars[[i]][, 1]))
      }
    }
    self$radar_distance()
  },

  radar_distance = function() {
    self$dists <- mapply(function(x, etat) ifelse(etat == FALSE, 0, sqrt((x[2, 1] - x[1, 1]) ^ 2 + (x[2, 2] - x[1, 2]) ^ 2)), self$radars, self$radars_etat)
    self$dists <- data.frame("l"=self$dists[[1]], "t_l"=self$dists[[2]], "t"=self$dists[[3]], "t_r"=self$dists[[4]], "r"=self$dists[[5]], "behind"=self$dists[[6]])
  },

  put_on_track = function(walls, finish) {
    if (!missing(walls)) self$walls <- walls
    if (!missing(finish)) self$finish <- finish
    self$intersect_radar()
  },

  see_car = function() {
    ggplot2::ggplot()+
      ggplot2::geom_sf(data=self$outer)+
      ggplot2::geom_sf(data=self$wheels)+
      ggplot2::geom_point(ggplot2::aes(x=self$x, y=self$y))+
      ggplot2::labs(x = "x", y = "y")+
      ggplot2::coord_sf()
  },

  see_radars = function() {
    ggplot2::ggplot()+
      ggplot2::geom_sf(data=self$outer)+
      ggplot2::geom_sf(data=self$wheels)+
      ggplot2::geom_sf(data=self$radars, col = "blue")+
      ggplot2::geom_point(ggplot2::aes(x=self$x, y=self$y))+
      ggplot2::labs(x = "x", y = "y")+
      ggplot2::coord_sf()
  },

  see_track = function() {
    if (is.null(self$walls)) {
    ggplot2::ggplot()+
      ggplot2::labs(x = "x", y = "y")+
      ggplot2::coord_sf()
    } else {
      ggplot2::ggplot()+
        ggplot2::geom_sf(data = self$walls)+
        ggplot2::geom_sf(data = self$finish, col = "red")+
        ggplot2::labs(x = "x", y = "y")+
        ggplot2::coord_sf()
    }
  },

  see_car_track = function() {
    suppressMessages(
      self$see_radars()+
        self$see_track()
    )
  },

  see_car_track_inside = function() {

    ggplot2::ggplot()+
      ggplot2::geom_sf(data=self$outer)+
      ggplot2::geom_sf(data=self$wheels)+
      ggplot2::geom_sf(data=self$radars_inside, col = "blue")+
      ggplot2::geom_point(ggplot2::aes(x=self$x, y=self$y))+
      ggplot2::labs(x = "x", y = "y")+
      ggplot2::geom_sf(data = self$walls)+
      ggplot2::geom_sf(data = self$finish, col = "red")+
      ggplot2::coord_sf()
  },

  see_inputs = function() {
    ggplot2::ggplot()+
      ggplot2::geom_raster(ggplot2::aes(x = 1:6, y = rep(0, 6), fill = unlist(self$dists)))+
      ggplot2::geom_text(ggplot2::aes(x = 1:6, y = rep(1, 6), label = c("l", "t-l", "t", "t-r", "r", "b")))+
      ggplot2::scale_fill_gradient(low = "red", high = "white")+
      ggplot2::labs(x = NULL, y = NULL)+
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     legend.position = "bottom")+
      ggplot2::coord_fixed()
  },

  see_line = function(col_speed_or_acc = "speed") {
    if (col_speed_or_acc == "speed") {
      suppressMessages(
        self$see_track()+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[1, 1, ],
            y = self$line[1, 2, ],
            col = self$line[6, 1, ]
          ))+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[2, 1, ],
            y = self$line[2, 2, ],
            col = self$line[6, 1, ]
          ))+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[3, 1, ],
            y = self$line[3, 2, ],
            col = self$line[6, 1, ]
          ))+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[4, 1, ],
            y = self$line[4, 2, ],
            col = self$line[6, 1, ]
          ))+
          ggplot2::scale_color_gradient(low = "green", high = "red")
      )

    } else {
      suppressMessages(
        self$see_track()+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[1, 1, ],
            y = self$line[1, 2, ],
            col = self$line[6, 2, ]
          ))+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[2, 1, ],
            y = self$line[2, 2, ],
            col = self$line[6, 2, ]
          ))+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[3, 1, ],
            y = self$line[3, 2, ],
            col = self$line[6, 2, ]
          ))+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[4, 1, ],
            y = self$line[4, 2, ],
            col = self$line[6, 2, ]
          ))+
          ggplot2::scale_color_gradient(low = "green", high = "red")
      )
    }
  },

  move = function(gaz, wanted_steer) {
    limited_actions <- self$physics_limit(gaz, wanted_steer, self$t)
    self$acc <- limited_actions[1]
    steer <- limited_actions[2]
    distance <- self$speed * self$t + self$acc / 2 * self$t ^ 2
    self$speed <- self$speed + self$acc * self$t
    self$odo <- self$odo + distance
    turning_radius <- self$dimensions$inner_l / tand(steer)
    if(abs(turning_radius) > 20000) {
      self$x <- self$x + cosd(self$orien) * distance
      self$y <- self$y + sind(self$orien) * distance
    } else {
      turning_cen_x <- self$x + cosd(self$orien + 90) * turning_radius
      turning_cen_y <- self$y+sind(self$orien+90)*turning_radius
      turning_cen_orien <- self$orien-90*ifelse(turning_radius<0, -1, 1)
      turning_cen_new_orien <- turning_cen_orien+distance/turning_radius*180/pi
      self$x <- turning_cen_x + cosd(turning_cen_new_orien) * abs(turning_radius)
      self$y <- turning_cen_y + sind(turning_cen_new_orien) * abs(turning_radius)
      self$orien <- turning_cen_new_orien + 90 * ifelse(turning_radius < 0, -1, 1)
    }
    self$actualize()
    invisible(self)
  },

  physics_limit = function(acc, steer, t) {
    #limites physiques de l'auto
    acc <- min(max(acc, self$min_acc), self$max_acc)
    steer <- min(max(steer, -self$dimensions$max_steer), self$dimensions$max_steer)
    turning_radius <- self$dimensions$inner_l / tand(steer)

    #air drag
    acc <- acc - self$pSC_2 * self$speed ^ 2

    new_speed <- self$speed * t + acc * t

    #si le char va trop vite, c'est le steering que je limite.
    if(new_speed > min(
      sqrt(self$mu * self$g * abs(turning_radius)),
      sqrt(self$g * self$dimensions$outer_w * abs(turning_radius) / 2 / self$dimensions$mass_height))
    ) {
      if(which.min(c(
        sqrt(self$mu * self$g * abs(turning_radius)),
        sqrt(self$g * self$dimensions$outer_w * abs(turning_radius) / 2 / self$dimensions$mass_height)
      )) == 1) {
        print("almost slipping")
        steer <- sign(steer) * atan(self$mu * self$g * self$dimensions$inner_l / new_speed ^ 2) * 180 / pi
      } else {
        print("presque tonneau")
        steer <- sign(steer) * atan(self$g * self$dimensions$inner_l * self$dimensions$outer_w / new_speed ^ 2 / self$dimensions$mass_height / 2) * 180 / pi
      }
    }

    c(acc, steer)
  }

))
#auto1 <- Car$new(1, 5, 74, 15)
#auto1$put_on_track(walls1, finish1)
#auto1$see_car_track()
#auto1$see_inputs()

#auto1$radars
#auto1$radars_etat
#auto1$dists
#auto1$see_car_track()
#auto1$radars_inside
#auto1$see_car_track_inside()

#auto1 <- Car$new(1, 5, 74, 15, 0.2)
#replicate(21, auto1$move(15, -6))
#auto1$see_line("speed")


