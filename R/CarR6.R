#' @include internal-functions.R
#' @import R6

#' @param walls LINESTRING (can be many connected lines).
#' @param finish LINESTRING.
NULL

#' @title Car
#' @param x numeric : placement initial du centre du cylindre arrière de l'auto.
#' @param y numeric : placement initial du centre du cylindre arrière de l'auto.
#' @param orien numeric : orientation initiale de l'auto.
#' @param range numeric positive : longueur des radars.
#' @param t numeric positive : temps entre chaque actualisation.
#' @export
Car <- R6Class("Car", list(

  x = NULL,
  y = NULL,
  orien = NULL,
  range = NULL,
  walls = NULL,
  finish = NULL,
  t = NULL,
  speed = NULL,
  acc = NULL,
  limited_acc = NULL,
  physics_acc = NULL,
  steer = NULL,
  limited_steer = NULL,
  physics_steer = 0,
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
    outer_l = 1.004 + 3.657,
    height = 1.241,
    mass_height = 0.558, #approximation
    max_steer = NULL
  ),

  wheels = NULL,
  outer = NULL,
  radars_count = 23,
  radars = NULL,
  radars_inside = NULL,
  radars_etat = NULL,
  dists = NULL,
  # dist_to_finish = NULL,
  line = NULL,
  has_collapsed = FALSE,
  has_finished = FALSE,
  helper = NULL,

  initialize = function(x = 0, y = 0, orien = 90, range = 80, t = 0.1, helper = FALSE, speed = 0) {
    self$x <- x
    self$y <- y
    self$orien <- orien
    self$range <- range
    self$t <- t
    self$helper <- helper
    self$speed <- speed
    self$actualize(TRUE)
    self$radars_etat = list()
    for (i in 1:length(self$radars_inside)) self$radars_etat[[i]] <- TRUE
    self$intersect_radar()
  },

  actualize = function(initialize = FALSE) {
    self$pSC_2 <- self$max_acc / self$max_speed ^ 2
    self$dimensions$max_steer <- atan(self$dimensions$inner_l / (self$dimensions$max_turning_radius_outer + self$dimensions$outer_w / 2)) * 180 / pi
    self$dimensions$x_front <- self$x + cosd(self$orien) * self$dimensions$inner_l
    self$dimensions$y_front <- self$y + sind(self$orien) * self$dimensions$inner_l
    self$place_wheels()
    self$place_outer()
    self$place_radars()
    self$place_radars_inside()
    if (initialize) {
      self$line <- matrix(c(
        "speed" = self$speed,
        "orien" = self$orien,
        "acc" = NA,
        "limited_acc" = NA,
        "physics_acc" = NA,
        "steer" = NA,
        "limited_steer" = NA,
        "physics_steer" = NA,

        "blx" = self$outer[[1]][1, 1],
        "bly" = self$outer[[1]][1, 2],
        "tlx" = self$outer[[1]][2, 1],
        "tly" = self$outer[[1]][2, 2],
        "trx" = self$outer[[1]][3, 1],
        "try" = self$outer[[1]][3, 2],
        "brx" = self$outer[[1]][4, 1],
        "bry" = self$outer[[1]][4, 2],

        rep(NA, self$radars_count),
        self$has_finished,
        self$has_collapsed
      ), ncol = 16 + self$radars_count + 2,
      dimnames=list(NULL, c(
        "speed", "orien", "acc", "limited_acc", "physics_acc", "steer",
        "limited_steer", "physics_steer", "blx", "bly", "tlx", "tly", "trx",
        "try", "brx", "bry", mapply(function(i) paste0("V", i), 1:self$radars_count),
        "has_finished", "has_collapsed"
      )))
    }
    if (!initialize) {
      self$intersect_radar()
      self$radar_distance()
      self$line <- rbind(
        self$line,
        matrix(c(
          "speed" = self$speed,
          "orien" = self$orien,
          "acc" = self$acc,
          "limited_acc" = self$limited_acc,
          "physics_acc" = self$physics_acc,
          "steer" = self$steer,
          "limited_steer" = self$limited_steer,
          "physics_steer" = self$physics_steer,

          "blx" = self$outer[[1]][1, 1],
          "bly" = self$outer[[1]][1, 2],
          "tlx" = self$outer[[1]][2, 1],
          "tly" = self$outer[[1]][2, 2],
          "trx" = self$outer[[1]][3, 1],
          "try" = self$outer[[1]][3, 2],
          "brx" = self$outer[[1]][4, 1],
          "bry" = self$outer[[1]][4, 2],

          unlist(self$dists),
          self$has_finished,
          self$has_collapsed
        ), ncol = 16 + self$radars_count + 2,
        dimnames=list(NULL, c(
          "speed", "orien", "acc", "limited_acc", "physics_acc", "steer",
          "limited_steer", "physics_steer", "blx", "bly", "tlx", "tly", "trx",
          "try", "brx", "bry", mapply(function(i) paste0("V", i), 1:self$radars_count),
          "has_finished", "has_collapsed"
        )))
      )
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

    #angles <- (0:(self$radars_count-1)) * 270 / (self$radars_count-1) - 90 - 45
    angles <- c(135, 90, 75, 60, 45, 35, 25, 20, 15, 10, 5, 0, -5, -10, -15, -20, -25, -35, -45, -60, -75, -90, -135)
    radar_list <- list()
    for (i in 1:self$radars_count) {
      radar_list[[i]] <- matrix(c(self$x, self$y, self$x + cosd(self$orien + angles[i]) * self$range, self$y + sind(self$orien + angles[i]) * self$range), ncol=2, byrow=TRUE)
    }
    self$radars <- sf::st_multilinestring(radar_list)
  },

  place_radars_inside = function() {

    angles <- c(135, 90, 75, 60, 45, 35, 25, 20, 15, 10, 5, 0, -5, -10, -15, -20, -25, -35, -45, -60, -75, -90, -135)
    radar_inside_list <- list()
    range <- self$t * self$speed + self$max_acc / 2 * self$t ^ 2

    for (i in 1:self$radars_count) {
      radar_inside_list[[i]] <- matrix(c(self$x, self$y, self$x + cosd(self$orien + angles[i]) * range, self$y + sind(self$orien + angles[i]) * range), ncol=2, byrow=TRUE)
    }
    self$radars_inside <- sf::st_multilinestring(radar_inside_list)
  },

  intersect_radar = function() {
    if (!is.null(self$walls)) {

      for(i in (1:length(self$radars))[unlist(self$radars_etat)]) {
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

    if (!is.null(self$walls)) self$has_collapsed <- length(sf::st_intersects(self$walls, self$outer)[[1]]) != 0L
    if (!is.null(self$finish)) self$has_finished <- length(sf::st_intersects(self$outer, self$finish)[[1]]) != 0L
    self$radar_distance()
  },

  radar_distance = function() {
    self$dists <- unlist(mapply(function(x, etat) ifelse(etat == FALSE, 0, sqrt((x[2, 1] - x[1, 1]) ^ 2 + (x[2, 2] - x[1, 2]) ^ 2)), self$radars, self$radars_etat))

    # if (!is.null(self$finish)) {
    #
    #   radar_to_finish <- list()
    #   j <- 1
    #   for(i in (1:length(self$radars))[unlist(self$radars_etat)]) {
    #     radar <- sf::st_linestring(self$radars[[i]])
    #
    #     inter <- sf::st_intersects(radar, self$finish)[[1]]
    #     if(length(inter) != 0L) {
    #       intersection <- sf::st_intersection(radar, self$finish)
    #       radar_to_finish[[j]] <- sf::st_nearest_points(sf::st_point(radar[1, ]), intersection)[[1]]
    #       j <- j+1
    #     }
    #   }
    #
    #   self$dist_to_finish <- min(self$range, unlist(lapply(radar_to_finish, function(x) sqrt((x[2, 1] - x[1, 1]) ^ 2 + (x[2, 2] - x[1, 2]) ^ 2))))
    #
    # }
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
    p <- suppressMessages(
      self$see_radars()+
        ggplot2::geom_sf(data = self$walls)+
        ggplot2::geom_sf(data = self$finish, col = "red")+
        ggplot2::labs(x = "x", y = "y")
    )
    if (self$helper) {
      turning_radius <- self$dimensions$inner_l / tand(self$physics_steer)
      max_steer <- sign(self$physics_steer) * atan(self$mu * self$g * self$dimensions$inner_l / self$speed ^ 2) * 180 / pi
      max_turning_radius <- self$dimensions$inner_l / tand(max_steer)
      distance <- self$t * self$speed * 20
      new_x <- numeric(20)
      new_y <- numeric(20)
      new_orien <- numeric(20)
      new_x[1] <- self$x
      new_y[1] <- self$y
      new_orien[1] <- self$orien
      new2_x <- numeric(20)
      new2_y <- numeric(20)
      new2_orien <- numeric(20)
      new2_x[1] <- self$x
      new2_y[1] <- self$y
      new2_orien[1] <- self$orien
      for (i in 2:20) {
        if (self$physics_steer != 0) {
          turning_cen_x <- new2_x[i - 1] + cosd(new2_orien[i-1] + 90) * max_turning_radius
          turning_cen_y <- new2_y[i - 1] + sind(new2_orien[i-1] + 90) * max_turning_radius
          turning_cen_orien <- new2_orien[i - 1] - 90 * ifelse(max_turning_radius < 0, -1, 1)
          turning_cen_new_orien <- turning_cen_orien + distance / 20 / max_turning_radius * 180 / pi
          new2_x[i] <- turning_cen_x + cosd(turning_cen_new_orien) * abs(max_turning_radius)
          new2_y[i] <- turning_cen_y + sind(turning_cen_new_orien) * abs(max_turning_radius)
          new2_orien[i] <- new2_orien[i - 1] + turning_cen_new_orien-turning_cen_orien
        }
        if(abs(turning_radius) > 20000) {
          new_x[i] <- new_x[i-1] + cosd(self$orien) * distance / 20
          new_y[i] <- new_y[i-1] + sind(self$orien) * distance / 20
        } else {
          turning_cen_x <- new_x[i - 1] + cosd(new_orien[i-1] + 90) * turning_radius
          turning_cen_y <- new_y[i - 1] + sind(new_orien[i-1] + 90) * turning_radius
          turning_cen_orien <- new_orien[i - 1] - 90 * ifelse(turning_radius < 0, -1, 1)
          turning_cen_new_orien <- turning_cen_orien + distance / 20 / turning_radius * 180 / pi
          new_x[i] <- turning_cen_x + cosd(turning_cen_new_orien) * abs(turning_radius)
          new_y[i] <- turning_cen_y + sind(turning_cen_new_orien) * abs(turning_radius)
          new_orien[i] <- new_orien[i - 1] + turning_cen_new_orien-turning_cen_orien
        }
      }
      p <- p+
        ggplot2::geom_path(ggplot2::aes(x=new_x, y=new_y),
                           col = "green")
      if (self$physics_steer != 0) {
        p <- p + ggplot2::geom_path(ggplot2::aes(x=new2_x, y=new2_y),
                                    col = "red")
      }
    }
    #changer ca pur arrêter que les radars changent les axes a chaque iteration
    p <- p + ggplot2::coord_sf()
    p
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
      ggplot2::geom_raster(ggplot2::aes(x = 1:(length(self$dists)), y = rep(0, length(self$dists)), fill = self$dists))+
      ggplot2::geom_text(ggplot2::aes(x = 1:(length(self$dists)), y = rep(1, length(self$dists)), label = mapply(function(x) paste0("V", x), 1:self$radars_count)))+
      ggplot2::scale_fill_gradient(low = "red", high = "white")+
      ggplot2::labs(x = NULL, y = NULL)+
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     legend.position = "bottom")+
      ggplot2::coord_fixed()
  },

  see_line = function(col_speed_or_acc = "speed") {
    if (col_speed_or_acc == "speed") {
      n <- nrow(self$line)
      suppressMessages(
        self$see_track()+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[, "blx"],
            y = self$line[, "bly"],
            col = self$line[, "speed"]*3.6
          ))+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[, "tlx"],
            y = self$line[, "tly"],
            col = self$line[, "speed"]*3.6
          ))+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[, "trx"],
            y = self$line[, "try"],
            col = self$line[, "speed"]*3.6
          ))+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[, "brx"],
            y = self$line[, "bry"],
            col = self$line[, "speed"]*3.6
          ))+
          ggplot2::scale_color_gradient(low = "green", high = "red")
      )

    } else {
      suppressMessages(
        self$see_track()+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[, "blx"],
            y = self$line[, "bly"],
            col = self$line[, "acc"]
          ))+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[, "tlx"],
            y = self$line[, "tly"],
            col = self$line[, "acc"]
          ))+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[, "trx"],
            y = self$line[, "try"],
            col = self$line[, "acc"]
          ))+
          ggplot2::geom_path(ggplot2::aes(
            x = self$line[, "brx"],
            y = self$line[, "bry"],
            col = self$line[, "acc"]
          ))+
          ggplot2::scale_color_gradient(low = "green", high = "red")
      )
    }
  },

  move = function(acc, steer) {
    self$acc <- acc
    self$steer <- steer
    self$physics_limit(self$acc, self$steer)

    distance <- self$speed * self$t + self$physics_acc / 2 * self$t ^ 2
    self$speed <- self$speed + self$physics_acc * self$t
    self$odo <- self$odo + distance
    turning_radius <- self$dimensions$inner_l / tand(self$physics_steer)
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

  physics_limit = function(acc, steer) {
    #limites physiques de l'auto
    self$limited_acc <- min(max(acc, self$min_acc), self$max_acc)
    self$limited_steer <- min(max(steer, -self$dimensions$max_steer), self$dimensions$max_steer)
    turning_radius <- self$dimensions$inner_l / tand(self$limited_steer)

    #air drag
    self$physics_acc <- self$limited_acc - self$pSC_2 * self$speed ^ 2

    new_speed <- self$speed + self$physics_acc * self$t

    #si le char va trop vite, c'est le steering que je limite.
    if (new_speed > min(
      sqrt(self$mu * self$g * abs(turning_radius)),
      sqrt(self$g * self$dimensions$outer_w * abs(turning_radius) / 2 / self$dimensions$mass_height))
    ) {
      if(which.min(c(
        sqrt(self$mu * self$g * abs(turning_radius)),
        sqrt(self$g * self$dimensions$outer_w * abs(turning_radius) / 2 / self$dimensions$mass_height)
      )) == 1) {
        #print("almost slipping")
        self$physics_steer <- sign(self$limited_steer) * atan(self$mu * self$g * self$dimensions$inner_l / new_speed ^ 2) * 180 / pi
      } else {
        #print("presque tonneau")
        self$physics_steer <- sign(self$limited_steer) * atan(self$g * self$dimensions$inner_l * self$dimensions$outer_w / new_speed ^ 2 / self$dimensions$mass_height / 2) * 180 / pi
      }
    } else {
      self$physics_steer <- self$limited_steer
    }
  }

))
#auto1 <- Car$new(1, 5, 74, 15)
#auto1$put_on_track(walls1, finish1)
#auto1$see_car_track()
#auto1$see_inputs()
#auto1$see_track()
#auto1$radars
#auto1$radars_etat
#auto1$dists
#auto1$see_car_track()
#auto1$radars_inside
#auto1$see_car_track_inside()

#auto1 <- Car$new(1, 5, 74, 15, 0.2)
#replicate(21, auto1$move(15, -6))
#auto1$see_line("speed")


