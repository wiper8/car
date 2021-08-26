see_physics <- function() {
  car <- set(reset(x=0, y=0, orien=90))
  actions$steer <- -20
  car$turning_radius <- car$inner_l/tand(actions$steer)
  print("Si son steer est à -20, tan(20)=2.69/turning_radius")
  print("c'est comme une moto, mais dans la vraie l'orientation est 2 roues est différente.")
  ggplot2::ggplot()+
    ggplot2::geom_point(ggplot2::aes(x=c(car$x, car$x_front), y=c(car$y, car$y_front)), col=c("red", "black"))+
    ggplot2::geom_segment(ggplot2::aes(x=car$x, xend=car$x_front, y=car$y, yend=car$y_front))+
    ggplot2::geom_segment(ggplot2::aes(x=car$x-0.3, xend=car$x_front+0.3, y=car$y_front-0.8242, yend=car$y_front+0.8242))+
    ggplot2::geom_segment(ggplot2::aes(x=car$x, xend=car$x-car$turning_radius, y=car$y, yend=car$y), linetype = "dashed")+
    ggplot2::geom_segment(ggplot2::aes(x=car$x, xend=car$x-car$turning_radius, y=car$y_front, yend=car$y), linetype = "dashed")+
    ggplot2::geom_text(ggplot2::aes(x=c(-car$turning_radius/2, car$x), y=c(car$y, car$inner_l/2), label=c(round(-car$turning_radius, 2), car$inner_l)))+
    ggplot2::labs(x="x", y="y")+
    ggplot2::coord_fixed()
}

see_new_position <- function() {
  car <- set(reset(x=0, y=0, orien=10))
  actions <- list(acc=10, steer=20)
  car$turning_radius <- car$inner_l/tand(actions$steer)
  print("Si le char accélère à 10m/s pendant 1 sec, il fait v*1+10/2*1^2=5m")
  print("si son steer est 20")
  p1 <- ggplot2::ggplot()+
    ggplot2::geom_sf(data=car$outer)+
    ggplot2::geom_sf(data=car$wheels)+
    ggplot2::geom_point(ggplot2::aes(x=car$x, y=car$y))+
    ggplot2::labs(x = "x", y = "y")+
    ggplot2::coord_sf()+
    ggplot2::geom_point(ggplot2::aes(x=car$x, y=car$y), col="red")+
    ggplot2::geom_segment(ggplot2::aes(x=car$x_front-cosd(car$orien+actions$steer)*0.5, xend=car$x_front+cosd(car$orien+actions$steer)*0.5, y=car$y_front-sind(car$orien+actions$steer)*0.5, yend=car$y_front+sind(car$orien+actions$steer)*0.5))+
    ggplot2::geom_segment(ggplot2::aes(x=car$x, xend=car$x+cosd(car$orien+90)*car$turning_radius, y=car$y, yend=car$y+sind(car$orien+90)*car$turning_radius), linetype="dashed")+
    ggplot2::geom_segment(ggplot2::aes(x=car$x_front, xend=car$x+cosd(car$orien+90)*car$turning_radius, y=car$y_front, yend=car$y+sind(car$orien+90)*car$turning_radius), linetype="dashed")
  car2 <- actualize(car, actions=list(acc=10, steer=20), t=1, walls4)
  suppressMessages(p1+
    ggplot2::geom_sf(data=car2$outer)+
    ggplot2::geom_sf(data=car2$wheels)+
    ggplot2::geom_point(ggplot2::aes(x=car2$x, y=car2$y), col="red")+
    ggplot2::coord_sf())
}
