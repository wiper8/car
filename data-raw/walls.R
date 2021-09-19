## code to prepare `walls` dataset goes here
walls1 <- sf::st_linestring(matrix(c(-2, 20, -2, -2, 2, -2, 2, 20), ncol=2, byrow=TRUE))
finish1 <- sf::st_linestring(matrix(c(2, 20, -2, 20), ncol=2, byrow=TRUE))
usethis::use_data(walls1, overwrite = TRUE)
usethis::use_data(finish1, overwrite = TRUE)

walls2 <- sf::st_linestring(matrix(c(-2, 50, -10, -2, 10, -2, 2, 50), ncol=2, byrow=TRUE))
finish2 <- sf::st_linestring(matrix(c(2, 50, -2, 50), ncol=2, byrow=TRUE))
usethis::use_data(walls2, overwrite = TRUE)
usethis::use_data(finish2, overwrite = TRUE)

walls3 <- sf::st_linestring(matrix(c(-10, 20, -10, -2, 10, -2, 10, 20), ncol=2, byrow=TRUE))
finish3 <- sf::st_linestring(matrix(c(10, 20, -10, 20), ncol=2, byrow=TRUE))
usethis::use_data(walls3, overwrite = TRUE)
usethis::use_data(finish3, overwrite = TRUE)

walls4 <- sf::st_linestring(matrix(c(40, 62, 2, 62, -1.2, 61.2, -2, 58, -2, -2, 2, -2, 2, 50, 10, 58, 40, 58), ncol=2, byrow=TRUE))
finish4 <- sf::st_linestring(matrix(c(40, 58, 40, 62), ncol=2, byrow=TRUE))
usethis::use_data(walls4, overwrite = TRUE)
usethis::use_data(finish4, overwrite = TRUE)

walls4r <- sf::st_linestring(matrix(c(-40, 62, -2, 62, 1.2, 61.2, 2, 58, 2, -2, -2, -2, -2, 50, -10, 58, -40, 58), ncol=2, byrow=TRUE))
finish4r <- sf::st_linestring(matrix(c(-40, 58, -40, 62), ncol=2, byrow=TRUE))
usethis::use_data(walls4r, overwrite = TRUE)
usethis::use_data(finish4r, overwrite = TRUE)

walls5 <- sf::st_linestring(matrix(c(40, 70, 10, 70, -3, 63, -10, 50, -10, -2, 10, -2, 10, 50, 40, 50), ncol=2, byrow=TRUE))
finish5 <- sf::st_linestring(matrix(c(40, 50, 40, 70), ncol=2, byrow=TRUE))
usethis::use_data(walls5, overwrite = TRUE)
usethis::use_data(finish5, overwrite = TRUE)

walls5r <- sf::st_linestring(matrix(c(-40, 70, -10, 70, 3, 63, 10, 50, 10, -2, -10, -2, -10, 50, -40, 50), ncol=2, byrow=TRUE))
finish5r <- sf::st_linestring(matrix(c(-40, 50, -40, 70), ncol=2, byrow=TRUE))
usethis::use_data(walls5r, overwrite = TRUE)
usethis::use_data(finish5r, overwrite = TRUE)

walls6 <- sf::st_linestring(matrix(c(0, 90, 40, 90, 42, 80, 40, 70, 10, 70,
                                     -3, 63, -10, 50, -10, -2, 10, -2, 10, 50,
                                     40, 50, 53, 58, 60, 70, 60, 90, 53, 103,
                                     40, 110, 0, 110), ncol=2, byrow=TRUE))
finish6 <- sf::st_linestring(matrix(c(0, 110, 0, 90), ncol=2, byrow=TRUE))
usethis::use_data(walls6, overwrite = TRUE)
usethis::use_data(finish6, overwrite = TRUE)

walls6r <- sf::st_linestring(matrix(c(0, 90, -40, 90, -42, 80, -40, 70, -10, 70,
                                     3, 63, 10, 50, 10, -2, -10, -2, -10, 50,
                                     -40, 50, -53, 58, -60, 70, -60, 90, -53, 103,
                                     -40, 110, 0, 110), ncol=2, byrow=TRUE))
finish6r <- sf::st_linestring(matrix(c(0, 110, 0, 90), ncol=2, byrow=TRUE))
usethis::use_data(walls6r, overwrite = TRUE)
usethis::use_data(finish6r, overwrite = TRUE)

walls7 <- sf::st_linestring(matrix(c(-10, -60, -12, -135, -15, -135, -15, -110, -25, -90, -25, 16,
                                     -40, 28, -10, 10, -10, -2, 10, -2, 9, 10,
                                     8, 11, 7, 12, 6, 13, 5, 14, 4, 15, 3, 16,
                                     2, 17, 1, 18, -40, 40, -50, 39, -53, 35,
                                     -52, 29, -50, 20, -40, 15, -40, -90,
                                     -30, -110, -30, -150, -27, -160, -20, -170,
                                     -13, -170, -5, -160, 0, -150, 0, -60), ncol=2, byrow=TRUE))
finish7 <- sf::st_linestring(matrix(c(-10, -60, 0, -60), ncol=2, byrow=TRUE))
usethis::use_data(walls7, overwrite = TRUE)
usethis::use_data(finish7, overwrite = TRUE)

walls7r <- sf::st_linestring(matrix(c(10, -25, 12, -135, 15, -135, 15, -110, 25, -90, 25, 16,
                                     40, 28, 10, 10, 10, -2, -10, -2, -9, 10,
                                     -8, 11, -7, 12, -6, 13, -5, 14, -4, 15, -3, 16,
                                     -2, 17, -1, 18, 40, 40, 50, 39, 53, 35,
                                     52, 29, 50, 20, 40, 15, 40, -90,
                                     30, -110, 30, -150, 27, -160, 20, -170,
                                     13, -170, 5, -160, 0, -150, 0, -25), ncol=2, byrow=TRUE))
finish7r <- sf::st_linestring(matrix(c(10, -25, 0, -25), ncol=2, byrow=TRUE))
usethis::use_data(walls7r, overwrite = TRUE)
usethis::use_data(finish7r, overwrite = TRUE)

walls8 <- sf::st_linestring(matrix(c(-70, 10, -40, 10, -20, 30, -20, 90, 40, 90,
                                     42, 80, 40, 70, 10, 70,
                                     -3, 63, -10, 50, -10, -2, 10, -2, 10, 50,
                                     40, 50, 53, 58, 60, 70, 60, 90, 53, 103,
                                     40, 110, -40, 110, -40, 40, -48, 30,
                                     -70, 30), ncol=2, byrow=TRUE))
finish8 <- sf::st_linestring(matrix(c(-70, 10, -70, 30), ncol=2, byrow=TRUE))
usethis::use_data(walls8, overwrite = TRUE)
usethis::use_data(finish8, overwrite = TRUE)

walls8r <- sf::st_linestring(matrix(c(70, 10, 40, 10, 20, 30, 20, 90, -40, 90,
                                     -42, 80, -40, 70, -10, 70,
                                     3, 63, 10, 50, 10, -2, -10, -2, -10, 50,
                                     -40, 50, -53, 58, -60, 70, -60, 90, -53, 103,
                                     -40, 110, 40, 110, 40, 40, 48, 30,
                                     70, 30), ncol=2, byrow=TRUE))
finish8r <- sf::st_linestring(matrix(c(70, 10, 70, 30), ncol=2, byrow=TRUE))
usethis::use_data(walls8r, overwrite = TRUE)
usethis::use_data(finish8r, overwrite = TRUE)

walls9 <- sf::st_linestring(matrix(c(
  -20, 35, -58, 35, -60, 30, -60, 10, -58, 3, -30, 3, -20, 12, -8, 12,
  -8, -2, 8, -2, 8, 15, 0, 20, -20, 20, -30, 10, -50, 10, -50, 30, -20, 30


), ncol=2, byrow=TRUE))
finish9 <- sf::st_linestring(matrix(c(-20, 35, -20, 30), ncol=2, byrow=TRUE))
usethis::use_data(walls9, overwrite = TRUE)
usethis::use_data(finish9, overwrite = TRUE)

walls9r <- sf::st_linestring(matrix(c(
  20, 35, 58, 35, 60, 30, 60, 10, 58, 3, 30, 3, 20, 12, 8, 12,
  8, -2, -8, -2, -8, 15, 0, 20, 20, 20, 30, 10, 50, 10, 50, 30, 20, 30


), ncol=2, byrow=TRUE))
finish9r <- sf::st_linestring(matrix(c(20, 35, 20, 30), ncol=2, byrow=TRUE))
usethis::use_data(walls9r, overwrite = TRUE)
usethis::use_data(finish9r, overwrite = TRUE)

walls10 <- sf::st_linestring(matrix(c(
  32, 0, 32, 50, 19, 60, 3, 60, -10, 50, -10, -2, 10, -2, 10, 40, 12, 40, 12, 0
), ncol=2, byrow=TRUE))
finish10 <- sf::st_linestring(matrix(c(12, 0, 32, 0), ncol=2, byrow=TRUE))
usethis::use_data(walls10, overwrite = TRUE)
usethis::use_data(finish10, overwrite = TRUE)

walls10r <- sf::st_linestring(matrix(c(
  -32, 0, -32, 50, -19, 60, -3, 60, 10, 50, 10, -2, -10, -2, -10, 40, -12, 40, -12, 0


), ncol=2, byrow=TRUE))
finish10r <- sf::st_linestring(matrix(c(-12, 0, -32, 0), ncol=2, byrow=TRUE))
usethis::use_data(walls10r, overwrite = TRUE)
usethis::use_data(finish10r, overwrite = TRUE)

# auto=Car$new()
# auto$put_on_track(walls10r, finish10r)
# auto$see_car_track()
