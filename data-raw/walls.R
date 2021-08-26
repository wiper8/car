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

walls4 <- sf::st_linestring(matrix(c(-10, 20, -10, -2, 10, -2, 10, 20), ncol=2, byrow=TRUE))
finish4 <- sf::st_linestring(matrix(c(10, 20, -10, 20), ncol=2, byrow=TRUE))
usethis::use_data(walls4, overwrite = TRUE)
usethis::use_data(finish4, overwrite = TRUE)

walls5 <- sf::st_linestring(matrix(c(40, 62, 2, 62, -1.2, 61.2, -2, 58, -2, -2, 2, -2, 2, 50, 10, 58, 40, 58), ncol=2, byrow=TRUE))
finish5 <- sf::st_linestring(matrix(c(40, 58, 40, 62), ncol=2, byrow=TRUE))
usethis::use_data(walls5, overwrite = TRUE)
usethis::use_data(finish5, overwrite = TRUE)

walls6 <- sf::st_linestring(matrix(c(40, 70, 10, 70, -3, 63, -10, 50, -10, -2, 10, -2, 10, 50, 40, 50), ncol=2, byrow=TRUE))
finish6 <- sf::st_linestring(matrix(c(40, 50, 40, 70), ncol=2, byrow=TRUE))
usethis::use_data(walls6, overwrite = TRUE)
usethis::use_data(finish6, overwrite = TRUE)
