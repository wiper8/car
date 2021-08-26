test_that("radars are ok", {
  expect_equal(car:::radar(set(reset()))$radar[[2]],
               matrix(c(-0.9775, 3.657, -71.688, 74.36768), ncol=2, byrow=TRUE),
               tolerance=0.01)
})

test_that("radar stops on the first wall", {
  expect_equal(intersect_radar(radar=sf::st_linestring(matrix(c(0, 0, 0, 10), ncol=2, byrow=TRUE)), wall=sf::st_linestring(matrix(c(1, 0, -1, 2, 1, 5), ncol=2, byrow=TRUE)))[],
               matrix(c(0, 0, 0, 1), ncol=2, byrow=TRUE))
  expect_equal(intersect_radar(radar=sf::st_linestring(matrix(c(0, 10, 0, 0), ncol=2, byrow=TRUE)), wall=sf::st_linestring(matrix(c(1, 0, -1, 2, 1, 5), ncol=2, byrow=TRUE)))[],
               matrix(c(0, 10, 0, 3.5), ncol=2, byrow=TRUE))
})


test_that("radar distance is ok", {
  expect_equal(car:::radar_distance(set(reset()), walls)$dist,
               data.frame("l"=9.0225, "t_l"=12.759, "t"=61.484, "t_r"=12.797, "r"=10.9775),
               tolerance = 0.01)
})
