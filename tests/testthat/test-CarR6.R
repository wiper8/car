test_that("odometre works", {
  expect_equal(
    {
      auto1 <- Car$new(1, 5, 74, 5)
      for (i in 1:10) {
        auto1$move(20, 0, 0.1)
      }
      sqrt((auto1$x-1)^2 + (auto1$y-5)^2)
    },
    auto1$odo,
    tolerance = 0.01
  )
})

test_that("acceleration works", {
  expect_equal(
    {
      auto1 <- Car$new(1, 5, 74, 5)
      for (i in 1:10) {
        auto1$move(20, 0, 0.1)
      }
      c(auto1$x, auto1$y, auto1$speed)
    },
    c(2.23115, 9.293549, 8.921374),
    tolerance = 0.01
  )
})

test_that("max speed works", {
  expect_equal(
    {
      auto1 <- Car$new(1, 5, 75, 5)
      for (i in 1:70) {
        auto1$move(20, 0, 1)
      }
      auto1$speed
    },

    Car$new(1, 5, 75, 5)$max_speed,
    tolerance = 0.01
  )
})
