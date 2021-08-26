test_that("physical limits of a car works", {
  expect_equal(limits(list(min_acc=-10, max_acc=12, max_steer=20, speed=0, inner_l=2.69, outer_w=2,
                           mu=0.7, g=9.8, mass_height=1, pSC_2=0),
                      list(acc=15, steer=30), 0.1),
               list(acc=12, steer=20))
  expect_equal(limits(list(min_acc=-10, max_acc=12, max_steer=20, speed=0, inner_l=2.69, outer_w=2,
                           mu=0.7, g=9.8, mass_height=1, pSC_2=0),
                      list(acc=-15, steer=-30), 0.1),
               list(acc=-10, steer=-20))
})

test_that("air drag limits of a car works", {
  expect_equal(limits(list(min_acc=-10, max_acc=12, max_steer=20, speed=3, inner_l=3, outer_w=2,
                           mu=0.7, g=9.8, mass_height=1, pSC_2=0.01),
                      list(acc=15, steer=0), 1),
               list(acc=11.91, steer=0))
})

test_that("slipping limit works", {
  expect_equal(limits(list(min_acc=-10, max_acc=12, max_steer=20, speed=7, inner_l=2.69, outer_w=1.6,
                           mu=0.7, g=9.8, mass_height=0.5, pSC_2=0),
                      list(acc=1, steer=20), 1),
               list(acc=1, steer=16.08),
               tolerance=0.01)
})


test_that("tonneau limit works", {
  expect_equal(limits(list(min_acc=-10, max_acc=12, max_steer=20, speed=7, inner_l=2.69, inner_w=1.6, outer_w=1.96,
                           mu=0.7, g=9.8, mass_height=2, pSC_2=0),
                      list(acc=1, steer=20), 1),
               list(acc=1, steer=11.41),
               tolerance=0.01)
})
