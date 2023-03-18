test_that("Returns Minimum value from a vector", {
  expect_equal(vec_min(c(1,2,3,4,5)), 1)
})

test_that("Returns Maximum value from a vector", {
  expect_equal(vec_max(c(2,4,6,8)), 8)
})

test_that("Sorts the vector in ascending order", {
  expect_equal(vec_sort(c(1,3,2,6,4,8,9)), c(1,2,3,4,6,8,9))
})


test_that("Reverses a vector", {
  expect_equal(vec_sort(c(5,4,3,2,1)), c(1,2,3,4,5))
})

test_that("Removes duplicate values", {
  expect_equal(vec_uniq(c(8,5,1,4,3,2,8,5,1)), c(8,5,1,4,3,2))
})


test_that("Replaces NA values with user defined values", {
  expect_equal(replace_na(c(3,6,2,6,4,NA,NA),3), c(3,6,2,6,4,3,3))
})

