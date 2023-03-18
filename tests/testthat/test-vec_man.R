test_that("Subsets vectors based on logical conditions", {
  expect_equal(vec_subset(c(1,2,3,4,5), c(1,2,3,4,5) %% 2 == 0), c(2,4))
})

test_that("Splits vector into 2 new lists with even and odd values separated", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  expected_output <- list(even = c(2, 4, 6, 8, 10), odd = c(1, 3, 5, 7, 9))
  expect_equal(vec_split(x), expected_output)

})

test_that("Combines multiple vectors in one vector", {
  expect_equal(vec_concat(c(1,2,3,4,5), c(6,7,8,9,10)), c(1,2,3,4,5,6,7,8,9,10))
})
