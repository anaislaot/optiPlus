context("fonction createCv")

test_that("test createCv columName", {
  data(mtcars)

  test1 <- createCv(mtcars, columName = c("cyl"))
  test1 <- as.numeric(test1)
  test2 <- mtcars$cyl
  expect_true(identical(test1, test2))
})

test_that("test createCv kfolds", {
  data(mtcars)

  test1 <- createCv(mtcars,  kfolds = 4)
  expect_true(length(unique(test1)) == 4)
})



