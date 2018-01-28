context("fonction .selectBestMod")

test_that("test select best mod with RMSE", {
  x <- c(1650, 2000, 1350, 1593, 2010)
  rmse1 <- which.min(x)

  rmse2 <- .selectBestMod(x, "RMSE")
  expect_true(rmse1 == rmse2)
})

test_that("test select best mod with R2", {
  x <- c(0.2, 0.5, 0.1, 0.8, 0.81)
  R2.1 <- which.max(x)

  R2.2 <- .selectBestMod(x, "R2")
  expect_true(R2.1 == R2.2)
})

test_that("test select best mod with AUC", {
  x <- c(0.2, 0.5, 0.1, 0.8, 0.81)
  AUC1 <- which.max(x)

  AUC2 <- .selectBestMod(x, "AUC")
  expect_true(AUC1 == AUC2)
})
