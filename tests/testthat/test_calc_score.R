

context("fonction .compute_criteria")

test_that("test rmse", {
  listPred <- list()
  listPred$y <- c(1,2,3,4,5)
  listPred$yp <- c(2,4,5,6,3)
  totest <- round(.compute_criteria(listPred, "RMSE") , 4) == 1.8439
  expect_true(totest)
})

test_that("test mape", {
  listPred <- list()
  listPred$y <- c(1,2,3,4,5)
  listPred$yp <- c(2,4,5,6,3)
  totest <- round(.compute_criteria(listPred, "MAPE") , 4) == 0.7133
  expect_true(totest)
})



test_that("test auc", {
  library(AUC)
  data("churn")
  listPred <- list()
  listPred$y <- churn$labels
  listPred$prob <- data.frame(a= 0, b=churn$predictions)


  AUC1 <- auc(roc(churn$predictions,churn$labels))

  AUC2 <- .compute_criteria(listPred, "AUC")
  expect_true(AUC1 == AUC2)

})


