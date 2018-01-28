context("fonction rfMod")

test_that("test rfMod RMSE", {
  data(mtcars)
  cv <- sample(1:8, nrow(mtcars), replace = TRUE)
  #Data
  y <- "mpg"
  ycolumnindex <- names(mtcars) == "mpg"
  x <- mtcars[, !ycolumnindex]
  y <- mtcars[, ycolumnindex]

  test1 <- rfMod(x = x, y = y, cvcol= cv,
        ntree= c(1, 100),  criterion = "RMSE")
  expect_true(test1$param$ntree == 100)
})


test_that("test rfMod MAPE", {
  data(mtcars)
  cv <- sample(1:8, nrow(mtcars), replace = TRUE)
  #Data
  y <- "mpg"
  ycolumnindex <- names(mtcars) == "mpg"
  x <- mtcars[, !ycolumnindex]
  y <- mtcars[, ycolumnindex]

  test1 <- rfMod(x = x, y = y, cvcol= cv,
                 ntree= c(1, 100),  criterion = "MAPE")
  expect_true(test1$param$ntree == 100)
})

test_that("test rfMod R2", {
  data(mtcars)
  cv <- sample(1:8, nrow(mtcars), replace = TRUE)
  #Data
  y <- "mpg"
  ycolumnindex <- names(mtcars) == "mpg"
  x <- mtcars[, !ycolumnindex]
  y <- mtcars[, ycolumnindex]

  test1 <- rfMod(x = x, y = y, cvcol= cv,
                 ntree= c(1, 100),  criterion = "R2")
  expect_true(test1$param$ntree == 100)
})

test_that("test rfMod AUC", {
  data(iris)
  data <- iris[ which(iris$Species!= "setosa"), ]
  cv <- sample(1:8, nrow(data), replace = TRUE)
  #Data
  y <- "Species"
  ycolumnindex <- names(data) == "Species"
  x <- data[, !ycolumnindex]
  y <- data[, ycolumnindex]
  y <- as.factor(as.character(y))

  test1 <- rfMod(x = x, y = y, cvcol= cv,
                 ntree= c(1, 100),  criterion = "AUC")
  expect_true(test1$param$ntree == 100)
})


test_that("test rfMod CONF", {
  data(iris)
  cv <- sample(1:8, nrow(iris), replace = TRUE)
  #Data
  y <- "Species"
  ycolumnindex <- names(iris) == "Species"
  x <- iris[, !ycolumnindex]
  y <- iris[, ycolumnindex]

  test1 <- rfMod(x = x, y = y, cvcol= cv,
                 ntree= c(1, 100),  criterion = "CONF")
  expect_true(test1$param$ntree == 100)
})

test_that("test rfMod sortie", {
  data(mtcars)
  cv <- sample(1:8, nrow(mtcars), replace = TRUE)
  #Data
  y <- "mpg"
  ycolumnindex <- names(mtcars) == "mpg"
  x <- mtcars[, !ycolumnindex]
  y <- mtcars[, ycolumnindex]

  test1 <- rfMod(x = x, y = y, cvcol= cv,
                 ntree= 1,  criterion = "RMSE")

  expect_true("y"%in%names(test1))
  expect_true("yp"%in%names(test1))
  expect_true("cvcol"%in%names(test1))
  expect_true("param"%in%names(test1))
  expect_true("RMSE"%in%names(test1))
  expect_true("R2"%in%names(test1))
  expect_true("ntree"%in%names(test1$param))
  expect_true("mtry"%in%names(test1$param))
  expect_true("maxnodes"%in%names(test1$param))
  expect_true("nodesize"%in%names(test1$param))
})
