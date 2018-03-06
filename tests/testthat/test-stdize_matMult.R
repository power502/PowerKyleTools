context("Quiz functions")

test_that("matMult computes a scalar of a vector and matrix", {
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
  inv <- solve(a)
  p <-(t(x) %*% inv %*% x)
  expect_identical(p,matMult(a, x))
         })

test_that("stdize will standarize a matrix", {
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
  count = 1
  z <- a
  rows <- nrow(z)
  cols <- ncol(z)
  while (count <= cols){
    singleColumn <- z[,count]
    columnMean <- mean(singleColumn)
    columnSd <- sd(singleColumn)
    z[,count] = ((singleColumn-columnMean)/columnSd)
    count = count + 1
  }
  new <- z
  
  expect_identical(new,stdize(a))
})

