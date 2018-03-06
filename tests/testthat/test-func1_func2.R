context("Homework functions")

test_that("func1 computes mean, var, sd", {
         x <- 1:10
         var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
         x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
         expect_identical(func1(x), x_list)
         })

test_that("func2 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  expect_identical(func2(x), x_list)
  save<-try(func2(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: is.numeric(x) is not TRUE\n")
})
test_that("func3 computes the liklihood of a gamma distribution", {
  dis <- rnorm(10)
  alpha <- pi
  log <- function(alpha)
    sum(dgamma(dis, shape = alpha, log = TRUE))
  interval <- mean(dis) + c(-1,1) * 3 * sd(dis)
  interval <- pmax(mean(dis) / 1e3, interval)
  
  oout<- optimize(log, maximum = TRUE, interval)
  xxx <- (oout$maximum)
  expect_identical(xxx, func3(dis))
})

test_that("func4 computes the weighted mean, var, sd", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  sol <- list(mean=a,var=b,sd=c)
  
  expect_identical(sol,func4(d))
})

test_that("func7 Computes the liklihood of a given distribution for data x", {
  x1 = rgamma(100,3)
  func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
  result7_gamma <- func7(x1,func1,c(0,3))
  
  x = x1
  f7 <- function(theta, x){
    sum(func1(theta, x))
  }
  interval = c(0,3)
  oout <- optimize(f7, maximum = TRUE, interval, x=x)
  final <- oout$maximum
  
  expect_identical(final, result7_gamma)
})
