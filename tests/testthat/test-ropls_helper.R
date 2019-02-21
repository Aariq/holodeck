context("test functions for extracting data from ropls objects")
library(ropls)
test_that("get_scores fails if not ropls model", {
  expect_error(
    prcomp(mtcars) %>% get_scores()
  )
})

pca <- opls(mtcars, plotL = FALSE, printL = FALSE)
pls <- opls(dplyr::select(mtcars, -mpg), mtcars$mpg,
            plotL = FALSE, printL = FALSE, predI = 2)
orthpls <- opls(dplyr::select(mtcars, -mpg), mtcars$mpg, predI = 1, orthoI = 1,
             plotL = FALSE, printL = FALSE)
plsda <- opls(dplyr::select(mtcars, -gear), as.factor(mtcars$gear),
              plotL = FALSE, printL = FALSE)
oplsda <- opls(dplyr::select(mtcars, -vs), as.factor(mtcars$vs),
               predI = 1, orthoI = NA,
               plotL = FALSE, printL = FALSE)

test_that("get_scores() returns a data frame", {
  expect_is(pca %>% get_scores(), "data.frame")
  expect_is(pls %>% get_scores(), "data.frame")
  expect_is(orthpls %>% get_scores(), "data.frame")
  expect_is(plsda %>% get_scores(), "data.frame")
  expect_is(oplsda %>% get_scores(), "data.frame")
})

test_that("get_VIP() returns a data frame", {
  expect_error(get_VIP(pca))
  expect_is(get_VIP(pls), "data.frame")
  expect_is(get_VIP(orthpls), "data.frame")
  expect_is(get_VIP(plsda), "data.frame")
  expect_is(get_VIP(oplsda), "data.frame")
})

test_that("get_plotdata() doesn't return warnings", {
  expect_silent(get_plotdata(pca))
  expect_silent(get_plotdata(pls))
  expect_silent(get_plotdata(orthpls))
  expect_silent(get_plotdata(plsda))
  expect_silent(get_plotdata(oplsda))
})
