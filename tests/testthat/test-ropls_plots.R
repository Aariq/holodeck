context("test that ropls plots work")

library(ropls)
library(ggplot2)
data(foods) ## see Eriksson et al. (2001); presence of 3 missing values (NA)
foodMN <- as.matrix(foods[, colnames(foods) != "Country"])
rownames(foodMN) <- foods[, "Country"]
foo.pca <- opls(foodMN, printL = FALSE, plotL = FALSE)

data(cornell) ## see Tenenhaus, 1998
cornell.pls <- opls(as.matrix(cornell[, grep("x", colnames(cornell))]),
                    cornell[, "y"], printL = FALSE, plotL = FALSE)
cornell.opls <- opls(as.matrix(cornell[, grep("x", colnames(cornell))]),
                     cornell[, "y"], printL = FALSE, plotL = FALSE, predI = 1, orthoI = 1)

data(sacurine)
sacurine.plsda <- opls(sacurine$dataMatrix, sacurine$sampleMetadata[, "gender"], plotL = FALSE, printL = FALSE)
sacurine.oplsda <- opls(sacurine$dataMatrix, sacurine$sampleMetadata[, "gender"], predI = 1, orthoI = NA, plotL = FALSE, printL = FALSE)

test_that("returns plots", {
  expect_is(plot_pca(foo.pca), "ggplot")
  expect_is(plot_pls(cornell.pls), "ggplot")
  expect_is(plot_plsda(sacurine.plsda), "ggplot")
  expect_is(plot_opls(cornell.opls), "ggplot")
  expect_is(plot_oplsda(sacurine.oplsda), "ggplot")
})

test_that("plotting functions don't give warnings", {
  expect_silent(plot_pca(foo.pca))
  expect_silent(plot_pls(cornell.pls))
  expect_silent(plot_plsda(sacurine.plsda))
  expect_silent(plot_opls(cornell.opls))
  expect_silent(plot_oplsda(sacurine.oplsda))
})
