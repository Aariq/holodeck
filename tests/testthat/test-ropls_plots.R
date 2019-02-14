context("test that ropls plots work")

test_that("returns plots", {
  library(ropls)
  library(ggplot2)
  data(foods) ## see Eriksson et al. (2001); presence of 3 missing values (NA)
  foodMN <- as.matrix(foods[, colnames(foods) != "Country"])
  rownames(foodMN) <- foods[, "Country"]
  foo.pca <- opls(foodMN, plotL = FALSE)
  
  data(cornell) ## see Tenenhaus, 1998
  cornell.pls <- opls(as.matrix(cornell[, grep("x", colnames(cornell))]),
                      cornell[, "y"], plotL = FALSE)
  
  data(sacurine)
  sacurine.plsda <- opls(sacurine$dataMatrix, sacurine$sampleMetadata[, "gender"], plotL = FALSE)
  sacurine.oplsda <- opls(sacurine$dataMatrix, sacurine$sampleMetadata[, "gender"], predI = 1, orthoI = NA, plotL = FALSE)
  
  
  expect_is(plot_pca(foo.pca), "ggplot")
  expect_is(plot_pls(cornell.pls), "ggplot")
  expect_is(plot_plsda(sacurine.plsda), "ggplot")
  expect_is(plot_opls(sacurine.oplsda), "ggplot")
})
