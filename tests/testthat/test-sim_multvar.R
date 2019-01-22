context("Simulated multivariate data functions")

test_that("sim_cat returns a tibble", {
  expect_that(sim_cat(N = 30, n_groups = 2), is_a("tbl_df"))
})

test_that("sim_cat works with uneven group size", {
  expect_that(sim_cat(N = 30, n_groups = 5), is_a("tbl_df"))
})

test_that("sim_cat gives warnings/erros when too many groups", {
  expect_that(sim_cat(N = 30, n_groups = 11), throws_error())
  expect_that(sim_cat(N = 30, n_groups = 7), gives_warning())
})


test_that("sim_covar returns a tibble", {
  expect_that(sim_cat(N = 30, n_groups = 2) %>% sim_covar(p = 5, var = 1, cov = 0.5, name = "test"),
              is_a("tbl_df"))
})

test_that("sim_covar handles var and cov longer than 1", {
  expect_that(sim_cat(30, 2) %>% sim_covar(p = 3, var = c(1, 0.5, 1), cov = 0.5), is_a("tbl_df"))
  expect_that(sim_cat(30, 2) %>% sim_covar(p = 3, var = 1, cov = c(0.4, 0.5, 0.6)), throws_error())

})

context("Check that chaining functions works")

test_that("default for name= works", {
  expect_that(sim_cat(30, 2) %>%
                sim_covar(p = 20, var = 1, cov = 0.5) %>%
                sim_covar(p = 20, var = 1, cov = 0.4) %>%
                sim_discr(p = 20, var = 1, cov = 0.1, group_means = c(0, 1)),
              is_a("tbl_df"))
})

test_that("functions work with custom group column name", {
  expect_that(sim_cat(30,2) %>% rename("testing" = group) %>%
                sim_discr(p = 20, var = 1, cov = 0.5, group = "testing", group_means = c(1,2)), is_a("tbl_df"))
})


context("check RNG is working as expected")

test_that("RNG is consistent for sim_covar", {
  expect_that(sim_cat(30,2) %>% sim_covar(p = 5, var = 1, cov = 0.5, seed = 100),
              is_identical_to(sim_cat(30,2) %>% sim_covar(p = 5, var = 1, cov = 0.5, seed = 100)))
  })

test_that("RNG is consistent for sim_discr", {
  expect_that(sim_cat(30,2) %>%
              sim_discr(p = 5, var = 1, cov = 0.5, group_means = c(0,1), seed = 100),
              is_identical_to(
                sim_cat(30,2) %>%
                  sim_discr(p = 5, var = 1, cov = 0.5, group_means = c(0,1), seed = 100)
                )
              )
})


test_that("sim_missing adds NAs", {
  expect_true(sim_cat(30,3) %>%
                sim_covar(6, var = 1, cov = 0.5) %>%
                sim_missing(prop = 0.1) %>% anyNA)
})


test_that("sim_missing adds the correct proprotion of NAs", {
  expect_that(sim_cat(10, 2) %>%
                sim_covar(10, var = 1, cov = 0.5) %>%
                sim_missing(prop = 0.1) %>% is.na() %>% sum(),
              is_equivalent_to(10))
})


test_that("sim_missing works with small proportions", {
  expect_warning(sim_cat(10, 2) %>%
                  sim_covar(10, 1, 0, name = "uncorr") %>%
                  sim_covar(10, 1, 0.5, name = "corr") %>%
                  sim_discr(5, 1, 0, group_means = c(-1, 1), name = "discr") %>%
                  sim_missing(prop = 0.0001))
})
