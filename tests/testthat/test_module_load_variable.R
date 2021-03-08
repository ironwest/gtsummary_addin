library(gtsummaryAddin)
library(rlang)
library(purrr)
library(tibble)
library(magrittr)
library(shiny)
library(testthat)
library(shinytest)
library(checkmate)



test_that("Check if there is no data.frame", code = {
  testServer(variableLoaderModalServer, args = list(target_type="data.frame"), {
    expect_equal(ui_select_target, str_glue("There is no variable with class:{target_type} (Restart Add-in!)"))
  })
})
