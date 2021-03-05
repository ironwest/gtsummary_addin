#' Argument option for tbl_summary:type
#'
#' @return character vector

tbl_summary_type <- function(){
  return(
    c("continuous", "continuous2", "categorical", "dichotomous")
  )
}

#' Argument option for tbl_summary %>%  add_p()
#'
#' @return character vector
test_argument_for_add_p_tbl_summary <- function(){
  return(
    c("t.test","aov","kruskal.test","wilcox.text","chisq.test",
      "chisq.test.no.correct","fisher.test","mcnemar.test",
      "lme4","paired.t.test","paired.wilcox.test","prop.test",
      "ancova")
  )
}
