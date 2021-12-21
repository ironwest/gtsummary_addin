#' generate sidebar UI for tbl_summary_addin
#' @import shiny
#' @importFrom tibble lst
#' @importFrom shinyWidgets pickerInput
#' @return list

tbl_summary_addin_sidebar_ui <- function(){
  setting_by        <- selectInput(
    inputId = "by",
    label   = "Group By",
    choices = NA
  )

  setting_variables <- pickerInput(
    inputId  = "var",
    label    = "Select Variables",
    choices  = NA,
    options  = list(`actions-box`=TRUE),
    multiple = TRUE
  )

  setting_statistics <- div(
    textInput(inputId = "statistics_continuous",
              label   = "Statistics(Continuous) * use {mean / median / sd / var / min / max / p##}",
              value   = "{mean} ({sd})"
    ),
    textInput(
      "statistics_categorical",
      label = "Statistics(Categorical) * use {n / N / p}",
      value = "{n} / {N} ({p}%)"
    )
  )

  setting_digits <- numericInput(
    inputId = "digits",
    label   = "Digits",
    value   = 2,
    step    = 1
  )

  setting_missing <- selectInput(
    inputId  = "missing",
    label    = "Missing * Indicate whether to include count of NA values in the table", choices = c("ifany","no","always"),
    selected = "ifany"
  )

  setting_missingtext <- textInput(
    inputId = "missing_text",
    label   = "Missing text",
    value = "(Missing)"
  )

  setting_percent <- selectInput(
    inputId = "percent",
    label   = "Percent",
    choices = c("column", "row", "cell"),
    selected = "column"
  )

  return(
    lst(
      setting_by,
      setting_variables,
      setting_statistics,
      setting_digits,
      setting_missing,
      setting_missingtext,
      setting_percent
    )
  )

}

#' generate dropdown UI for add_p for tbl_summary_addin
#' @import shiny
#' @importFrom shinyWidgets materialSwitch prettyCheckbox dropdownButton
#' @return list

tbl_summary_addin_dropdown_ui_add_column <- function(){
  setting_add_p <- div(
    materialSwitch("add_p_condition","Add p", status = "primary") %>% add_popover_help("add p value","http://www.danieldsjoberg.com/gtsummary/reference/add_p.tbl_survfit.html"),
    selectInput("add_p_categorical", "Test for categorical data", choices=add_p_tbl_summary_test(), selected="chisq.test"),
    selectInput("add_p_continuous" , "Test for continuous data" , choices=add_p_tbl_summary_test(), selected="kruskal.test")
  )

  setting_add_overall <- div(
    materialSwitch("add_overall_condition", label="Add Overall", status="primary"),
    prettyCheckbox("add_overall_last", label="Last", value=FALSE),
    textInput("add_overall_label", label="Label", value="**Overall**, N = {N}")
  )

  setting_add_n <- div(
    materialSwitch("add_n_condition", label="Add N", status="primary")
  )

  dropdown_add_column <- dropdownButton(
    label="Add Columns (p,N,overall)",
    setting_add_p, setting_add_overall, setting_add_n,
    circle=FALSE, status="primary", icon=icon("gear"))

  return(dropdown_add_column)
}
