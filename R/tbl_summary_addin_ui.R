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
