#' Function to run tbl_regrerssion add-in.
#'
#' This function is to call shiny app for tbl_regression
#'
#' @importFrom magrittr %>%
#' @importFrom shinyjs useShinyjs hide show
#' @importFrom shinycssloaders withSpinner
#' @importFrom gt gt_output render_gt
#' @importFrom purrr map pmap map2_chr
#' @importFrom stringr str_c str_glue str_detect str_remove
#' @importFrom tibble enframe as_tibble
#' @importFrom tidyr unnest
#' @importFrom readr read_delim write_csv
#' @importFrom rlang set_names
#' @importFrom rmarkdown render
#' @importFrom flextable as_flextable
#' @import gtsummary
#' @import miniUI
#' @import shiny
#' @import dplyr
#' @import shinyWidgets
#' @import rclipboard

tbl_regression_addin <- function(){
  #call module------------------------
  variable_loader_modal_ui <- variableLoaderModalUI(id = "load_variable_name")

  #[UI parts]------------------
  # > Sidebar panel-----------------------------------------------------------

  setting_exponentiate  <- awesomeCheckbox("exponentiate", label="Exponentiate", value = FALSE)
  setting_include       <- pickerInput("include", label="Select Variables", choices=NA, options=list(`actions-box`=TRUE), multiple=TRUE)
  setting_single_row    <- pickerInput("single_row", label="Show Single Row", choices = NA, options=list(`actions-box`=TRUE), multiple=TRUE)
  setting_conf_level    <- numericInput("conf_level", label = "Confidence Level", min = 0, max = 1, value = 0.95, step = 0.01)
  setting_intercept     <- awesomeCheckbox("intercept", label="Intercept", value = FALSE)
  setting_add_reference <- awesomeCheckbox("add_reference", label="Add Referenct", value=FALSE)

  # > Dropdown component-----------------------------------------------------------------

  # >> Theme ----------------------
  setting_theme_language     <- selectInput("language", "Select Language", choices=c("de", "en", "es", "fr", "gu", "hi", "ja", "mr", "pt", "se", "zh-cn","zh-tw"), selected = "en")
  setting_theme_decimal_mark <- textInput("decimal_mark", "Decimal Mark:", ".")
  setting_theme_big_mark     <- textInput("big_mark", "Big Mark:", ",")
  setting_theme_iqr_sep      <- textInput("iqr_sep", "IQR Sep:", "-")
  setting_theme_ci_sep       <- textInput("ci_sep", "CI Sep:", "-")

  # > Dropdown buttons---------------------------
  dropdown_modify_label <- labelModifyDropDownUI("modify_label")

  dropdown_theme_setting <- dropdownButton(
    label = "Set Theme for Table",
    setting_theme_language,setting_theme_decimal_mark,setting_theme_big_mark,
    setting_theme_iqr_sep,setting_theme_ci_sep,
    circle=FALSE, status="primary", icon=icon("paint-roller")
  )

  # > Dlbuttons -------------------------------------

  dlbutton_excel <- downloadButton("dltable_word", "DL(Word)")
  dlbutton_csv <- downloadButton("dltable_csv", "DL(CSV)(data only)")
  dlbutton_html <- downloadButton("dltable_html", "DL(HTML)")

  # > Copy button-------------------------------------
  button_copy_script <- uiOutput("clip")

  # > CSS code--------------------
  css_bigfont <- function(inputId){str_c("#",inputId,".shiny-bound-input{font-size: 32px; line-height: 40px}")}

  # @@@UI ------------------------------
  ui <- fluidPage(
    tags$style(type="text/css",css_bigfont("decimal_mark") ),#decimal_mark.shiny-bound-input{font-size: 32px; line-height: 40px}"),
    tags$style(type="text/css",css_bigfont("big_mark") ),#big_mark.shiny-bound-input{font-size: 32px; line-height: 40px}"),
    tags$style(type="text/css",css_bigfont("iqr_sep") ),#iqr_sep.shiny-bound-input{font-size: 32px; line-height: 40px}"),
    tags$style(type="text/css",css_bigfont("ci_sep") ),#ci_sep.shiny-bound-input{font-size: 32px; line-height: 40px}"),
    useShinyjs(),
    rclipboardSetup(),
    titlePanel("Interactive tbl_regression"),
    sidebarLayout(
      sidebarPanel(
        setting_exponentiate, setting_include,setting_conf_level, setting_intercept, setting_add_reference,
        fluidRow( dlbutton_excel, dlbutton_csv, dlbutton_html ),
        fluidRow( button_copy_script )
      ),

      mainPanel(
        fluidRow(
          column(width = 2 ,
                 dropdown_modify_label   , hr(),
                 dropdown_theme_setting  , hr()),
          column(width = 10, shinycssloaders::withSpinner(gt::gt_output("regression_table")))
        ),
        fluidRow(
          h3("R script:"),
          verbatimTextOutput("script")
        )
      )
    )
  )

  #@@@ SERVER----------------------------------

  server <- function(input, output, session) {
    #[UI]-----------------------------------------------

    # > Edit label------------------------------
    label_vector <- labelModifyDropDownServer("modify_label", label_data = mod())

    # > Update select input:include----------------------
    observeEvent(mod(), {

      term_names <- get_terms(mod())

      updatePickerInput(
        session  = session,
        inputId  = "include",
        choices  = term_names,
        selected = term_names
      )
    })

    #[Load Data] ---------------------------------------

    # > variable_name: Module(variable_LoaderModalServer) -----------------------------
    target_types_for_models <- c("lm", "glm", "coxph", "clogit", "survreg", "lme4", "lmerMod", "geeglm", "gee")
    variable_name <- variableLoaderModalServer(id = "load_variable_name", target_type = target_types_for_models)

    # > mod() ------------------------------------
    mod <- reactive({
      req(variable_name())

      variable_name()
      read_this <- eval(expr=parse(text=variable_name()), envir=.GlobalEnv)
      return(read_this)
    })

    # [Make Regression Table] -------------------------------------------------

    # > summary_table() ----------------------------------------
    summary_table <- reactive({
      req(mod())

      # >> model----------------
      model <- mod()

      # >> set_label: make list for label---------------------------
      editted_label <- tryCatch(
        expr = {label_vector()},
        error = function(e) {
          return(NULL)
        }
      )

      # >> gtsummary::theme_gtsummary_language()-------------------------
      reset_gtsummary_theme()

      theme_gtsummary_language(
        language=input$language,
        decimal.mark=input$decimal_mark,
        big.mark=input$big_mark,
        iqr.sep=input$iqr_sep,
        ci.sep=input$ci_sep
      )

      # >> gtsummary::tbl_regression() -----------------------------------
      final_table <- tbl_regression(
        model,
        label = editted_label,
        exponentiate = input$exponentiate,
        include = input$include,
        show_single_row = NULL, #not implemented
        conf.level = input$conf_level,
        intercept = input$intercept,
        estimate_fun = NULL, #not implemented
        pvalue_fun = NULL, # will implement: make ui for style_pvalue()
        add_estimate_to_reference_rows = input$add_reference
      )

      return(final_table)
    })

    # [Script Text]-----------------------------------
    # > make script to copy and display -------------------

    script_to_generate_table <- reactive({

      var_name <- variable_name()

      # >> set_label----
      editted_label <- tryCatch(
        expr = {label_vector()},
        error = function(e) {
          return(NULL)
        }
      )

      if(is.null(editted_label)){
        set_label <- "NULL"
      }else{

        val <- editted_label %>% as.character()
        nam <- editted_label %>% names()

        set_label <- map2_chr(val,nam,~{str_glue('    "{.x}" = "{.y}"')}) %>%
          str_c(collapse = ",\n") %>%
          str_c("list(\n",.,"\n  )")
      }

      string_include <- str_c("c('", str_c(input$include, collapse="','"), "')")

      # >> base_text --------------------------
      base_text <- c(
        "#THIS TEXT IS EXPERIMENTAL AND IS UNDER DEVELOPMENT",
        "library(tidyverse)",
        "library(gtsummary)",
        "",
        "# Set theme for regression table ---------------------------------",
        "theme_gtsummary_language(",
        "  language = '{input$language}',",
        "  decimal.mark = '{input$decimal_mark}',",
        "  big.mark = '{input$big_mark}',",
        "  iqr.sep = '{input$iqr_sep}',",
        "  ci.sep = '{input$ci_sep}'",
        ")",
        "",
        "# Make regression table-----------------------------------",
        "regression_table <- tbl_regression(",
        "  x            = {var_name},",
        "  label        = {as.character(set_label)},",
        "  exponentiate = {as.character(input$exponentiate)},",
        "  include      = {string_include},",
        "  conf.level   = {as.character(input$conf_level)},",
        "  intercept    = {as.character(input$intercept)},",
        "  add_estimate_to_reference_rows = {as.character(input$add_reference)}",
        ")"
      )

      fintext <- str_c(base_text, collapse = "\n")
      res <- str_glue(fintext)

      return(res)
    })

    # > output$script ------------------------
    output$script <- renderText({ script_to_generate_table() }) #

    # [Buttons] ---------------------------------------
    # > Make clip button ------------------------------
    output$clip <- renderUI({
      rclipButton(
        "clipbtn",
        label="Copy script to clipboard",
        clipText=script_to_generate_table(),
        icon=icon("clipboard")
      )
    })

    # [Output] ---------------------------
    output$regression_table <- gt::render_gt({
      req(summary_table())
      summary_table() %>% as_gt()
    })

    # [DL button] --------------------------------------
    output$dltable_word <- downloadHandler(
      filename = function() {"regression_table.docx"},
      content = function(file){
        render( system.file("extdata","word_templateR.Rmd", package="gtsummaryAddin", mustWork = TRUE), output_file = file)
      }
    )

    output$dltable_csv <- downloadHandler(
      filename = function() {"regression_table.csv"},
      content = function(file){
        temp <- modified_appearance() %>% as_tibble()
        write_csv(temp,file)
      }
    )

    output$dltable_html <- downloadHandler(
      filename = function(){"regression_table.html"},
      content = function(file){
        render( system.file("extdata","html_templateR.rmd", package="gtsummaryAddin", mustWork = TRUE), output_file = file)
      }
    )
  }

  viewer <- browserViewer()
  runGadget(ui, server, viewer = viewer)

}
