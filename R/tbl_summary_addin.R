#' Function to run tbl_summary add-in.
#'
#' This function is to call shiny app for tbl_summary/
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
#' @importFrom bsplus bs_embed_popover shiny_iconlink
#' @import gtsummary
#' @import miniUI
#' @import shiny
#' @import dplyr
#' @import shinyWidgets
#' @import rclipboard

tbl_summary_addin <- function(){
  #call module------------------------
  variable_loader_modal_ui <- variableLoaderModalUI(id = "load_variable_name")

  #[UI parts]------------------
  # > Sidebar panel-----------------------------------------------------------
  setting_by <- selectInput("by", label = "Group By", choices = NA)
  setting_variables <- pickerInput("var", label="Select Variables", choices=NA, options=list(`actions-box`=TRUE), multiple=TRUE)
  setting_statistics <- div(
    textInput("statistics_continuous",
      label = "Statistics(Continuous) * use {mean / median / sd / var / min / max / p##}",
      value = "{mean} ({sd})"
    ),
    textInput(
      "statistics_categorical",
      label = "Statistics(Categorical) * use {n / N / p}",
      value = "{n} / {N} ({p}%)"
    )
  )
  setting_digits <- numericInput("digits","Digits",value = 2, step = 1)
  setting_missing <- selectInput("missing","Missing * Indicate whether to include count of NA values in the table", choices = c("ifany","no","always"), selected = "ifany")
  setting_missingtext <- textInput("missing_text","Missing text", value = "(Missing)")
  setting_percent <- selectInput("percent", "Percent", choices = c("column", "row", "cell"), selected = "column")

  # > Dropdown component-----------------------------------------------------------------
  # >> Add column----------------------
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

  # >> Theme ----------------------
  setting_theme_language     <- selectInput("language", "Select Language", choices=c("de", "en", "es", "fr", "gu", "hi", "ja", "mr", "pt", "se", "zh-cn","zh-tw"), selected = "en")
  setting_theme_decimal_mark <- textInput("decimal_mark", "Decimal Mark:", ".")
  setting_theme_big_mark     <- textInput("big_mark", "Big Mark:", ",")
  setting_theme_iqr_sep      <- textInput("iqr_sep", "IQR Sep:", "-")
  setting_theme_ci_sep       <- textInput("ci_sep", "CI Sep:", "-")

  # > Dropdown buttons---------------------------
  dropdown_add_column <- dropdownButton(
    label="Add Columns (p,N,overall)",
    setting_add_p, setting_add_overall, setting_add_n,
    circle=FALSE, status="primary", icon=icon("gear"))

  dropdown_modify_label <- dropdownButton(
    label="Variable Label Setting",
    uiOutput("edit_label"),
    circle=FALSE, status="primary", icon=icon("tag")
  )

  dropdown_header_setting <- dropdownButton(
    label="Table Header Setting",
    uiOutput("header"), prettyCheckbox("bold_label", "Bold Label"),
    circle=FALSE, status="primary", icon=icon("heading")
  )

  dropdown_set_column_type <- dropdownButton(
    label="Variable Type Setting",
    uiOutput("set_column_type"),
    circle=FALSE, status="primary", icon=icon("cat"), width=12
  )

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
    use_bs_popover(),
    rclipboardSetup(),
    titlePanel("Interactive tbl_summary"),
    sidebarLayout(
      sidebarPanel(
        setting_variables,setting_by,setting_statistics,setting_digits,setting_missing, setting_missingtext,setting_percent,
        fluidRow( dlbutton_excel, dlbutton_csv, dlbutton_html ),
        fluidRow( button_copy_script )
      ),

      mainPanel(
        fluidRow(
          column(width = 2 ,
                 dropdown_add_column     , hr(),
                 dropdown_modify_label   , hr(),
                 dropdown_set_column_type, hr(),
                 dropdown_header_setting , hr(),
                 dropdown_theme_setting  , hr()),
          column(width = 10, shinycssloaders::withSpinner(gt::gt_output("table1")))
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


    # Hide all UI ----------------------------------
    hide("dltable_word")
    hide("dltable_csv")
    hide("var")
    hide("by")
    hide("percent")
    hide("missing")
    hide("add_p_condition")
    hide("add_p_categorical")
    hide("add_p_continuous")
    hide("statistics_categorical")
    hide("statistics_continuous")
    hide("digits")
    hide("missing_text")
    hide("add_overall_condition")
    hide("add_overall_last")
    hide("add_overall_label")
    hide("drop_down_add_column")
    hide("drop_down_modify_label")
    hide("drop_down_header_setting")
    hide("drop_down_set_column_type")
    hide("dropdown_theme_setting")
    hide("dltable_html")
    hide("bold_label")
    hide("footnote_p")

    #[UI]-----------------------------------------------
    # > Show/Hide logic--------------------------------
    observeEvent(input$by,{
      if(input$by == "NA"){
        updateMaterialSwitch(session = session, inputId = "add_p_condition", value = FALSE)
        hide("add_p_condition")

        updateMaterialSwitch(session = session, inputId = "add_overall_condition", value = FALSE)
        hide("add_overall_condition")

      }else{
        show("add_p_condition")
        show("add_overall_condition")
      }
    })

    observeEvent(input$add_p_condition,{
      if(input$add_p_condition){
        show("add_p_categorical")
        show("add_p_continuous")
      }else{
        hide("add_p_categorical")
        hide("add_p_continuous")
      }
    })

    observeEvent(input$add_overall_condition,{
      if(input$add_overall_condition){
        show("add_overall_last")
        show("add_overall_label")
      }else{
        hide("add_overall_last")
        hide("add_overall_label")
      }
    })

    # > Edit label------------------------------
    output$edit_label <- renderUI({
      req(selected_data())

      tgtcols <- colnames(selected_data())

      returning_ui <- div(
        map(tgtcols, ~{
          acol <- .
          aId <- str_c("col_label_", .)
          return(textInput(inputId = aId, label = acol, value = acol))
        }),
        actionButton("update_label","Update Label")
      )

      return(returning_ui)
    })

    # > Modify label logic -----------------------------------
    label_vector <- eventReactive(input$update_label, {

      label_inputs <- names(input) %>%
        enframe(name = NULL, value = "id") %>%
        mutate(value = map(id, ~{input[[.]]})) %>%
        filter(str_detect(id,"^col_label_")) %>%
        unnest(value) %>%
        mutate(id = str_remove(id, "col_label_"))

      res <- label_inputs$id
      names(res) <- label_inputs$value

      return(res)
    })

    # > Update select input:var----------------------
    observeEvent(dat(), {

      column_names <- dat() %>%colnames()

      updatePickerInput(
        session  = session,
        inputId  = "var",
        choices  = column_names,
        selected = column_names
      )
    })

    # > Update select input:by----------------------
    observeEvent(input$var, {
      updateSelectInput(
        session = session,
        inputId = "by",
        choices = c(NA_character_, input$var)
      )
    })

    # > Show relevant UI when variable selected--------------------
    observeEvent(dat(), {
      show("dltable_word")
      show("dltable_csv")
      show("dltable_html")
      show("var")
      show("by")
      show("statistics_categorical")
      show("statistics_continuous")
      show("digits")
      show("missing")
      show("missing_text")
      show("drop_down_add_column")
      show("drop_down_modify_label")
      show("drop_down_header_setting")
      show("drop_down_set_column_type")
      show("dropdown_theme_setting")
      show("bold_label")
      show("footnote")
      show("percent")
    })

    #[Load Data] ---------------------------------------

    # > variable_name: Module(variable_LoaderModalServer) -----------------------------
    variable_name <- variableLoaderModalServer(id = "load_variable_name", target_type = "data.frame")

    # > dat() ------------------------------------
    dat <- reactive({
      req(variable_name())

      variable_name()
      read_this <- eval(expr=parse(text=variable_name()), envir=.GlobalEnv)
      return(read_this)
    })

    # > selected_data()-----------------------------
    selected_data <- reactive({
      req(dat())

      return_this <- dat()

      # >> select data depend on input$var-----------------------
      if(is.null(input$var)){
        return_this <- tibble(` ` = "Select at least one variable")
      }else{
        return_this <- return_this %>%
          select(input$var)
      }

      return(return_this)
    })

    # [Make Summary Table] -------------------------------------------------

    # > summary_table() ----------------------------------------
    summary_table <- reactive({
      req(selected_data())

      # >> table_data----------------
      table_data <- selected_data()

      # >> set_by: name depend on renamed vector----------------
      if(input$by == "NA"){ set_by <- NULL }else{ set_by <- input$by }

      # >> set_label: make list for label---------------------------
      editted_label <- tryCatch(
        expr = {label_vector()},
        error = function(e) {
          return(NULL)
        }
      )

      if(is.null(editted_label)){
        set_label <- NULL
      }else{

        val <- editted_label %>% as.character()
        nam <- editted_label %>% names()

        set_label <- as.list(nam) %>% set_names(val)
      }

      # >> gtsummary::theme_gtsummary_language()-------------------------
      reset_gtsummary_theme()

      theme_gtsummary_language(
        language=input$language,
        decimal.mark=input$decimal_mark,
        big.mark=input$big_mark,
        iqr.sep=input$iqr_sep,
        ci.sep=input$ci_sep
      )

      # >> gtsummary::tbl_summary() -----------------------------------
      final_table <- tbl_summary(
        data = table_data,
        by = set_by,
        label = set_label,
        statistic = list(
          all_continuous()   ~ input$statistics_continuous,
          all_categorical() ~ input$statistics_categorical
        ),
        digits = all_continuous() ~ input$digits,
        type = type_argument(),
        value = NULL,      #value to display for dichotomous variables (not implemented yet)
        missing = input$missing,
        missing_text = input$missing_text,
        sort = NULL,       # Not implemented yet
        percent = input$percent
      )

      # >> gtsummary::add_p() --------------------------------------------
      if(input$add_p_condition){
        final_table <- final_table %>%
          add_p(test = list(all_continuous()  ~ input$add_p_continuous,
                            all_categorical() ~ input$add_p_categorrical))
      }else{
        #do nothing
      }

      # >> gtsummary::add_overall ---------------------------------------
      if(input$add_overall_condition){

        final_table <- final_table %>%
          add_overall(last = input$add_overall_last, col_label = input$add_overall_label)
      }

      # >> gtsummary::add_n()--------------------------------------------
      if(input$add_n_condition){
        final_table <- final_table %>%
          add_n()
      }

      return(final_table)
    })

    # [Column Type setting UI]----------------------------------------

    # > setting_table() -------------------
    setting_table <- reactive({
      req(selected_data())

      current_data <- selected_data()

      settings <-enframe(map(current_data, class)) %>%
        unnest(value) %>%
        mutate(type = case_when(
          value %in% c("numeric", "integer") ~ "continuous",
          value %in% c("factor","character","logical") ~ "categorical"
        )) %>%
        mutate(id = str_c("type_",1:n()))

      return(settings)
    })

    # > output$set_column_type----------------
    output$set_column_type <- renderUI({
      req(setting_table())
      settings <- setting_table()

      finui <- column(width = 6,pmap(.l = list(settings$id, settings$name, settings$type), ~{
        radioGroupButtons(
          inputId  = ..1,
          label    = ..2,
          choices  = c("categorical","continuous"),
          selected = ..3
        )
      }))

      return(finui)
    })

    # > type_argument() -----------------------
    type_argument <- reactive({
      req(setting_table())

      settings <- setting_table()

      settings <- settings %>%
        mutate(current_val = map2_chr(id, type, ~{
          if_else( is.null(input[[.x]]), .y, input[[.x]] )
        }))

      as.list(settings$current_val) %>% set_names(settings$name) %>%
        return()

    })

    #[Modify Appearance] ----------------------------

    # > header_names() ------------------------------
    header_names <- reactive({
      req(summary_table())
      summary_table() %>%
        show_header_names() %>%
        as_tibble() %>%
        return()
    })

    # > modify_apeearance()-------------------------
    modified_appearance <- reactive({
      req(summary_table())
      fin <- summary_table()

      if(!is.null(input$label)){
        fin <- fin %>% modify_header(label = input$label)
      }


      if(is.null(input$spanning_header) ){

      }else if(input$spanning_header == ""){

      }else{
        fin <- fin %>% modify_spanning_header(starts_with("stat_") ~ input$spanning_header)
      }

      if(input$bold_label){ fin <- fin %>% bold_labels() }

      return(fin)
    })

    # > output$header: generate UI for modify appearance----------------
    output$header <- renderUI({
      req(header_names())
      hd <- header_names()
      length_hd <- nrow(hd)


      if(is.null(input$label)){
        label_text <- hd %>% filter(column == "label") %>% pull(label)
      }else{
        label_text <- input$label
      }

      if(is.null(input$spanning_header)){
        spanning_header_text <- ""
      }else{
        spanning_header_text <- input$spanning_header
      }

      head_ui <- column(width = 12,
                        textInput("label","Label",label_text),
                        textInput("spanning_header", "Spanning Header", spanning_header_text)
      )

      return(head_ui)
    })

    # [Script Text]-----------------------------------
    # > make script to copy and display -------------------
    script_to_generate_table <- reactive({
      var_name <- variable_name()

      # >> set_by-----
      if(input$by == "NA") set_by <- "NULL" else set_by <- input$by

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

      # >> type_argument-------
      if(is.null(type_argument())){
        set_type <- "NULL"
      }else{
        typ <- type_argument() %>% as.character()
        nam <- type_argument() %>% names()

        set_type <- map2_chr(nam, typ, ~{
          str_glue("    {.x} ~ '{.y}'")
        }) %>%
          str_c(collapse = ",\n") %>%
          str_c("list(\n",.,"\n  )")
      }

      # >> add header------------

      if(!is.null(input$label)){
        add1 <- "summarised_table <- summarised_table %>% \n  modify_header(label = '{input$label}')\n\n"
      }else{
        add1 <- ""
      }

      if(is.null(input$spanning_header) ){
        add2 <- ""
      }else if(input$spanning_header == ""){
        add2 <- ""
      }else{
        add2 <- "summarised_table <- summarised_table %>% \n  modify_spanning_header(starts_with('stat_') ~ '{input$spanning_header}')\n\n"
      }

      if(input$bold_label){
        add3 <- "summarised_table <- summarised_table %>% \n  bold_labels()\n\n"
      }else{
        add3 <- ""
      }

      add_header <- c(add1,add2,add3) %>% str_c(collapse = "")

      # >> add columns----------------------------
      if(input$add_p_condition){
        add_col1_1 <- "summarised_table <- summarised_table %>% \n"
        add_col1_2 <- "  add_p(test = list(all_continuous()  ~ '{input$add_p_continuous}',\n"
        add_col1_3 <- "                    all_categorical() ~ '{input$add_p_categorical}'))\n\n"
      }else{
        add_col1_1 <- ""
        add_col1_2 <- ""
        add_col1_3 <- ""
      }


      if(input$add_overall_condition){
        add_col2_1 <- "summarised_table <- summarised_table %>% \n"
        add_col2_2 <- "  add_overall(last = {input$add_overall_last}, col_label = '{input$add_overall_label}')\n\n"
      }else{
        add_col2_1 <- ""
        add_col2_2 <- ""
      }

      if(input$add_n_condition){
        add_col3_1 <- "summarised_table <- summarised_table %>% add_n()\n\n"
      }else{
        add_col3_1 <- ""
      }

      add_columns <- c(
        add_col1_1, add_col1_2, add_col1_3,
        add_col2_1, add_col2_2,
        add_col3_1
      ) %>%
        str_c(collapse = "")

      # >> base_text --------------------------
      base_text <- c(
        "#THIS TEXT IS EXPERIMENTAL AND IS UNDER DEVELOPMENT",
        "library(tidyverse)",
        "library(gtsummary)",
        "",
        "# Set theme for table ---------------------------------",
        "theme_gtsummary_language(",
        "  language = '{input$language}',",
        "  decimal.mark = '{input$decimal_mark}',",
        "  big.mark = '{input$big_mark}',",
        "  iqr.sep = '{input$iqr_sep}',",
        "  ci.sep = '{input$ci_sep}',",
        ")",
        "",
        "# Make summary table-----------------------------------",
        "summarised_table <- tbl_summary(",
        "  data  = {var_name},",
        "  by    = {set_by},",
        "  label = {set_label},",
        "  type  = {set_type},",
        "  statistic = list(",
        "    all_continuous() ~ '{input$statistics_continuous}',",
        "    all_categorical() ~ '{input$statistics_categorical}'",
        "  ),",
        "  digits = list(all_continuous() ~ {input$digits}),",
        "  missing = '{input$missing}',",
        "  missing_text = '{input$missing_text}',",
        "  percent = '{input$percent}'",
        ")",
        "",
        "#Add Header--------------------------",
        add_header,
        "",
        "#Add column(N, P, Overall)-----------------------------",
        add_columns
      )

      fintext <- str_c(base_text, collapse = "\n")

      return(str_glue(fintext))
    })

    # > output$script ------------------------
    output$script <- renderText({ script_to_generate_table() })

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
    output$table1 <- gt::render_gt({
      req(dat())
      modified_appearance() %>% as_gt()
    })

    # [DL button] --------------------------------------
    output$dltable_word <- downloadHandler(
      filename = function() {"table1.docx"},
      content = function(file){

        render( system.file("extdata","word_template.Rmd", package="gtsummaryAddin", mustWork = TRUE), output_file = file)
      }
    )

    output$dltable_csv <- downloadHandler(
      filename = function() {"table1.csv"},
      content = function(file){

        temp <- modified_appearance() %>% as_tibble()
        write_csv(temp,file)
      }
    )

    output$dltable_html <- downloadHandler(
      filename = function(){"table1.html"},
      content = function(file){
        render( system.file("extdata","html_template.rmd", package="gtsummaryAddin", mustWork = TRUE), output_file = file)
      }
    )
  }

  viewer <- browserViewer()
  runGadget(ui, server, viewer = viewer)

}
