# library(miniUI)
# library(gt)
# library(gtsummary)
# library(readr)
# library(magrittr)
# library(dplyr)
# library(shiny)
# library(openxlsx)
# library(shinycssloaders)
# library(shinyjs)
# library(shinyWidgets)
# library(purrr)
# library(stringr)
# library(tibble)
# library(tidyr)
# library(rlang)
# library(rmarkdown)
# library(flextable)

#' @importFrom magrittr %>%
#' @importFrom shinyjs useShinyjs hide show
#' @importFrom shinycssloaders withSpinner
#' @importFrom gt gt_output render_gt
#' @importFrom purrr map pmap map2_chr
#' @importFrom stringr str_c str_glue
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
#' @import knitr

tbl_summary_addin <- function(){

  choices_add_p_test <- c(
    "t.test","aov","wilcox.test","kruskal.test",
    "chisq.test","chisq.test.no.correct","fisher.test"
  )

  input_file <- fileInput(
    inputId = "file",
    label =  "Choose CSV File",
    multiple = FALSE,
    accept = c("text/csv",
               "text/comma-separated-values,text/plain",
               ".csv")
  )


  set_csv_delimiter <- awesomeRadio(
    inputId = "set_csv_delimiter",
    label = "CSV Delimiter:",
    choices = list(",(comma)" = ",", ";(semi-colon)" = ";", "\t(tab)" = "\t"),
    selected = ",",
    inline = TRUE,
    status = "success"
  )

  #Setting for sidebar panel--------------------------------------------
  setting_by <- selectInput("by", label = "Group By", choices = NA)

  setting_variables <- pickerInput(
    inputId = "var",
    label = "Select Variables",
    choices = NA,
    options = list(
      `actions-box` = TRUE),
    multiple = TRUE
  )

  setting_statistics <- div(
    textInput(
      "statistics_continuous",
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

  setting_missingtext <- textInput("missing_text","Missing text", value = "(Missing)")

  #setting for dropdown--------------------------
  setting_add_p <- div(
    materialSwitch("add_p_condition","Add p", status = "primary"),
    selectInput("add_p_categorical","Test for categorical data", choices = choices_add_p_test, selected = "chisq.test"),
    selectInput("add_p_continuous", "Test for continuous data" , choices = choices_add_p_test, selected = "kruskal.test")
  )

  setting_add_overall <- div(
    materialSwitch("add_overall_condition","Add Overall", status = "primary"),
    prettyCheckbox("add_overall_last", label = "Last", value = FALSE),
    textInput("add_overall_label",label = "Label", value = "**Overall**, N = {N}")
  )

  setting_add_n <- div(
    materialSwitch("add_n_condition","Add N", status = "primary"),
  )

  #dropdown buttons---------------------------
  dropdown_add_column <- dropdownButton(
    inputId = "drop_down_add_column",
    label = "Add Columns (p,N,overall)",
    setting_add_p,
    setting_add_overall,
    setting_add_n,
    circle = FALSE,
    status = "primary",
    icon = icon("gear"))

  dropdown_modify_label <- dropdownButton(
    inputId = "drop_down_modify_label",
    label = "Variable Label Setting",
    uiOutput("edit_label"),
    circle = FALSE,
    status = "primary",
    icon = icon("tag")
  )

  dropdown_header_setting <- dropdownButton(
    inputId = "drop_down_header_setting",
    label = "Table Header Setting",
    uiOutput("header"),
    prettyCheckbox("bold_label", "Bold Label"),
    circle = FALSE,
    status = "primary",
    icon = icon("heading")
  )

  dropdown_set_column_type <- dropdownButton(
    inputId = "drop_down_set_column_type",
    label = "Variable Type Setting",
    uiOutput("set_column_type"),
    circle = FALSE,
    status = "primary",
    icon = icon("cat"),
    width = 12
  )

  #dlbuttons -------------------------------------

  dlbutton_excel <- downloadButton("dltable_word", "DL(Word)")
  dlbutton_csv <- downloadButton("dltable_csv", "DL(CSV)(data only)")
  dlbutton_html <- downloadButton("dltable_html", "DL(HTML)")

  #modify appearance------------------------------

  ui <- fluidPage(
    useShinyjs(),

    titlePanel("Summarise Your Data!"),

    sidebarLayout(
      sidebarPanel(
        input_file,
        set_csv_delimiter,
        setting_variables,
        setting_by,
        setting_statistics,
        setting_digits,
        setting_missingtext,
        fluidRow(dlbutton_excel, dlbutton_csv, dlbutton_html),
        hr(),
        p("This app uses following great packages! gt, gtsummary, readr, magrittr, dplyr, shiny, openxlsx, shinycssloaders, shinyjs, shinyWidgets, purrr, stringr, tibble, tidyr, rlang, rmarkdown and flextable."),
        hr(),
        fluidRow(a("Script for this app is placed in here", href = "https://github.com/ironwest/egsummary"))
      ),

      mainPanel(
        fluidRow(
          column(width = 2 ,
                 dropdown_add_column     , hr(),
                 dropdown_modify_label   , hr(),
                 dropdown_set_column_type, hr(),
                 dropdown_header_setting , hr()),
          column(width = 10, shinycssloaders::withSpinner(gt::gt_output("table1")))
        ),
        fluidRow(
          h3("R script:"),
          verbatimTextOutput("script")
        )
      )
    )
  )

  server <- function(input, output, session) {

    #Hide all UI ----------------------------------
    hide("dltable_word")
    hide("dltable_csv")
    hide("var")
    hide("by")
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
    hide("dltable_html")
    hide("bold_label")
    hide("footnote_p")

    #UI show/hide logic --------------------------------
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

    #UI: edit label------------------------------
    output$edit_label <- renderUI({
      req(dat())

      tgtcols <- colnames(dat())

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

    #modify label logic -----------------------------------
    label_vector <- eventReactive(input$update_label, {
      #_Label Modification---------------------------
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

    #Update Var and By select input----------------------
    observeEvent(dat(), {

      column_names <- dat() %>%
        colnames()

      updatePickerInput(
        session  = session,
        inputId  = "var",
        choices  = column_names,
        selected = column_names
      )
    })

    observeEvent(input$var, {
      updateSelectInput(
        session = session,
        inputId = "by",
        choices = c(NA_character_, input$var)
      )
    })

    #data logic ---------------------------------------

    #Show relevant UI when file uploaded--------------------
    observeEvent(input$file, {

      show("dltable_word")
      show("dltable_csv")
      show("dltable_html")
      show("var")
      show("by")
      show("statistics_categorical")
      show("statistics_continuous")
      show("digits")
      show("missing_text")
      show("drop_down_add_column")
      show("drop_down_modify_label")
      show("drop_down_header_setting")
      show("drop_down_set_column_type")
      show("bold_label")
      show("footnote")
    })

    # dat() -----------------------------
    dat <- reactive({
      req(input$file)

      res <- read_delim(input$file$datapath, delim = input$set_csv_delimiter)

      return(res)
    })


    #Make Summary Table-------------------------------
    summary_table <- reactive({
      req(dat())

      table_data <- dat()

      #_set by name depend on renamed vector----------------
      if(input$by == "NA"){
        set_by <- NULL
      }else{
        set_by <- input$by
      }

      #_select data depend on input$var
      if(is.null(input$var)){
        table_data <- tibble(` ` = "Select at least one variable")
      }else{
        table_data <- table_data %>%
          select(input$var)
      }

      #make list for label---------------------------
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

      #_Tbl summary-----------------------------------

      final_table <- tbl_summary(
        data = table_data,
        by = set_by,
        label = set_label,
        type = type_argument(),
        statistic = list(
          all_continuous()   ~ input$statistics_continuous,
          all_categorical() ~ input$statistics_categorical
        ),
        digits = all_continuous() ~ input$digits,
        missing_text = input$missing_text
      )

      #Add columns --------------------------------------------
      if(input$add_p_condition){
        final_table <- final_table %>%
          add_p(test = list(all_continuous()  ~ input$add_p_continuous,
                            all_categorical() ~ input$add_p_categorrical))
      }else{
        #do nothing
      }

      if(input$add_overall_condition){

        final_table <- final_table %>%
          add_overall(last = input$add_overall_last, col_label = input$add_overall_label)
      }

      if(input$add_n_condition){
        final_table <- final_table %>%
          add_n()
      }

      return(final_table)
    })

    #generate ui for column_type_setting------------------

    setting_table <- reactive({
      req(dat())

      current_data <- dat()

      settings <- enframe(map(current_data, typeof)) %>%
        unnest(value) %>%
        mutate(type = case_when(
          value %in% c("double","integer") ~ "continuous",
          value %in% c("factor","character") ~ "categorical"
        )) %>%
        mutate(id = str_c("type_",1:n()))

      return(settings)
    })

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

    #type_argument-----------------------
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

    #modify appearance----------------------------
    header_names <- reactive({
      req(summary_table())
      summary_table() %>%
        show_header_names() %>%
        as_tibble() %>%
        return()
    })


    modified_appearance <- reactive({
      req(summary_table())
      fin <- summary_table()

      if(!is.null(input$label)){
        fin <- fin %>%
          modify_header(label = input$label)
      }


      if(is.null(input$spanning_header) ){

      }else if(input$spanning_header == ""){

      }else{
        fin <- fin %>%
          modify_spanning_header(
            starts_with("stat_") ~ input$spanning_header
          )
      }

      if(input$bold_label){
        fin <- fin %>%
          bold_labels()
      }

      return(fin)
    })

    #generate UI for modify appearance----------------
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

    # Under construction ---------------------------
    # output$footer <- renderUI({
    #   req(header_names())
    #   hd <- header_names()
    #   length_hd <- nrow(hd)
    #
    #   if("p.value" %in% hd$column){
    #     p_label <- modified_appearance()$table_header %>%
    #       filter(column == "p.value") %>%
    #       pull(footnote)
    #
    #     show("footnote_p")
    #   }else{
    #     p_label <- ""
    #     hide("footnote_p")
    #   }
    #
    #   footer_ui <- fluidRow(
    #     column(width = 4,
    #            textInput("footnote"  ,"1) Statistics footnote", "Statistics presented: Mean(SD); n / N (%)"),
    #            textInput("footnote_p","2) P-value footnote"   , p_label)
    #     )
    #   )
    #
    # })

    #[@]Script text output------------------------
    output$script <- renderText({
      filename <- input$file$name
      #_set_by-----
      if(input$by == "NA") set_by <- "NULL" else set_by <- input$by

      #_set_label----
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

      #_type_argument-------
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

      #_add header------------

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

      #_add columns----------------------------
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

      base_text <- c(
        "#THIS TEXT IS EXPERIMENTAL AND IS UNDER DEVELOPMENT",
        "library(tidyverse)",
        "library(gtsummary)",
        "# Read data from csv------------------------------------",
        "csv_data <- read_delim('<DIR PATH>//{filename}', delim = '{input$set_csv_delimiter}')",
        "",
        "# Make summary table-----------------------------------",
        "summarised_table <- tbl_summary(",
        "  data  = csv_data,",
        "  by    = {set_by},",
        "  label = {set_label},",
        "  type  = {set_type},",
        "  statistic = list(",
        "    all_continuous() ~ '{input$statistics_continuous}',",
        "    all_categorical() ~ '{input$statistics_categorical}'",
        "  ),",
        "  digits = list(all_continuous() ~ {input$digits}),",
        "  missing_text = '{input$missing_text}'",
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


    #Output---------------------------
    output$table1 <- gt::render_gt({
      req(input$file)
      modified_appearance() %>% as_gt()
    })

    #DL button logic--------------------------------------

    output$dltable_word <- downloadHandler(
      filename = function() {"table1.docx"},
      content = function(file){
        render( system.file("extdata","word_template.rmd", package="egsummaryAddin"), output_file = file)
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
        render( system.file("extdata","html_template.rmd", package="egsummaryAddin"), output_file = file)
      }
    )
  }

  viewer <- browserViewer()
  runGadget(ui, server, viewer = viewer)

}
