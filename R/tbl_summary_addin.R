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
#' @importFrom bsplus bs_embed_popover shiny_iconlink use_bs_popover
#' @import gtsummary
#' @import miniUI
#' @import shiny
#' @import dplyr
#' @import shinyWidgets
#' @import rclipboard

tbl_summary_addin <- function(){
  #call module------------------------
  variable_loader_modal_ui <- variableLoaderModalUI(id = "load_variable_name")

  # > Dropdown buttons---------------------------
  dropdown_modify_label <- dropdownButton(
    label="Variable Label Setting",
    uiOutput("edit_label"),
    circle=FALSE, status="primary", icon=icon("tag")
  )

  dropdown_header_setting <- dropdownButton(
    label="Table Header Setting",
    uiOutput("header"),
    prettyCheckbox("bold_label", "Bold Label"),
    circle=FALSE, status="primary", icon=icon("heading")
  )

  dropdown_set_column_type <- dropdownButton(
    label="Variable Type Setting",
    uiOutput("set_column_type"),
    circle=FALSE, status="primary", icon=icon("cat"), width=12
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
    tags$style(type="text/css",css_bigfont("decimal_mark") ),
    tags$style(type="text/css",css_bigfont("big_mark") ),
    tags$style(type="text/css",css_bigfont("iqr_sep") ),
    tags$style(type="text/css",css_bigfont("ci_sep") ),
    useShinyjs(),
    use_bs_popover(),
    rclipboardSetup(),
    titlePanel("Interactive tbl_summary"),
    sidebarLayout(
      sidebarPanel(
        tbl_summary_addin_sidebar_ui(),
        fluidRow( dlbutton_excel, dlbutton_csv, dlbutton_html ),
        fluidRow( button_copy_script )
      ),

      mainPanel(
        fluidRow(
          column(width = 2 ,
                 tbl_summary_addin_dropdown_ui_add_column(), hr(),
                 dropdown_modify_label   , hr(),
                 dropdown_set_column_type, hr(),
                 dropdown_header_setting , hr(),
                 tbl_summary_addin_dropdown_ui_theme(), hr()),
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

    #stop app when browser closed-------------------
    session$onSessionEnded(function() {stopApp()})

    # Hide all UI ----------------------------------
    hide_these_on_load <- c("dltable_word","dltable_csv","var","by","percent","missing","add_p_condition","add_p_categorical","add_p_continuous","statistics_categorical","statistics_continuous","digits","missing_text","add_overall_condition","add_overall_last","add_overall_label","drop_down_add_column","drop_down_modify_label","drop_down_header_setting","drop_down_set_column_type","dropdown_theme_setting","dltable_html","bold_label","footnote_p")
    map(hide_these_on_load, hide)

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

      column_names <- dat() %>% colnames()

      # select variable upto first 5 (for large dataset)
      if(length(column_names) >= 5){
        select_these <- column_names[1:5]
      }else{
        select_these <- column_names
      }

      updatePickerInput(
        session  = session,
        inputId  = "var",
        choices  = column_names,
        selected = select_these
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
      show_these_when_data_selected <- {
        c("dltable_word","dltable_csv","dltable_html","var","by","statistics_categorical","statistics_continuous","digits","missing","missing_text","drop_down_add_column","drop_down_modify_label","drop_down_header_setting","drop_down_set_column_type","dropdown_theme_setting","bold_label","footnote","percent")
      }
      map(show_these_when_data_selected,show)
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
