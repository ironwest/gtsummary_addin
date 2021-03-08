#' Internal function for load variable from environment.
#' @param type Either Currently "data.frame" (for tbl_summary) only.
#' @importFrom purrr map_lgl


load_environmental_variable <- function(type="data.frame"){
  current_variable <- names(globalenv())

  return(
    current_variable[
      map_lgl(current_variable, ~{
        type %in% class(get(.))
      })
    ]
  )
}

#' UI Module to select variable from target environment.
#'
#' @param target_type Currently "data.frame" (for tbl_summary) only.
#' @importFrom stringr str_glue

variableLoaderModalUI <- function(id){
  ns <- NS(id)

  list(
    ""
  )
}

#' Server Module to select variable from target environment.
#'

variableLoaderModalServer <- function(id, target_type="data.frame"){
  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      target_variable_vector <- load_environmental_variable(type = target_type)

      if(length(target_variable_vector)==0){
        ui_select_target <- str_glue("There is no variable with class:{target_type} (Restart Add-in!)")
      }else{
        ui_select_target <- selectInput(
          inputId = ns("select_target"),
          label = str_glue("Select `{target_type}` class variable."),
          choices=target_variable_vector)
      }



      modal_ui <- modalDialog(
        easyClose = FALSE,
        ui_select_target,
        footer = actionButton(ns("set_target"), label="Select")
      )

      showModal(modal_ui)
      observeEvent(input$set_target, {
        removeModal()
      })

      return_this <- eventReactive(input$set_target, {input$select_target})

      return(return_this)
    }
  )

}