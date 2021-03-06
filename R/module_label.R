#' Module:UI for labelling dropdown menu
#'
#' @import shiny
#' @import shinyWidgets
#' @param id an id

labelModifyDropDownUI <- function(id){

  ns <- NS(id)

  label_modify_dropdown <- dropdownButton(
    label="Variable Label Setting",
    uiOutput(ns("edit_label")),
    circle=FALSE, status="primary", icon=icon("tag")
  )

  return(label_modify_dropdown)
}


#' Module: Server for labelling dropdown menu
#'
#' @param id an id
#' @param label_data label_data to display in the module.
#' @import shiny

labelModifyDropDownServer <- function(id, label_data){
  moduleServer(id,function(input, output, session){
    ns <- session$ns

    #Make label editor ui--------------------------------
    output$edit_label <- renderUI({
      is_data_table <- "data.frame" %in% class(label_data)
      is_model      <- any(c("lm", "glm", "coxph", "clogit", "survreg", "geeglm", "gee","glmerMod", "lmerMod") %in% class(label_data))

      if(is_data_table){
        tgtcols <- colnames(label_data)
      }else if(is_model){
        tgtcols <- get_terms(label_data)
      }

      replaced <- tgtcols %>%
        str_replace_all(.,":","___colon12345678910___")

      returning_ui <- div(
        map2(tgtcols, replaced, ~{
          acol <- .x
          aId <- str_c(ns("col_label_"), .y)
          return(textInput(inputId = aId, label = acol, value = acol))
        }),
        actionButton(ns("update_label"),"Update Label")
      )

      return(returning_ui)
    })

    #Make reactive output of editted label --------------------------------
    label_vector <- eventReactive(input$update_label, {

      label_inputs <- names(input) %>%
        enframe(name = NULL, value = "id") %>%
        mutate(value = map(id, ~{input[[.]]})) %>%
        filter(str_detect(id,"^col_label_")) %>%
        unnest(value) %>%
        mutate(id = str_remove(id, "col_label_"))

      val <- label_inputs$id %>% as.character() %>% str_replace_all(.,"___colon12345678910___",":")
      nam <- label_inputs$value

      set_label <- as.list(nam) %>% set_names(val)

      return(set_label)
    })

    #Return Module value----------------------------------
    return(label_vector)

  })
}

#' Function for obtain terms from model object
#' @param mod model object
#' @import gtsummary

get_terms <- function(mod){

  res <- unique(tbl_regression(mod)$table_body$variable)

  return(res)

}

#----------------------------------
#
# library(shiny)
# library(shinyWidgets)
# source("module_label.R")
# library(palmerpenguins)
# ppp <- penguins
# fmla_dic_multi <- "sex ~ flipper_length_mm + island + species + bill_length_mm + body_mass_g"
# model_glm <- glm(formula = fmla_dic_multi, data = ppp, family = binomial)
#
# ui <- fluidPage(
#   labelModifyDropDownUI(id = "test")
# )
#
# server <- function(input,output,session){
#   res <- labelModifyDropDownServer(id = "test", label_data = model_lmer)
#
#   observeEvent(res(),{
#
#     print(res())
#   })
# }
#
# shinyApp(ui,server)
#
# #dat <- iris
#
