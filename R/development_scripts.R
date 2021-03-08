#call module_load_variable-----------------------------------
#
# library(shiny)
# source("R/module_load_variable.R")
# source("R/argument_options.R")
#
# ui <- fluidPage(
#   variableLoaderModalUI(id = "test")
# )
#
# server <- function(input,output,session){
#   res <- variableLoaderModalServer(id = "test", target_type = "data.frame")
#   observeEvent(res(),{
#     print(res())
#   })
# }
#
# shinyApp(ui,server)
