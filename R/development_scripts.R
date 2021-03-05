# #call module_load_variable-----------------------------------
#
# library(shiny)
# source("module_load_variable.R")
# source("argument_options.R")
#
# ui <- fluidPage(
#   variableLoaderModalUI(id = "test")
# )
#
# server <- function(input,output,session){
#   res <- variableLoaderModalServer(id = "test", target_type = "lm")
#   observeEvent(res(),{
#     print(res())
#   })
# }
#
# shinyApp(ui,server)
