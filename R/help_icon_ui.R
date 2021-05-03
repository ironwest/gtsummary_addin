#' Add embed help link to input object
#'
#' @param id id for shiny module.
#' @param input_object object to add popover help.
#' @param link_text text to show in popover help.
#' @param link_url url for desired destination.
#' @importFrom bsplus bs_embed_popover shiny_iconlink
#' @import shiny

add_popover_help <- function(input_object, link_text, link_url){

  alink <- tags$a(href = link_url, link_text, target = "_blank")

  bsplus::shinyInput_label_embed(
    tag = input_object,
    element = bsplus::bs_embed_popover(
      tag=bsplus::shiny_iconlink(),
      title="Link to documentation",
      content= alink,
      placement="left",
      html="true"

    )
  )
}
