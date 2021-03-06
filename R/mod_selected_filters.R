# Module UI

#' @title   selected_filters_ui and selected_filters_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param group_inputs From shinyWidgets \code{pickerGroup} or \code{selectizeGroup}
#' @param label_none Label display if no filter is set
#' @param labels Rename filter codes to labels in sidebar
#'
#' @rdname selected_filters
#'
#' @export
#' @importFrom shiny NS tagList
selected_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("filtres"))
  )
}

# Module Server

#' @rdname selected_filters
#' @export
selected_filters_server <- function(input, output, session, group_inputs, label_none = "None", labels = c()){
  ns <- session$ns

  output$filtres <- shiny::renderUI({

    input_list <- group_inputs %>%
      shiny::reactiveValuesToList() %>%
      .[!grepl("-(selectized|reset_all)$", names(.),)] %>%
      { Filter(Negate(is.null), .) }

    if (length(input_list) == 0) {

      html <- shiny::div(shiny::HTML(label_none), style = "margin-left: 25px;")

      return(html)
    }

    names(input_list) <- names(input_list) %>%
      stringr::str_match("-([^-]+?)$") %>%
      .[, 2] %>%
      dplyr::recode(!!!labels) %>%
      paste0(" :")

    for (num in 1:length(input_list)) {
      html <- input_list[[num]]
      html <- purrr::map(html, ~ tippy::tippy(., tooltip = .))
      html <- purrr::map(html, shiny::tags$li)
      html <- shiny::tags$ul(html)
      input_list[[num]] <- shiny::tags$li(names(input_list)[num], html)
    }

    html <- shiny::tags$ul(input_list)

    return(html)

  })

}
