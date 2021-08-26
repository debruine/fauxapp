library(shiny)
library(shinydashboard)
library(shinyjs)
library(faux)
library(DT)
library(shinyWidgets)
faux_options(plot = FALSE)

#' Piped OR
#'
#' LHS if not \code{NULL}, otherwise RHS
#'
#' @param l LHS.
#' @param r RHS.
#' @return LHS if not \code{NULL}, otherwise RHS.
#' @name OR
#'
#' @export
#'
#' @examples
#' x <- list(b = 2, c = 3)
#' x$a %||% x$b %||% x$c
#' x$a %||% "default_value"
#'
`%||%` <- function(l, r) {
  if (is.null(l)) r else l
}

# boxes to hold parameters ----
param_box <- function(title, ..., collapsed = TRUE) {
  box(title = title,
      width = 12,
      collapsible = TRUE,
      collapsed = collapsed,
      solidHeader = TRUE,
      ...)
}

# make text inputs for n level labels
level_labels <- function(n = 10) {
  lapply(1:n, function(i) {
    nm <- paste0("level_name_", i)
    ph <- paste0("level_", i)
    table_label <- textInput(nm, NULL, placeholder = ph)

    nm <- paste0("level_display_", i)
    ph <- paste0("Level ", i, " Display Label")
    display_label <- textInput(nm, NULL, placeholder = ph)

    fluidRow(id = paste0("label_row_", i),
      column(4, table_label),
      column(8, display_label)
    )
  })
}


