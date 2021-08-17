library(shiny)
library(shinydashboard)
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


