library(shiny)
library(shinydashboard)
library(shinyjs)
library(faux)
library(DT)
library(dplyr)
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
param_box <- function(title, ..., collapsed = FALSE) {
  box(title = title,
      width = 12,
      collapsible = TRUE,
      collapsed = collapsed,
      solidHeader = TRUE,
      ...)
}

# make text inputs for n level labels
level_labels <- function(n = 8) {
  lapply(1:n, function(i) {
    nm <- paste0("level_name_", i)
    ph <- paste0("level", i)
    table_label <- textInput(nm, NULL, placeholder = ph)

    nm <- paste0("level_display_", i)
    ph <- paste0("Level ", i, " Display Label")
    display_label <- textInput(nm, NULL, placeholder = ph)

    fillRow(flex = c(4, 8), height = "2.5em",
            id = paste0("label_row_", i),
            table_label,
            display_label
    )
  })
}

## parse text input for parameters
parse_param <- function(param, len) {
  if (grepl("\\d+:", param)) {
    range <- strsplit(param, ":")[[1]] %>%
      as.numeric() %>% na.omit()
    if (length(range) == 1) range[[2]] <- range[[1]]
    vals <- seq(range[[1]], range[[2]], length.out = len)
  } else {
    vals <- strsplit(param, "\\s*,\\s*")[[1]] %>%
      as.numeric() %>% na.omit()
  }
  param_vals <- rep_len(vals, len) %>% as.list()
  param_vals
}

dt_opts <- list(
  info = FALSE,
  lengthChange = FALSE,
  paging = FALSE,
  ordering = FALSE,
  searching = FALSE,
  pageLength = 500,
  keys = TRUE
)

# display debugging messages in R (if local)
# and in the console log (if running in shiny)
message <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) base::message(crayon::green(txt))
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}


