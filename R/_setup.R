suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(faux)
  library(DT)
  library(dplyr)
  library(shinyWidgets)
  library(glue)
})
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

`%else%` <- function(l, r) {
  if (length(l) == 0 || all(is.na(l)) || all(l == "")) r else l
}

# boxes to hold parameters
param_box <- function(title, ..., collapsed = FALSE) {
  box(title = title,
      width = 12,
      collapsible = TRUE,
      collapsed = collapsed,
      solidHeader = TRUE,
      ...)
}

# process DV and ID
name_label <- function(name, label) {
  name <- trimws(name)
  label <- trimws(label)

  if (name == "") name <- "y"
  if (label == "") label <- name
  setNames(label, name)
}

# get multiple inputs with indices
indexed_input <- function(base_name, indices, input) {
  paste0(base_name, indices) %>%
    lapply(function(x)
      input[[x]]) %>%
    trimws()
}

# replace missing values in a vector
replace_missing <- function(x, replace, missing_value = "") {
  replace <- rep(replace, length.out = length(x))
  missing <- x == missing_value
  x[missing] <- replace[missing]
  x
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

# make text inputs for n fixed level labels
fixed_level_labels <- function(n = 8) {
  lapply(1:n, function(i) {
    nm <- paste0("fixed_level_name_", i)
    ph <- paste0("level", i)
    table_label <- textInput(nm, NULL, placeholder = ph)

    nm <- paste0("fixed_level_code_", i)
    coding <- textInput(nm, NULL, value = "")

    nm <- paste0("fixed_level_prob_", i)
    prob <- numericInput(nm, NULL, value = 1, min = 0)

    fillRow(flex = c(4, 6, 2), height = "2.5em",
            id = paste0("fixed_label_row_", i),
            table_label,
            coding,
            prob
    )
  })
}

# make text inputs for n likert labels
likert_labels <- function(n = 8) {
  lapply(1:n, function(i) {
    nm <- paste0("likert_name_", i)
    table_label <- textInput(nm, NULL, value = i)

    nm <- paste0("likert_prob_", i)
    display_label <- numericInput(nm, NULL, value = 1, min = 0)

    fillRow(flex = c(6, 6), height = "2.5em",
            id = paste0("likert_row_", i),
            i, # table_label,
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

  rep_len(vals, len) %>% as.list()
}

# options for DT tables with less stuff around them
dt_opts <- function() {
  list(
    info = FALSE,
    lengthChange = FALSE,
    paging = FALSE,
    ordering = FALSE,
    searching = FALSE,
    pageLength = 500,
    keys = TRUE
  )
}

# display debugging messages in R (if local)
# and in the console log (if running in shiny)
message <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) base::message(crayon::green(txt))
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}

# make a container with a header above the correlations
ctnr <- function(param_table, n_factors) {
  param_cols <- names(param_table)
  factor_cols <- param_cols[2:(1+n_factors)]
  cor_cols <- param_cols[(2+n_factors):(ncol(param_table) - 2)]

  htmltools::withTags(table(
    thead(
      tr(
        th(rowspan = 2, 'n'),
        lapply(factor_cols, th, rowspan = 2),
        th(colspan = length(cor_cols), 'correlations'),
        th(rowspan = 2, 'mean'),
        th(rowspan = 2, 'sd')
      ),
      tr(
        lapply(cor_cols, th)
      )
    )
  ))
}

