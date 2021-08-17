factorUI <- function(id, title) {
  ns <- NS(id)

  param_box(title,
            selectInput(ns("factor_n"), "How many?", 0:5),
            uiOutput(ns("levels"))
  )
}

factorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # update the UI to add a level select for each factor
    level_ui <- reactive({
      message("--", id, "-factor_n: ", input$factor_n)
      n <- as.integer(input$factor_n)

      lapply(seq_len(n), function(i) {
        factor_i <- paste0("factor", i)
        factor_val <- input[[factor_i]] %||% 2
        name_i <- paste0("name", i)
        default_name <- substr(id, 1, 1) %>%
          toupper() %>% paste(i)
        name_val <- input[[name_i]] %||% default_name

        level_name_inputs <- lapply(1:factor_val, function(j) {
          level_j <- paste0(factor_i, "_", j)
          default_level_val <- paste0(name_val, ".", letters[j])
          level_val <- input[[level_j]] %||% default_level_val
          textInput(inputId = NS(id, level_j),
                    label = NULL,
                    value = level_val)
        })


        tagList(
          h4("Factor", i),
          textInput(inputId = NS(id, name_i),
                    label = NULL,
                    placeholder = "Factor Name",
                    value = name_val),
          selectInput(inputId = NS(id, factor_i),
                      label = "Levels",
                      choices = 2:10,
                      selected = factor_val),
          level_name_inputs
        )
      })
    })

    # outputs
    output$levels <- renderUI( level_ui() )

    # output values of the factor levels
    reactive({
      message("--", id, "--")
      n <- as.integer(input$factor_n)

      levels <- lapply(seq_len(n), function(i) {
        factor_id <- paste0("factor", i)
        name_id <- paste0("name", i)
        factor_name <- input[[name_id]]

        # get level names
        level_j <- 1:as.integer(input[[factor_id]])
        lapply(level_j, function(j) {
          level_name_id <- paste0(factor_id, "_", j)
          input[[level_name_id]]
        })
      })

      names(levels) <- sapply(seq_len(n), function(i) {
        name_id <- paste0("name", i)
        input[[name_id]]
      })

      levels
    })
  })
}
