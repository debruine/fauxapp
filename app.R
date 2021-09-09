# ui ----
ui <- dashboardPage(
  title = "Faux",
  skin = "purple",
  dashboardHeader(title = "Faux"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      # links to files in www/
      tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    fluidRow(
      column(4,
             box(width = 12,
               actionButton("demo_data", "Demo Data"),
               actionButton("new_data", "New Data"),
               actionButton("clear_design", "Clear Design"),
               downloadButton("download_data", "Download Data")
             ),
             ## current factors ----
             param_box("Current Factors", collapsed = FALSE,
                       uiOutput("current_factors")
             ),
             ## new factor ----
             param_box("New Factor", collapsed = FALSE,
                       radioGroupButtons("factor_type",
                         choices = c("within", "between"),
                         checkIcon = list(
                           yes = icon("ok", lib = "glyphicon")
                         )
                       ),
                       fluidRow(
                         column(4, textInput("factor_name", NULL,
                                 placeholder = "name")),
                         column(8, textInput("factor_display", NULL,
                                 placeholder = "Display Name"))
                       ),
                       pickerInput("levels_n", "How many levels?",
                                   choices = 2:10, selected = 2),
                       hidden(level_labels(10)),
                       actionButton("add_factor", "Add Factor",
                                    icon = icon("plus")),
                       actionButton("delete_factor", "Delete Factor",
                                    icon = icon("minus")),
                       actionButton("reset_factor", "Reset",
                                    icon = icon("broom"))
             ),
             # cell parameters ----
             param_box("Cell parameters",
                 textInput("n", "n (per cell)", 100),
                 textInput("mu", "mu", 0),
                 textInput("sd", "sd", 1),
                 textInput("r", "r", 0)
             ),
             param_box("Other",
                       awesomeRadio("empirical", "Parameters decribe the...",
                            choices = c("population" = FALSE, "sample" = TRUE),
                            inline = TRUE)
               )
      ),
      column(8,
             tabsetPanel(type = "tabs",
                         tabPanel("Design Parameters", DTOutput("design_params_table")),
                         tabPanel("Plot", plotOutput("design_plot")),
                         tabPanel("Simulated Parameters", DTOutput("data_params_table")),
                         tabPanel("Data",
                                  hidden(awesomeRadio("long", "Data format",
                                                      choices = c("wide" = FALSE, "long" = TRUE),
                                                      inline = TRUE)),
                                  DTOutput("sim_data"))
             )
      )
    )
  )
)

server <- function(input, output, session) {
  within <- reactiveVal(list())
  between <- reactiveVal(list())

  # demo_data ----
  observeEvent(input$demo_data, {
    w <- list(time = c(am = "Morning", pm = "Evening"),
              condition = c(A = "A", B = "B"))
    b <- list(pets = c(cat = "Kittens", dog = "Puppies", ferret = "Slinkies"))
    within(w)
    between(b)

    updateTextInput(session, "n", value = "30")
    updateTextInput(session, "mu", value = "0")
    updateTextInput(session, "sd", value = "1")
    updateTextInput(session, "r", value = "0.5")
    updateAwesomeRadio(session, "empirical", selected = "sample")
  })

  # parameters ----
  n <- reactive({
    strsplit(input$n, "\\s*,\\s*")[[1]] %>% as.numeric()
  })

  mu <- reactive({
    strsplit(input$mu, "\\s*,\\s*")[[1]] %>% as.numeric()
  })

  sd <- reactive({
    strsplit(input$sd, "\\s*,\\s*")[[1]] %>% as.numeric()
  })

  r <- reactive({
    strsplit(input$r, "\\s*,\\s*")[[1]] %>% as.numeric()
  })

  # factors ----
  #within <- factorServer("within")
  #between <- factorServer("between")

  # levels_n ----
  observeEvent(input$levels_n, {
    to_show <- paste0("label_row_", 1:input$levels_n)
    h <- setdiff(1:10, 1:input$levels_n)
    to_hide <- paste0("label_row_", h)

    lapply(to_show, show)
    lapply(to_hide, hide)
  })

  # edit_factor ----
  observeEvent(input$edit_factor, {
    factors <- c(within(), between())
    levels <- factors[[input$edit_factor]]

    # update new factor box inputs
    factor_type <- if (input$edit_factor %in% names(within())) "within" else "between"
    updateRadioGroupButtons(session, "factor_type", selected = factor_type)
    updateTextInput(session, "factor_name", value = input$edit_factor)
    n <- length(levels)
    updatePickerInput(session, "levels_n", selected = n)

    for (i in 1:n) {
      updateTextInput(session, paste0("level_name_", i), value = names(levels)[[i]])
      updateTextInput(session, paste0("level_display_", i), value = levels[[i]])
    }

    updateTextInput(session, "edit_factor", value = NULL)
  })

  # add_factor ----
  observeEvent(input$add_factor, {
    # check factor_name is set
    factor_name <- trimws(input$factor_name)
    if (factor_name == "") return()

    # get list of level names and display
    level_names <- paste0("level_name_", 1:input$levels_n) %>%
      lapply(function(x) input[[x]]) %>%
      trimws()
    level_displays <- paste0("level_display_", 1:input$levels_n) %>%
      lapply(function(x) input[[x]]) %>%
      trimws()

    # quit if any level names are missing
    if (any(level_names == "")) return()

    # replace missing level display names with level names
    level_displays <- ifelse(level_displays == "", level_names, level_displays)

    # remove any old factor with the same name
    w <- within()
    b <- between()
    w[[factor_name]] <- NULL
    b[[factor_name]] <- NULL
    # update within/between
    if (input$factor_type == "within") {
      w[[factor_name]] <- setNames(level_displays, level_names)
    } else {
      b[[factor_name]] <- setNames(level_displays, level_names)
    }

    within(w)
    between(b)

    reset_factor_box()
  })

  # reset_factor ----
  observeEvent(input$reset_factor, {
    reset_factor_box()
  })

  reset_factor_box <- function() {
    # reset factor box
    c("factor_name", "factor_display",
      paste0("level_name_", 1:10),
      paste0("level_display_", 1:10)) %>%
      lapply(shinyjs::reset)
  }

  # delete_factor ----
  observeEvent(input$delete_factor, {
    # remove any old factor with the same name
    w <- within()
    b <- between()
    w[[input$factor_name]] <- NULL
    b[[input$factor_name]] <- NULL
    within(w)
    between(b)
  })

  # sim_data ----
  sim_data <- eventReactive(input$new_data, {
    print("--sim_data--")

    show("long") # only show the long button after there is data to reshape

    tryCatch( {
      sim_design(
        within = within(),
        between = between(),
        n = n(),
        mu = mu(),
        sd = sd(),
        r = r(),
        empirical = input$empirical,
        long = FALSE,
        dv = list(y = "value"),
        id = list(id = "id"),
        # vardesc = list(),
        rep = 1,
        # nested = TRUE,
        sep = "_"
      )
    }, error = function(e) {
      modalDialog(e$message, title = "Error", easyClose = TRUE) %>%
        showModal()
      return(NULL)
    })
  })

  # display_data ----
  # TODO: Fix this :(
  display_data <- reactive({
    within_factor_n <- length(within())

    # TODO: cancel function if within_factor_n == 0
    # so table doesn't flash update
    if (isTRUE(input$long) && within_factor_n>0) {
      wide2long(sim_data())
    } else {
      sim_data()
    }
  })

  # outputs ----
  ## design_plot ----
  output$design_plot <- renderPlot({
    req(sim_data())
    plot(sim_data())
  }, res = 96)

  ## sim_data ----
  output$sim_data <- renderDT({
    req(display_data())
    display_data()
  })

  ## data_params_table ----
  data_params_table <- reactiveVal()

  observeEvent(sim_data(), {
    message("update data_params_table")

    within_factors <- names(within())

    new_table <- get_params(sim_data()) %>%
      tidyr::separate(var, into = within_factors, sep = "_") %>%
      dplyr::select(n, everything())

    data_params_table(new_table)
  })

  output$data_params_table <- renderDT({
    data_params_table()
  }, rownames = FALSE,
  select = "none",
  options = list(
    info = FALSE,
    lengthChange = FALSE,
    paging = FALSE,
    ordering = FALSE,
    searching = FALSE,
    pageLength = 500,
    keys = TRUE
  ))

  ## design_params_table ----
  design_params_table <- reactiveVal()

  observeEvent(sim_data(), {
    message("design_params_table updated by sim_data")

    params <- attr(sim_data(), "design")$params

    # update parameter boxes
    updateTextInput(session, "mu", value = paste(params$mu, collapse = ", "))
    updateTextInput(session, "sd", value = paste(params$sd, collapse = ", "))

    new_design_table <- dplyr::select(params, n, everything())
    design_params_table(new_design_table)
  })

  output$design_params_table <- renderDT({
    design_params_table()
  }, rownames = FALSE,
    selection = list(mode = "single", target = "cell"),
    editable = TRUE,
    options = list(
      info = FALSE,
      lengthChange = FALSE,
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE,
      pageLength = 500,
      keys = TRUE
    ))

  ## design_params_table edit ----
  observeEvent(input$design_params_table_cell_edit, {
    cell <- input$design_params_table_cell_edit

    col_name <- names(design_params_table())[[cell$col+1]]
    old_cell <- design_params_table()[cell$row, cell$col+1]

    if (col_name %in% c("mu", "sd")) {
      old_table <- design_params_table()
      old_table[cell$row, cell$col+1] <- cell$value
      message("design_params_table updated by edit")
      design_params_table(old_table)
    }

    # update boxes
    updateTextInput(session, "mu", value = paste(design_params_table()$mu, collapse = ", "))
    updateTextInput(session, "sd", value = paste(design_params_table()$sd, collapse = ", "))

    # invalidate data until next simulation???

    # check OK
    new_cell <- design_params_table()[cell$row, cell$col+1]
    print(glue::glue("{old_cell} -> {new_cell}"))
  })

  # current_factors ----
  output$current_factors <- renderUI({ message("update current factors")
    factor_names <- c(names(within()), names(between()))

    lapply(factor_names, tags$button)
  })

  # download_data ----
  output$download_data <- downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      readr::write_csv(display_data(), file)
    }
  )
}

shinyApp(ui, server)
