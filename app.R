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
               actionButton("new_data", "New Data"),
               downloadButton("download_data", "Download Data")
             ),
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
                                    icon = icon("minus"))
             ),
             #factorUI("within", "Within-subject Factors"),
             #factorUI("between", "Between-subject Factors"),
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
             plotOutput("design_plot"),
             tableOutput("params_table"),
             hidden(
               awesomeRadio("long", "Data format",
                            choices = c("wide" = FALSE, "long" = TRUE),
                            inline = TRUE)
             ),
             DTOutput("sim_data")
      )
    )
  )
)

server <- function(input, output, session) {
  within <- reactiveVal(list())
  between <- reactiveVal(list())

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

  # add_factor ----
  observeEvent(input$add_factor, {
    # get list of level names and display
    level_names <- paste0("level_name_", 1:input$levels_n) %>%
      lapply(function(x) input[[x]])
    level_displays <- paste0("level_display_", 1:input$levels_n) %>%
      lapply(function(x) input[[x]])

    # update within/between
    if (input$factor_type == "within") {
      old_factors <- within()
    } else {
      old_factors <- between()
    }
    old_factors[[input$factor_name]] <- setNames(level_displays, level_names)

    if (input$factor_type == "within") {
      within(old_factors)
    } else {
      between(old_factors)
    }

    # reset factor box
    c("factor_name", "factor_display",
      paste0("level_name_", 1:10),
      paste0("level_display_", 1:10)) %>%
        lapply(shinyjs::reset)
  })

  # sim_data ----
  sim_data <- eventReactive(input$new_data, {
    print("--sim_data--")

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
  output$design_plot <- renderPlot({
    req(sim_data())
    plot(sim_data())
  }, res = 96)
  output$sim_data <- renderDT({
    req(display_data())

    display_data()


  })
  output$params_table <- renderTable({
    req(sim_data())
    show("long")
    get_params(sim_data())
  })
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
