ui <- dashboardPage(
  title = "Faux",
  skin = "blue",
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
    column(4,
           actionButton("new_data", "New Data"),
           downloadButton("download_data", "Download Data"),
           box(title = "Within-subject Factors",
               width = 12, collapsible = TRUE, solidHeader = TRUE,
               selectInput("within_n", "How many?", 0:5),
               uiOutput("within_levels")
           ),
           box(title = "Between-subject Factors",
               width = 12, collapsible = TRUE, solidHeader = TRUE,
               selectInput("between_n", "How many?", 0:5),
               uiOutput("between_levels")
           ),
           box(title = "Cell parameters", solidHeader = TRUE,
               collapsible = TRUE, width = 12,
               textInput("n", "n (per cell)", 100),
               textInput("mu", "mu", 0),
               textInput("sd", "sd", 1),
               textInput("r", "r", 0)
           ),
           radioButtons("empirical", "Are there parameters decribing the population or sample parameters?",
                        choices = c("population" = FALSE, "sample" = TRUE),
                        inline = TRUE),
           radioButtons("long", "Data format",
                        choices = c("wide" = FALSE, "long" = TRUE),
                        inline = TRUE)
    ),
    column(8,
           plotOutput("design_plot"),
           tableOutput("params_table"),
           DTOutput("sim_data")
    )
  )
)

server <- function(input, output, session) {
  # within_n ----
  within_ui <- eventReactive(input$within_n, {
    message("--within_n: ", input$within_n)
    wn <- as.integer(input$within_n)

    lapply(seq_len(wn), function(i) {
      id <- paste0("W", i)
      label <- sprintf("How many levels does within-subject factor %s have?", i)
      selectInput(id, label, 2:10)
    }) %>%
      tagList()
  })

  between_ui <- eventReactive(input$between_n, {
    message("--between_n: ", input$between_n)
    wn <- as.integer(input$between_n)

    lapply(seq_len(wn), function(i) {
      id <- paste0("B", i)
      label <- sprintf("How many levels does between-subject factor %s have?", i)
      selectInput(id, label, 2:10)
    }) %>%
      tagList()
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

  # within ----
  within <- reactive({
    wn <- as.integer(input$within_n)

    sapply(seq_len(wn), function(i) {
      id <- paste0("W", i)
      as.integer(input[[id]])
    })
  })

  # between ----
  between <- reactive({
    bn <- as.integer(input$between_n)

    sapply(seq_len(bn), function(i) {
      id <- paste0("B", i)
      as.integer(input[[id]])
    })
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
        long = input$long,
        dv = list(y = "value"),
        id = list(id = "id"),
        vardesc = list(),
        rep = 1,
        nested = TRUE,
        sep = "_"
      )
    }, error = function(e) {
      modalDialog(e$message, title = "Error", easyClose = TRUE) %>%
        showModal()
      return(NULL)
    })
  })

  # outputs ----
  output$within_levels <- renderUI( within_ui() )
  output$between_levels <- renderUI( between_ui() )
  output$design_plot <- renderPlot({
    req(sim_data())
    plot(sim_data())
  }, res = 96)
  output$sim_data <- renderDT({
    req(sim_data())
    sim_data()
  })
  output$params_table <- renderTable({
    req(sim_data())
    get_params(sim_data())
  })
  output$download_data <- downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      readr::write_csv(sim_data(), file)
    }
  )
}

shinyApp(ui, server)
