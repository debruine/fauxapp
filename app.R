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
           selectInput("W1", "How many levels does within-subject factor 1 have?", 2:10),
           selectInput("B1", "How many levels does between-subject factor 1 have?", 2:10),
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

  # sim_data ----
  sim_data <- eventReactive(input$new_data, {
    print("--sim_data--")

    tryCatch( {
      sim_design(
        within = as.integer(input$W1),
        between = as.integer(input$B1),
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
}

shinyApp(ui, server)
