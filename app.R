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
             factorUI("within", "Within-subject Factors"),
             factorUI("between", "Between-subject Factors"),
             param_box("Cell parameters",
                 textInput("n", "n (per cell)", 100),
                 textInput("mu", "mu", 0),
                 textInput("sd", "sd", 1),
                 textInput("r", "r", 0)
             ),
             param_box("Other",
                       awesomeRadio("empirical", "Parameters decribe the...",
                            choices = c("population" = FALSE, "sample" = TRUE),
                            inline = TRUE),
                       awesomeRadio("long", "Data format",
                            choices = c("wide" = FALSE, "long" = TRUE),
                            inline = TRUE)
               )
      ),
      column(8,
             plotOutput("design_plot"),
             tableOutput("params_table"),
             DTOutput("sim_data")
      )
    )
  )
)

server <- function(input, output, session) {
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
  within <- factorServer("within")
  between <- factorServer("between")

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
