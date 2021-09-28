factorial_tab <- tabItem(tabName = "factorial_tab",
  fluidRow(
    ## inputs ----
    column(
      5,
      ### buttons ----
      box(
        width = 12,
        actionButton("demo_data", "2W*3B Demo"),
        actionButton("demo_data2", "2W*2W*2B Demo"),
        actionButton("clear_design", "Clear Design")
      ),
      ### current factors ----
      param_box(
        "Current Factors",
        id = "current_factors_box",
        uiOutput("current_factors"),
        tags$br(),
        fluidRow(column(
          4, textInput("id_name", "ID", "id",
                       placeholder = "ID column")
        ),
        column(
          8,
          textInput("id_label", "ID Label", "id",
                    placeholder = "ID label")
        )),
        fluidRow(column(
          4, textInput("dv_name", "DV", "y",
                       placeholder = "DV column")
        ),
        column(
          8,
          textInput("dv_label", "DVLabel", "value",
                    placeholder = "DV label")
        ))
      ),
      ### new factor ----
      param_box(
        "New Factor",
        id = "new_factor_box",
        radioGroupButtons(
          "factor_type",
          choices = c("within", "between"),
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),
        fillRow(
          flex = c(4, 8),
          height = "2.5em",
          textInput("factor_name", NULL,
                    placeholder = "Factor Name"),
          textInput("factor_label", NULL,
                    placeholder = "Display Label")
        ),
        fillRow(
          flex = c(4, 8),
          height = "2.5em",
          "How many levels?",
          pickerInput("levels_n", NULL,
                      choices = 2:8, selected = 2)
        ),
        fillRow(
          flex = c(4, 8),
          height = "2em",
          tags$strong("Level Name",
                      style = "color: rgb(96, 92, 168)"),
          tags$strong("Display Label",
                      style = "color: rgb(96, 92, 168)")
        ),
        hidden(level_labels(8)),
        actionButton("add_factor", "Add Factor",
                     icon = icon("plus")),
        actionButton("delete_factor", "Delete Factor",
                     icon = icon("minus")),
        actionButton("reset_factor", "Reset",
                     icon = icon("broom"))
      ),
      ### cell parameters ----
      param_box(
        "Cell parameters",
        id = "cell_params_box",
        textInput("n", "n", 100),
        textInput("mu", "mu", 0),
        textInput("sd", "sigma", 1),
        textInput("r", "rho", 0),
        p(
          "Specify multiple values (up to the number in brackets), separated by commas. Values will be recycled if you don't specify enough, and dropped if you specify too many."
        )
      ),
      ### distributions ----
      # TODO: select cells to apply to
      param_box(
        "Distribution",
        id = "distribution_box",
        p(
          "This section is very experimental. Carefully check your data if you use this."
        ),
        pickerInput(
          "dv_distribution",
          NULL,
          choices = c("normal",
                      "truncnorm",
                      "likert"),
          selected = "normal"
        ),
        tags$div(
          id = "trunc_args",
          numericInput("trunc_min", "Minimum", value = -1),
          numericInput("trunc_max", "Maximum", value = 1),
          numericInput("trunc_mu", "Mean", value = 1),
          numericInput("trunc_sd", "SD", value = 0)
        ),
        tags$div(
          id = "likert_args",
          fillRow(
            flex = c(4, 8),
            height = "2.5em",
            "How many levels?",
            pickerInput("likert_n", NULL,
                        choices = 2:11, selected = 7)
          ),
          fillRow(
            flex = c(4, 8),
            height = "2em",
            tags$strong("Scale Point Name",
                        style = "color: rgb(96, 92, 168)"),
            tags$strong("Relative Proportion",
                        style = "color: rgb(96, 92, 168)")
          ),
          hidden(likert_labels(11))
        )
      )
    ),
    ## outputs ----
    column(
      7,
      box(width = 12,
          fluidRow(
            column(
              4,
              actionButton("simulate_data", "Simulate Data"),
              downloadButton("download_data", "Download Data")
            ),
            column(
              4,
              awesomeRadio(
                "long",
                "Data format",
                choices = c("long" = TRUE, "wide" = FALSE),
                inline = TRUE
              ),
              awesomeRadio(
                "empirical",
                "Parameters decribe the...",
                choices = c("population", "sample"),
                inline = TRUE
              )

            ),
            column(4,
                   sliderInput("round", "Round values", 0, 8, 3, 1, width = "90%"))
          )),
      tabsetPanel(
        type = "tabs",
        id = "sim_tabs",
        ### design_params_table ----
        tabPanel("Design Parameters", DTOutput("design_params_table")),
        ### design_plot ----
        tabPanel(
          "Design Plot",
          p(
            "The plot shows means and SD for the design you specified. Violin and boxplots will show the theoretical distribution."
          ),
          plotOutput("design_plot"),
          checkboxGroupButtons(
            "design_geoms",
            NULL,
            choices = c("pointrangeSD",
                        "violin",
                        "box"),
            selected = c("pointrangeSD"),
            checkIcon = list(
              yes = tags$i(class = "fa fa-check-square",
                           style = "color: rgb(96, 92, 168)"),
              no = tags$i(class = "fa fa-square-o",
                          style = "color: rgb(96, 92, 168)")
            )
          )
        ),
        ### data_params_table ----
        tabPanel("Data Parameters", DTOutput("data_params_table")),
        ### data_plot ----
        tabPanel(
          "Data Plot",
          p("The plot describes the data you simulated."),

          pickerInput(
            "palette",
            NULL,
            choices = c(
              "Dark2",
              "Spectral",
              "Accent",
              "Paired",
              "Pastel1",
              "Pastel2",
              "Set1",
              "Set2",
              "Set3",
              "Reds",
              "Oranges",
              "Greens",
              "Blues",
              "Purples",
              "Greys"
            ),
            inline = TRUE
          ),

          plotOutput("data_plot"),
          checkboxGroupButtons(
            "data_geoms",
            NULL,
            choices = c("pointrangeSD",
                        "pointrangeSE",
                        "violin",
                        "box",
                        "jitter"),
            selected = c("violin", "box"),
            checkIcon = list(
              yes = tags$i(class = "fa fa-check-square",
                           style = "color: rgb(96, 92, 168)"),
              no = tags$i(class = "fa fa-square-o",
                          style = "color: rgb(96, 92, 168)")
            )
          )
        ),
        ### sim_data ----
        tabPanel("Data", DTOutput("sim_data")),
        ### code ----
        tabPanel(
          "Code",
          p("This is still experimental and might be a little buggy."),
          verbatimTextOutput("code")
        )
      )
    )
  )
)
