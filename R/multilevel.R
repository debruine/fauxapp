multilevel_tab <- tabItem(tabName = "multilevel_tab",
  fluidRow(
    ##inputs----
    column(5,
           ### buttons ----
           box(
             width = 12,
             actionButton("demo_mixed", "Demo"),
             actionButton("clear_mixed_design", "Clear Design")
           ),
           ### current random factors ----
           param_box(
             "Current Factors",
             id = "current_random_factors_box",
             uiOutput("current_random_factors"),
             uiOutput("current_fixed_factors")
           ),
           ### new random factor ----
           param_box(
             "New Random Factor",
             id = "new_random_factor_box",
             hidden(radioGroupButtons(
               "random_factor_type",
               choices = c("crossed", "nested"),
               checkIcon = list(yes = icon("ok", lib = "glyphicon"))
             )),
             hidden(pickerInput(inputId = "random_factor_nested_in",
                         label = "Nested in",
                         choices = c(""))),
             textInput(inputId = "random_factor_name", label = "Name"),
             textInput(inputId = "random_factor_n", label = "How many?"),
             actionButton("add_random_factor", "Add Factor",
                          icon = icon("plus")),
             actionButton("delete_random_factor", "Delete Factor",
                          icon = icon("minus")),
             actionButton("reset_random_factor", "Reset",
                          icon = icon("broom"))
           ),
           ### new fixed factor ----
           param_box(
             "New Fixed Factor",
             id = "new_fixed_factor_box",
             radioGroupButtons(
               "fixed_factor_type",
               choices = c("within", "between"),
               checkIcon = list(yes = icon("ok", lib = "glyphicon"))
             ),
             radioGroupButtons(
               "fixed_factor_shuffle",
               label = "Shuffle order",
               choices = c("no", "yes"),
               checkIcon = list(yes = icon("ok", lib = "glyphicon"))
             ),
             hidden(pickerInput(inputId = "fixed_factor_by",
                                label = "by",
                                choices = c(""))),
             textInput(inputId = "fixed_factor_name", label = "Name"),

             fillRow(
               flex = c(4, 8),
               height = "2.5em",
               "How many levels?",
               pickerInput("fixed_levels_n", NULL,
                           choices = 2:8, selected = 2)
             ),

             fillRow(
               flex = c(8, 4),
               height = "2em",
               tags$strong("Level Name",
                           style = "color: rgb(96, 92, 168)"),
               tags$strong("Probability",
                           id = "probability_header",
                           style = "color: rgb(96, 92, 168)")
             ),

             hidden(fixed_level_labels(8)),

             actionButton("add_fixed_factor", "Add Factor",
                          icon = icon("plus")),
             actionButton("delete_fixed_factor", "Delete Factor",
                          icon = icon("minus")),
             actionButton("reset_fixed_factor", "Reset",
                          icon = icon("broom"))
           )
    ),

    ## Outputs ----
    column(7,
           DTOutput("multilevel_data")
    )
  )
)
