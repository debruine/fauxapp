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
             "Current Random Factors",
             id = "current_random_factors_box",
             uiOutput("current_random_factors")
           ),
           ### new random factor ----
           ### new factor ----
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
           )
    ),

    ## Outputs ----
    column(7,
           DTOutput("multilevel_data")
    )
  )
)
