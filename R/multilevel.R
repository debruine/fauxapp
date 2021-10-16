multilevel_tab <- tabItem(tabName = "multilevel_tab",
  fluidRow(
    ##inputs----
    column(6,
           ### buttons ----
           box(
             width = 12,
             actionButton("demo_mixed", "Demo"),
             actionButton("clear_mixed_design", "Clear Design"),
             downloadButton("download_ml_data", "Download Data")
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
           )
    ),
    column(6,
           ### new fixed factor ----
           param_box(
             "New Fixed Factor",
             id = "new_fixed_factor_box",
             fluidRow(
               column(4, radioGroupButtons(
                 "fixed_factor_type",
                 choices = c("within", "between"),
                 checkIcon = list(yes = icon("ok", lib = "glyphicon"))
               )),
               column(4, hidden(pickerInput(inputId = "fixed_factor_by",
                                         label = NULL,
                                         choices = c("")))),
               column(4, hidden(
                 switchInput(
                   inputId = "fixed_factor_shuffle",
                   label = "shuffle",
                   labelWidth = "80px"
                 )
               ))
              ),
             textInput(inputId = "fixed_factor_name", label = "Name"),

             pickerInput(inputId = "fixed_factor_coding",
                         label = "Contrast Coding",
                         choices = c("anova", "sum", "treatment", "helmert", "poly", "difference"),
                         selected = "anova"),

             fillRow(
               flex = c(4, 8),
               height = "2.5em",
               "How many levels?",
               pickerInput("fixed_levels_n", NULL,
                           choices = 2:8, selected = 2)
             ),

             fillRow(
               flex = c(4, 6, 2),
               height = "2em",
               tags$strong("Level Name",
                           style = "color: rgb(96, 92, 168)"),
               tags$strong("Coding",
                           style = "color: rgb(96, 92, 168)"),
               tags$strong("Proportion",
                           id = "prob_header",
                           style = "color: rgb(96, 92, 168)",
                           title = "Relative probability of the levels. Sampled unless the total is equal to the N.")
             ),

             hidden(fixed_level_labels(8)),

             actionButton("add_fixed_factor", "Add Factor",
                          icon = icon("plus")),
             actionButton("delete_fixed_factor", "Delete Factor",
                          icon = icon("minus")),
             actionButton("reset_fixed_factor", "Reset",
                          icon = icon("broom"))
           )
    )
  ),

  ## Outputs ----
  DTOutput("multilevel_data")
)
