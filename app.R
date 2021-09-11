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
      ## inputs ----
      column(4,
             ### buttons ----
             box(width = 12,
               actionButton("demo_data", "2W*3B Demo"),
               actionButton("demo_data2", "2W*2W*2B Demo"),
               actionButton("clear_design", "Clear Design")
             ),
             ### current factors ----
             param_box("Current Factors", id = "current_factors_box",
                       uiOutput("current_factors"),
                       tags$br(),
                       fluidRow(column(4, textInput("id_name", "ID", "id",
                                                    placeholder="ID column")),
                                column(8, textInput("id_label", "Label", "id",
                                                    placeholder = "ID label"))),
                       fluidRow(column(4, textInput("dv_name", "DV", "y",
                                                    placeholder="DV column")),
                                column(8, textInput("dv_label", "Label", "value",
                                                    placeholder="DV label")))

             ),
             ### new factor ----
             param_box("New Factor", id = "new_factor_box",
                       radioGroupButtons("factor_type",
                         choices = c("within", "between"),
                         checkIcon = list(
                           yes = icon("ok", lib = "glyphicon")
                         )
                       ),
                       fillRow(flex = c(4, 8), height = "2.5em",
                         textInput("factor_name", NULL,
                                 placeholder = "name"),
                         textInput("factor_display", NULL,
                                 placeholder = "Display Name")
                       ),
                       pickerInput("levels_n", "How many levels?",
                                   choices = 2:8, selected = 2),
                       hidden(level_labels(8)),
                       actionButton("add_factor", "Add Factor",
                                    icon = icon("plus")),
                       actionButton("delete_factor", "Delete Factor",
                                    icon = icon("minus")),
                       actionButton("reset_factor", "Reset",
                                    icon = icon("broom"))
             ),
             ### cell parameters ----
             param_box("Cell parameters", id = "cell_params_box",
                       awesomeRadio("empirical", "Parameters decribe the...",
                                    choices = c("population", "sample"),
                                    inline = TRUE),
                       textInput("n", "n (per cell)", 100),
                       textInput("mu", "mu", 0),
                       textInput("sd", "sd", 1),
                       textInput("r", "r", 0)
             )
      ),
      ## outputs ----
      column(8,
             box(width = 12,
                 fluidRow(
                   column(6,
                 actionButton("simulate_data", "Simulate Data"),
                 downloadButton("download_data", "Download Data")),
column(6,
                 awesomeRadio("long", "Data format",
                              choices = c("long" = TRUE, "wide" = FALSE),
                              inline = TRUE))
             )),
             tabsetPanel(type = "tabs", id="sim_tabs",
                         ### design_params_table ----
                         tabPanel("Design Parameters", DTOutput("design_params_table")),
                         ### design_plot ----
                         tabPanel("Design Plot",
                                  p("The plot show means and SD for the design you specified."),
                                  plotOutput("design_plot")),
                         ### data_params_table ----
                         tabPanel("Data Parameters", DTOutput("data_params_table")),
                         ### data_plot ----
                         tabPanel("Data Plot",
                                  p("The plot describes the data you simulated."),

                                  pickerInput("palette", NULL,
                                              choices= c("Dark2", "Spectral", "Accent", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3", "Reds", "Oranges", "Greens", "Blues", "Purples", "Greys"),
                                              inline = TRUE),

                                  plotOutput("data_plot"),
                                  awesomeCheckboxGroup("geoms", NULL,
                                                       choices = c("pointrangeSD",
                                                                   "pointrangeSE",
                                                                   "violin",
                                                                   "box",
                                                                   "jitter"),
                                                       selected = c("violin", "box"),
                                                       inline = TRUE)
                                  ),
                         ### sim_data ----
                         tabPanel("Data", DTOutput("sim_data"))
             )
      )
    )
  )
)

# server ----
server <- function(input, output, session) {
  # reactiveVals ----
  within <- reactiveVal(list())
  between <- reactiveVal(list())
  design <- reactiveVal()
  sim_data <- reactiveVal()
  data_params_table <- reactiveVal()
  design_params_table <- reactiveVal()

  # demo_data ----
  observeEvent(input$demo_data, {
    w <- list(time = c(am = "Day", pm = "Night"))
    b <- list(pets = c(cat = "Kittens", dog = "Puppies", ferret = "Slinkies"))
    within(w)
    between(b)

    updateTextInput(session, "id_name", value = "id")
    updateTextInput(session, "id_label", value = "Pet ID")
    updateTextInput(session, "dv_name", value = "score")
    updateTextInput(session, "dv_label", value = "Sleepiness Score")

    updateTextInput(session, "n", value = "30, 20, 10")
    updateTextInput(session, "mu", value = "8, 10, 4, 10, 10, 4")
    updateTextInput(session, "sd", value = "1")
    updateTextInput(session, "r", value = "0.5")
    updateAwesomeRadio(session, "empirical", selected = "population")

    runjs("closeBox('new_factor_box')")
    runjs("openBox('cell_params_box')")
  })

  # demo_data2 ----
  observeEvent(input$demo_data2, {
    w <- list(version = c(V1 = "Version 1", V2 = "Version 2"),
              condition = c(A = "A", B = "B"))
    b <- list(age_group = c(young = "Age 20-29", old = "Age 70-79"))
    within(w)
    between(b)

    updateTextInput(session, "id_name", value = "id")
    updateTextInput(session, "id_label", value = "Subject ID")
    updateTextInput(session, "dv_name", value = "score")
    updateTextInput(session, "dv_label", value = "Score")

    updateTextInput(session, "n", value = "30")
    updateTextInput(session, "mu", value = "100, 100, 100, 100, 100, 90, 110, 110, 90")
    updateTextInput(session, "sd", value = "20")
    updateTextInput(session, "r", value = "0.5")
    updateAwesomeRadio(session, "empirical", selected = "population")

    runjs("closeBox('new_factor_box')")
    runjs("openBox('cell_params_box')")
  })

  # parameters ----
  ## bcells ----
  bcells <- reactive(
    if (length(between()) == 0) {
      1
    } else {
      sapply(between(), length) %>% prod()
    }
  )

  ## wcells ----
  wcells <- reactive(
    if (length(within()) == 0) {
      1
    } else {
      sapply(within(), length) %>% prod()
    }
  )

  ## dv ----
  dv <- reactive({
    name <- trimws(input$dv_name)
    label <- trimws(input$dv_label)

    if (name == "") name <- "y"
    if (label == "") label <- name
    setNames(label, name)
  })

  ## id ----
  id <- reactive({
    name <- trimws(input$id_name)
    label <- trimws(input$id_label)

    if (name == "") name <- "id"
    if (label == "") label <- name
    setNames(label, name)
  })

  ## n ----
  n <- reactive({
    parse_param(input$n, bcells())
  })

  ## mu ----
  mu <- reactive({
    parse_param(input$mu, bcells()*wcells())
  })

  ## sd ----
  sd <- reactive({
    parse_param(input$sd, bcells()*wcells())
  })

  ## r ----
  r <- reactive({
    len <- wcells()*((wcells()-1)/2)
    parse_param(input$r, len) %>% unlist()
  })

  # factors ----

  ## levels_n ----
  observeEvent(input$levels_n, {
    to_show <- paste0("label_row_", 1:input$levels_n)
    h <- setdiff(1:10, 1:input$levels_n)
    to_hide <- paste0("label_row_", h)

    lapply(to_show, show)
    lapply(to_hide, hide)
  })

  ## edit_factor ----
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
    runjs("openBox('new_factor_box')")
  })

  ## add_factor ----
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

    # auto-fill missing level names
    level_names[level_names == ""] <- paste0("level", which(level_names== ""))

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

  ## reset_factor ----
  observeEvent(input$reset_factor, {
    reset_factor_box()
  })

  reset_factor_box <- function() {
    c("factor_name", "factor_display",
      paste0("level_name_", 1:10),
      paste0("level_display_", 1:10)) %>%
      lapply(shinyjs::reset)
  }

  ## delete_factor ----
  observeEvent(input$delete_factor, {
    # remove any old factor with the same name
    w <- within()
    b <- between()
    w[[input$factor_name]] <- NULL
    b[[input$factor_name]] <- NULL
    within(w)
    between(b)
  })

  # design/data ----
  ## clear_design ----
  observeEvent(input$clear_design, {
    c("id_name", "id_label", "dv_name", "dv_label",
      "n", "mu","sd", "r", "empirical") %>%
      lapply(reset)

    within(list())
    between(list())
    reset_factor_box()
  })

  ## design ----
  observe({
    message("--design--")

    # check names
    col_names <- c(
      names(within()),
      names(between()),
      names(dv()),
      names(id())
    )

    has_dupe_names <- duplicated(col_names) %>% any()
    if (has_dupe_names) {
      modalDialog("", title = "Duplicate column names", easyClose = TRUE) %>%
        showModal()
      return(NULL)
    }

    new_design <- tryCatch( {
      faux:::check_design(
        within = within(),
        between = between(),
        n = n(),
        mu = mu(),
        sd = sd(),
        r = r(),
        dv = dv(),
        id = id(),
        # vardesc = list(),
        sep = "_"
      )
    }, error = function(e) {
      message(e$message)
      modalDialog(e$message, title = "Error", easyClose = TRUE) %>%
        showModal()
      return(NULL)
    })

    design(new_design)
  })

  observeEvent(design(), {
    # only enable after there is a valid design
    sim_data(NULL)
    valid_design <- !is.null(design())
    toggleState("simulate_data", valid_design)

    showTab(inputId = "sim_tabs", target = "Design Parameters")
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  ## simulate_data ----
  observeEvent(input$simulate_data, {
    message("--simulate_data--")

    new_sim_data <- tryCatch( {
      sim_design(
        design = design(),
        empirical = input$empirical == "sample",
        long = FALSE,
        rep = 1,
        # nested = TRUE
      )
    }, error = function(e) {
      modalDialog(e$message, title = "Error", easyClose = TRUE) %>%
        showModal()
      return(NULL)
    })

    sim_data(new_sim_data)
  })

  ## sim_data update ----
  observeEvent(sim_data(), {
    message("--sim_data--")
    # only enable after there is data to reshape
    valid_data <- !is.null(sim_data())
    toggleState("long", valid_data)
    toggleState("download_data", valid_data)

    showTab(inputId = "sim_tabs", target = "Data Plot")
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  ## display_data ----
  display_data <- reactive({
    message("--display_data--")
    req(sim_data())

    within_factor_n <- length(isolate(within()))

    # TODO: cancel function if within_factor_n == 0
    # so table doesn't flash update
    if (input$long == "TRUE" && within_factor_n>0) {
      wide2long(sim_data())
    } else {
      sim_data()
    }
  })

  ## update data_params_table ----
  observeEvent(sim_data(), {
    message("--data_params_table--")

    within_factors <- names(within())

    if (length(within_factors) == 0) {
      new_table <- get_params(sim_data()) %>%
        dplyr::select(n, everything())
    } else {
      new_table <- get_params(sim_data()) %>%
        tidyr::separate(var, into = within_factors, sep = "_") %>%
        dplyr::select(n, everything())
    }

    data_params_table(new_table)
  })



  ## update design_params_table ----
  observeEvent(design(), {
    message("--design_params_table--")

    design <- design()

    # update parameter boxes
    # n <- paste(design$n, collapse = ", ")
    # updateTextInput(session, "n", value = n)
    # mu <- design$mu %>% unlist() %>% paste(collapse = ", ")
    # updateTextInput(session, "mu", value = mu)
    # sd <- design$sd %>% unlist() %>% paste(collapse = ", ")
    # updateTextInput(session, "sd", value = sd)
    # r <- design$r %>%
    #   lapply(function(m) { m[upper.tri(m)] }) %>%
    #   unlist() %>% paste(collapse = ", ")
    # if (r == "") r <- "0"
    # updateTextInput(session, "r", value = r)

    new_design_table <- design$params %>%
      dplyr::select(n, everything())
    design_params_table(new_design_table)
  })


  ## design_params_table_cell_edit ----
  # observeEvent(input$design_params_table_cell_edit, {
  #   message("--design_params_table_cell_edit--")
  #
  #   cell <- input$design_params_table_cell_edit
  #   col_name <- names(design_params_table())[[cell$col+1]]
  #   old_cell <- design_params_table()[cell$row, cell$col+1]
  #
  #   if (col_name %in% c("mu", "sd")) {
  #     old_table <- design_params_table()
  #     old_table[cell$row, cell$col+1] <- cell$value
  #     design_params_table(old_table)
  #   }
  #
  #   # check OK
  #   new_cell <- design_params_table()[cell$row, cell$col+1]
  #   message(glue::glue("* {col_name}: {old_cell} -> {new_cell}"))
  #
  #   # update boxes
  #   updateTextInput(session, "mu", value = paste(design_params_table()$mu, collapse = ", "))
  #   updateTextInput(session, "sd", value = paste(design_params_table()$sd, collapse = ", "))
  #
  #   # invalidate data until next simulation???
  #
  #
  # })

  # outputs ----
  ## design_plot ----
  output$design_plot <- renderPlot({
    req(design())
    faux::plot_design(x = design())
  }, res = 96)

  ## data_plot ----
  output$data_plot <- renderPlot({
    req(sim_data())
    faux::plot_design(x = sim_data(),
                      geoms = input$geoms,
                      palette = input$palette)
  }, res = 96)

  ## design_params_table ----
  output$design_params_table <- renderDT({
    design_params_table()
  }, rownames = FALSE,
     select= "none",
     #selection = list(mode = "single", target = "cell"),
     editable = FALSE,
     options = dt_opts)

  ## data_params_table ----
  output$data_params_table <- renderDT({
    data_params_table()
  }, rownames = FALSE,
     select = "none",
     options = dt_opts)

  ## sim_data ----
  output$sim_data <- renderDT({
    req(display_data())
    display_data()
  })

  ## current_factors ----
  output$current_factors <- renderUI({
    message("--update current factors--")

    factor_names <- c(names(within()), names(between()))

    lapply(factor_names, tags$button)
  })

  ## download_data ----
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
