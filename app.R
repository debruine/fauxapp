# ui ----
ui <- dashboardPage(
  title = "Faux",
  skin = "purple",
  dashboardHeader(title = "Faux"),
  dashboardSidebar(# https://fontawesome.com/icons?d=gallery&m=free
    sidebarMenu(
      id = "tabs",
      menuItem("Multilevel", tabName = "multilevel_tab",
               icon = icon("layer-group")),
      menuItem("Factorial", tabName = "factorial_tab",
               icon = icon("th-large"))
    ),
    HTML(
      "<div style = 'padding: 1em;'>This shiny app uses the {<a href='https://debruine.github.io/faux/'>faux</a>} R package to set up a factorial or multilevel design, simulate data, and download it. [<a href='https://github.com/debruine/fauxapp'>Code on GitHub</a>]</div>"
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      includeHTML("www/google-analytics.html"),
      # links to files in www/
      tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    tabItems(
      factorial_tab,
      multilevel_tab
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
  vardesc <- reactiveVal(list())

  # demo_data ----
  observeEvent(input$demo_data, {
    w <- list(time = c(am = "Day", pm = "Night"))
    b <- list(pets = c(
        cat = "Kittens",
        dog = "Puppies",
        ferret = "Slinkies"
    ))
    within(w)
    between(b)
    vardesc(list(time = "Time of Day",
                 pets = "Type of Pet"))

    updateTextInput(session, "id_name", value = "id")
    updateTextInput(session, "id_label", value = "Pet ID")
    updateTextInput(session, "dv_name", value = "score")
    updateTextInput(session, "dv_label", value = "Sleepiness Score")

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
    w <- list(
      version = c(V1 = "Version 1", V2 = "Version 2"),
      condition = c(ctl = "Control", exp = "Experimental")
    )
    b <- list(age_group = c(young = "Age 20-29", old = "Age 70-79"))
    within(w)
    between(b)
    vardesc(list(version = "Task Version",
                 condition = "Experiment Condition",
                 age_group = "Age Group"))

    updateTextInput(session, "id_name", value = "id")
    updateTextInput(session, "id_label", value = "Subject ID")
    updateTextInput(session, "dv_name", value = "score")
    updateTextInput(session, "dv_label", value = "Score")

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
  bcells <- reactive(if (length(between()) == 0) {
    1
  } else {
    sapply(between(), length) %>% prod()
  })

  ## wcells ----
  wcells <- reactive(if (length(within()) == 0) {
    1
  } else {
    sapply(within(), length) %>% prod()
  })

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
    parse_param(input$mu, bcells() * wcells())
  })

  ## sd ----
  sd <- reactive({
    parse_param(input$sd, bcells() * wcells())
  })

  ## r ----
  r <- reactive({
    len <- wcells() * ((wcells() - 1) / 2)

    parse_param(input$r, len) %>% unlist()
  })

  ## update cell numbers ----
  observe({
    n_cells <- bcells()
    f_cells <- bcells() * wcells()
    r_cells <- wcells() * ((wcells() - 1) / 2)

    updateTextInput(session, "n", glue("n ({n_cells})"))
    updateTextInput(session, "mu", glue("mu ({f_cells})"))
    updateTextInput(session, "sd", glue("sigma ({f_cells})"))
    updateTextInput(session, "r", glue("rho ({r_cells})"))
  })

  # factorial factors ----

  ## levels_n ----
  observeEvent(input$levels_n, {
    to_show <- paste0("label_row_", 1:input$levels_n)
    h <- setdiff(1:8, 1:input$levels_n)
    to_hide <- paste0("label_row_", h)

    lapply(to_show, show)
    lapply(to_hide, hide)
  })

  ## edit_factor ----
  observeEvent(input$edit_factor, {
    factors <- c(within(), between())
    levels <- factors[[input$edit_factor]]
    factor_label <- vardesc()[[input$edit_factor]]

    # update new factor box inputs
    factor_type <-
      if (input$edit_factor %in% names(within())) "within"
      else "between"
    updateRadioGroupButtons(session, "factor_type", selected = factor_type)
    updateTextInput(session, "factor_name", value = input$edit_factor)
    updateTextInput(session, "factor_label", value = factor_label)
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
    if (factor_name == "")
      return()

    # get list of level names and display
    level_names <- paste0("level_name_", 1:input$levels_n) %>%
      lapply(function(x)
        input[[x]]) %>%
      trimws()
    level_displays <- paste0("level_display_", 1:input$levels_n) %>%
      lapply(function(x)
        input[[x]]) %>%
      trimws()

    # auto-fill missing level names
    level_names[level_names == ""] <-
      paste0("level", which(level_names == ""))

    # replace missing level display names with level names
    level_displays <-
      ifelse(level_displays == "", level_names, level_displays)

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

    # update vardesc
    vd <- vardesc()
    vd[[factor_name]] <- if (input$factor_label == "") factor_name else input$factor_label
    vardesc(vd)

    reset_factor_box()
  })

  ## reset_factor ----
  observeEvent(input$reset_factor, {
    reset_factor_box()
  })

  reset_factor_box <- function() {
    c(
      "factor_name",
      "factor_label",
      paste0("level_name_", 1:10),
      paste0("level_display_", 1:10)
    ) %>%
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

    # update vardesc
    vd <- vardesc()
    vd[[input$factor_name]] <- NULL
    vardesc(vd)
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

    new_design <- tryCatch({
      faux:::check_design(
        within = within(),
        between = between(),
        n = n(),
        mu = mu(),
        sd = sd(),
        r = r(),
        dv = dv(),
        id = id(),
        vardesc = vardesc(),
        sep = "_"
      )
    }, error = function(e) {
      message(e$message)
      showNotification(e$message, duration = 30)
      return(NULL)
    })

    design(new_design)
  })

  ## design change ----
  observeEvent(design(), {
    # only enable after there is a valid design
    sim_data(NULL)
    valid_design <- !is.null(design())
    toggleState("simulate_data", valid_design)

    # change tab if in a data tab
    if (valid_design & input$sim_tabs %in% c("Data", "Data Parameters", "Data Plot", "Code")) {
      showTab(inputId = "sim_tabs", target = "Design Parameters", select = TRUE)
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  ## simulate_data ----
  observeEvent(input$simulate_data, {
    message("--simulate_data--")

    new_sim_data <- tryCatch( {
      data <- sim_design(
        design = design(),
        empirical = input$empirical == "sample",
        long = TRUE,
        rep = 1,
        # nested = TRUE
      )

      dv_name <- names(dv())

      if (input$dv_distribution == "truncnorm") {
        data[[dv_name]] <- norm2trunc(
          x = data[[dv_name]],
          min = input$trunc_min,
          max = input$trunc_max,
          mu = input$trunc_mu,
          sd = input$trunc_sd)
      } else if (input$dv_distribution == "likert") {
        # get list of likert names and display
        likert_prob <- paste0("likert_prob_", 1:input$likert_n) %>%
          sapply(function(x) input[[x]]) %>%
          unname()

        data[[dv_name]] <- norm2likert(
          x = data[[dv_name]],
          prob = likert_prob
        )
      }

      long2wide(data)
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
    toggleState("download_data", valid_data)

    # change tab if in a design tab
    if (valid_data & input$sim_tabs %in% c("Design Parameters", "Design Plot", "Code")) {
      showTab(inputId = "sim_tabs", target = "Data Plot", select = TRUE)
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  ## display_data ----
  display_data <- reactive({
    message("--display_data--")
    req(sim_data())

    within_factor_n <- length(isolate(within()))

    # TODO: cancel function if within_factor_n == 0
    # so table doesn't flash update
    if (input$long == "TRUE" && within_factor_n > 0) {
      ddat <- wide2long(sim_data())
    } else {
      ddat <- sim_data()
    }

    mutate_if(ddat, is.numeric, round, input$round)
  })

  ## update data_params_table ----
  observeEvent(sim_data(), {
    message("--data_params_table--")

    within_factors <- names(within())
    wn <- length(within_factors)
    bn <- length(between())

    if (wn == 0) {
      new_table <- get_params(sim_data()) %>%
        dplyr::select(n, everything())
    } else {
      param_table <- get_params(sim_data()) %>%
        tidyr::separate(var, into = within_factors, sep = "_") %>%
        dplyr::select(n, everything())

      # add correlation header
      container <- ctnr(param_table, n_factors = wn+bn)
      new_table <- DT::datatable(param_table,
                                 container = container,
                                 rownames = FALSE,
                                 options = dt_opts())
    }

    data_params_table(new_table)
  })



  ## update design_params_table ----
  observeEvent(design(), {
    message("--design_params_table--")

    design <- design()

    param_table <- design$params %>%
      dplyr::select(n, everything(), sigma = sd)

    # add correlation header
    if (length(design$within) == 0) {
      new_table <- param_table
    } else {
      n_factors <- length(design$within) + length(design$between)
      container <- ctnr(param_table, n_factors)
      new_table <- DT::datatable(param_table,
                                 container = container,
                                 rownames = FALSE,
                                 options = dt_opts())
    }
    design_params_table(new_table)
  })

  ## distribution ----
  observeEvent(input$dv_distribution, {
    message("--dv_distribution--")

    show_trunc <- input$dv_distribution == "truncnorm"
    toggle("trunc_args", condition = show_trunc)

    show_likert <- input$dv_distribution == "likert"
    toggle("likert_args", condition = show_likert)

    if (show_trunc) {
      trunc_mu <- design()$mu %>% unlist() %>% mean()
      trunc_sd <- design()$sd %>% unlist() %>% mean()
      trunc_min <- trunc_mu - 3*trunc_sd
      trunc_max <- trunc_mu + 3*trunc_sd
      updateNumericInput(session, "trunc_mu", value = trunc_mu)
      updateNumericInput(session, "trunc_sd", value = trunc_sd)
      updateNumericInput(session, "trunc_min", value = trunc_min)
      updateNumericInput(session, "trunc_max", value = trunc_max)
    }

    if (show_likert) {

    }
  })

  ## likert_n ----
  observeEvent(input$likert_n, {
    to_show <- paste0("likert_row_", 1:input$likert_n)
    h <- setdiff(1:11, 1:input$likert_n)
    to_hide <- paste0("likert_row_", h)

    lapply(to_show, show)
    lapply(to_hide, hide)
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

  # factorial outputs ----

  ## design_plot ----
  output$design_plot <- renderPlot({
    req(design())
    faux::plot_design(x = design(),
                      geoms = input$design_geoms)
  }, res = 96)

  ## data_plot ----
  output$data_plot <- renderPlot({
    req(display_data())
    faux::plot_design(x = display_data(),
                      geoms = input$data_geoms,
                      palette = input$palette)
  }, res = 96)

  ## design_params_table ----
  output$design_params_table <- renderDT({
    design_params_table()
  }, rownames = FALSE,
     select= "none",
     #selection = list(mode = "single", target = "cell"),
     editable = FALSE,
     options = dt_opts())

  ## data_params_table ----
  output$data_params_table <- renderDT({
    data_params_table()
  }, rownames = FALSE,
     select = "none",
     options = dt_opts())

  ## sim_data ----
  output$sim_data <- renderDT({
    req(display_data())
    display_data()
  }, rownames = FALSE)

  ## current_factors ----
  output$current_factors <- renderUI({
    message("--update current factors--")

    factor_names <- c(names(within()), names(between()))

    lapply(factor_names, tags$button)
  })

  ## code ----
  output$code <- renderText({
    message("--code--")
    req(design())

    vd <- capture.output(dput(vardesc())) %>% paste(collapse = "\n    ")
    w <- capture.output(dput(within()))  %>% paste(collapse = "\n    ")
    b <- capture.output(dput(between()))  %>% paste(collapse = "\n    ")

    code <- glue::glue("sim_data <- faux::sim_design(
  within = {w},
  between = {b},
  n = c({input$n}),
  mu = c({input$mu}),
  sd = c({input$sd}),
  r = c({input$r}),
  dv = {capture.output(dput(dv()))},
  id = {capture.output(dput(id()))},
  vardesc = {vd},
  sep = \"_\",
  empirical = {(input$empirical == 'sample')},
  long = {input$long}
)")

    #capture.output(formatR::tidy_source(text = code, args.newline = TRUE, width.cutoff = 80)) %>%
    #  paste(collapse="\n")
    code
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

  # multilevel simulation ----

  ## reactiveVals ----
  random_factors <- reactiveVal(list())
  fixed_factors <- reactiveVal(list())
  ml_data <- reactiveVal(data.frame())

  ## demo_mixed ----
  observeEvent(input$demo_mixed, {
    random_factors(list(
      class = list(n = 3),
      student = list(n = 2, nested_in = "class")
    ))

    fixed_factors(list(
      condition = list(by = NULL, levels = c("A", "B")),
      grade = list(by = "student",
                   levels = 1:3,
                   shuffle = TRUE,
                   prob = c(1, 2, 3))
    ))
  })

  ## sim data ----
  observe({
    if (length(random_factors()) == 0) return()

    new_data <- NULL

    ## add random factors
    for (factor in names(random_factors())) {
      message("--adding factor: ", factor)

      factor_params <- random_factors()[[factor]]

      args <- setNames(factor_params$n, factor) %>%
        as.list()
      args[[".data"]] <- new_data
      args[[".nested_in"]] <- factor_params$nested_in

      new_data <- do.call(what = add_random, args = args)
    }

    ## add fixed factors
    for (factor in names(fixed_factors())) {
      factor_params <- fixed_factors()[[factor]]
      args <- list()
      args[[factor]] <- factor_params$levels
      args[[".data"]] <- new_data
      args[[".by"]] <- factor_params$by

      if (is.null(factor_params$by)) {
        new_data <- do.call(what = add_within, args = args)
      } else {
        args[[".shuffle"]] <- factor_params$shuffle
        args[[".prob"]] <- factor_params$prob
        new_data <- do.call(what = add_between, args = args)
      }
    }

    ml_data(new_data)
  })

  ## clear_mixed_design ----
  observeEvent(input$clear_mixed_design, {
    random_factors(list())
    fixed_factors(list())
    ml_data(data.frame())
  })

  ## random factors ----

  ### random_factors updates ----
  observeEvent(random_factors(), {
    message("--random_factors updates--")
    click("reset_random_factor")

    # toggle visibility of fixed factor box
    if (length(random_factors()) > 0) {
      enable(id = "add_fixed_factor")
    } else {
      disable(id = "add_fixed_factor")
    }

    # toggle random_factor_type
    toggle(id = "random_factor_type",
           condition = length(random_factors()) > 0)

    # update random_factors_nested_in choices
    new_choices <- names(random_factors())
    updatePickerInput(session = session,
                      inputId = "random_factor_nested_in",
                      choices = new_choices)

    # update fixed_factors_by choices
    new_choices <- names(random_factors())
    updatePickerInput(session = session,
                      inputId = "fixed_factor_by",
                      choices = new_choices)
  })

  ### random_factor_nested_in show/hide ----
  observeEvent(input$random_factor_type, {
    # show nested_in if type is nested
    toggle(id = "random_factor_nested_in",
           condition = input$random_factor_type == "nested")
  })

  ### edit_random_factor ----
  observeEvent(input$edit_random_factor, {
    message("--edit_random_factor--")

    factor_params <- random_factors()[[input$edit_random_factor]]

    # update new factor box inputs
    updateTextInput(session, "random_factor_name",
                    value = input$edit_random_factor)
    updateTextInput(session, "random_factor_n",
                    value = factor_params$n)
    sel <- ifelse(is.null(factor_params$nested_in), "crossed", "nested")
    updateRadioGroupButtons(session, "random_factor_type",
                            selected = sel)
    updatePickerInput(session, "random_factor_nested_in",
                      selected = factor_params$nested_in)

    runjs("openBox('new_random_factor_box')")
  })

  ### add_random_factor ----
  observeEvent(input$add_random_factor, {
    # get existing list of random factors
    rfs <- random_factors()

    # get new factor parameters
    factor_name <- trimws(input$random_factor_name)
    factor_n <- as.integer(input$random_factor_n)
    nested_in <- if (input$random_factor_type == "nested") {
      input$random_factor_nested_in
    } else {
      NULL
    }

    # update list
    rfs[[factor_name]] <- list(
      nested_in = nested_in,
      n = factor_n
    )

    random_factors(rfs)
  })

  ### delete_random_factor ----
  observeEvent(input$delete_random_factor, {
    rfs <- random_factors()

    rfs[[input$random_factor_name]] <- NULL

    random_factors(rfs)
  })

  ### reset_random_factor ----
  observeEvent(input$reset_random_factor, {
    updateRadioGroupButtons(session, "random_factor_type", selected = "crossed")
    reset("random_factor_name")
    reset("random_factor_n")
  })

  ## fixed factors ----

  ### fixed_levels_n ----
  observeEvent(input$fixed_levels_n, {
    to_show <- paste0("fixed_label_row_", 1:input$fixed_levels_n)
    h <- setdiff(1:8, 1:input$fixed_levels_n)
    to_hide <- paste0("fixed_label_row_", h)

    lapply(to_show, show)
    lapply(to_hide, hide)
  })

  ### fixed_factor_type ----
  observeEvent(input$fixed_factor_type, {
    # show nested_in if type is nested
    toggle(id = "fixed_factor_by",
           condition = input$fixed_factor_type == "between")

    toggle(id = "fixed_factor_shuffle",
           condition = input$fixed_factor_type == "between")

    # hide / show prob
    probs <- paste0("fixed_level_prob_", 1:8)
    func <- ifelse(input$fixed_factor_type == "between", show, hide)
    c(probs, "probability_header") %>%
      lapply(func)
  })

  ### edit_fixed_factor ----
  observeEvent(input$edit_fixed_factor, {
    message("--edit_fixed_factor--")

    factor_params <- fixed_factors()[[input$edit_fixed_factor]]

    # update new factor box inputs
    updateTextInput(session, "fixed_factor_name",
                    value = input$edit_fixed_factor)
    n_levels <- length(factor_params$levels)
    updateTextInput(session, "fixed_levels_n",
                    value = n_levels)
    sel <- ifelse(is.null(factor_params$by), "within", "between")
    updateRadioGroupButtons(session, "fixed_factor_type",
                            selected = sel)
    updatePickerInput(session, "fixed_factor_by",
                      selected = factor_params$by)

    sel <- ifelse(factor_params$shuffle, "yes", "no")
    updateRadioGroupButtons(session, "fixed_factor_shuffle",
                            selected = sel)

    for (i in 1:n_levels) {
      nm <- paste0("fixed_level_name_", i)
      updateTextInput(session, nm, value = factor_params$levels[[i]])

      nm <- paste0("fixed_level_prob_", i)
      val <- factor_params$prob[[i]]
      val <- if (is.null(val)) { 1 } else { val }
      updateTextInput(session, nm, value = val)
    }

    runjs("openBox('new_fixed_factor_box')")
  })

  ### add_fixed_factor ----
  observeEvent(input$add_fixed_factor, {
    message("--add_fixed_factor--")
    # get existing list of fixed factors
    ffs <- fixed_factors()

    level_names <- paste0("fixed_level_name_", 1:input$fixed_levels_n) %>%
      lapply(function(x) input[[x]]) %>%
      trimws()

    level_probs <- paste0("fixed_level_prob_", 1:input$fixed_levels_n)%>%
      lapply(function(x) input[[x]]) %>%
      unlist() %>%
      as.numeric()

    # replace NAs with 0
    level_probs[is.na(level_probs)] <- 0

    ffs[[input$fixed_factor_name]] <- list(
      by = if (input$fixed_factor_type == "within") { NULL } else { input$fixed_factor_by },
      levels = level_names,
      shuffle = input$fixed_factor_shuffle == "yes",
      prob = level_probs
    )

    fixed_factors(ffs)

    click("reset_fixed_factor")
  })

  ### fixed_factors() updates ----
  observeEvent(fixed_factors(), {
    message("--fixed_factors updates--")
    click("reset_fixed_factor")
  })

  ### delete_fixed_factor ----
  observeEvent(input$delete_fixed_factor, {
    ffs <- fixed_factors()

    ffs[[input$fixed_factor_name]] <- NULL

    fixed_factors(ffs)
  })

  ### reset_fixed_factor ----
  observeEvent(input$reset_fixed_factor, {
    updateRadioGroupButtons(session, "fixed_factor_type", selected = "within")
    updateRadioGroupButtons(session, "fixed_factor_shuffle", selected = "no")

    reset("fixed_factor_name")
    reset("fixed_levels_n")

    paste0("fixed_level_name_", 1:8) %>%
      lapply(reset)

    paste0("fixed_level_prob_", 1:8) %>%
      lapply(reset)
  })

  ## multilevel outputs ----

  ### current_random_factors ----
  output$current_random_factors <- renderUI({
    factor_names <- names(random_factors())
    lapply(factor_names, tags$button)
  })

  ### current_fixed_factors ----
  output$current_fixed_factors <- renderUI({
    factor_names <- names(fixed_factors())
    lapply(factor_names, tags$button)
  })

  ### multilevel_data ----
  output$multilevel_data <- renderDT({
    ml_data()
  }, rownames = FALSE,
     options = list(pageLength = 25)
  )


}

shinyApp(ui, server)
