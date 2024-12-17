library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(eeptools)
library(jsonlite)
library(plotly)
library(glue)
library(dotenv)
library(shinycssloaders)

source("load_data.R")
source("ui_utils.R")

addResourcePath(prefix = "img", directoryPath = "images")


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = paste0("celida.css?", generate_random_string()))
  ),
  tags$script(src = paste0("process-cell-data.js?", generate_random_string())),
  tags$script(src = paste0("batterybar.js?", generate_random_string())),
  #*************************************************************************
  fluidRow(
    column(
      2,
      img(src = "img/celida-logo-white.png", height = 100)
    ),
    column(2, img(src = "img/logo_num.jpg", height = 100)),
    column(5,
      h2("Guideline Recommendation"),
      dropdownButton(
        label = "Guideline Recommendation",
        status = "primary",
        circle = F,
        checkboxGroupInput(
          inputId = "recommendation_url",
          label = "Guideline Recommendation",
          width = "100%",
          choiceNames = lapply(recommendations %>% str_glue_data("<b>[{short}]</b> {recommendation_title}"), HTML),
          choiceValues = recommendations$recommendation_url,
          selected = recommendations$recommendation_url
        )
      ),
      align = "center"
    ),
    column(3,
      dateRangeInput(
        inputId = "observation_window",
        label = h2("Date range"),
        start = "2021-01-01",
        end = "2021-01-10",
        min = "2021-01-01",
        max = Sys.Date(),
        width = "100%",
        weekstart = 1
      ),
      align = "center",
    )
  ),

  #*************************************************************************
  # Content Row

  fluidRow(

    # Patient Table Column
    column(
      12,
      wellPanel(
        DT::dataTableOutput("patienttable") %>% shinycssloaders::withSpinner(type = 6),
        h3("Legend"),
        div(class = "legend-icon", style = "background-color: var(--population-intervention-color);"),
        "Patient is treated according to the recommendation",
        br(),
        div(class = "legend-icon", style = "background-color: var(--population-color);"),
        "Patient is not treated according to the recommendation",
        br(),
        div(class = "legend-icon", style = "background-color: var(--none-color);"),
        "Recommendation not applicable to the patient",
        br(),
        div(class = "legend-icon", HTML("&#x1F4AC")),
        "Comment available",
        br(),
        align = "left"
      )
    )
  )
)


##############################################################################

############# Server ############
server <- function(input, output, session) {
  # REACTIVE VALUES

  # Overview of patients and the P/I state (for left side)
  # This tibble contains one patient per row, "name" and "ward" as columns and
  # for each selected guideline recommendations a column with one of "PI", "P",
  # "I" or "o", indicating whether the patient belongs to (P)opulation,
  # (I)ntervention, Population & Intervention (PI) or none (o).
  patient_overview <- reactive({
    load_patient_list(
      # isolate(input$recommendation_url), # retrieve only selected recommendations
      # retrieve all recommendations - required currently selecting a different subset
      # of recommendations currently does not fetch new data, it just subsets the
      # dataset of all retrieved recommendations (thus, just retrieve all at this point)
      recommendations$recommendation_url,
      start_datetime = format(input$observation_window[1]),
      end_datetime = format(input$observation_window[2])
    )
  })

  # Patient overview filtered by ward
  # Second reactive variable that just filters the patient_overview by the ward
  # This is used to not having to update the patient_overview() each time a
  # filter on ward is selected
  patient_data <- reactive({
    patient_overview()$patients
  })

  # Observe cell clicks and set person_id and recommendation_url accordingly
  rv <- reactiveValues()

  rv$table_initialized <- FALSE

  rv$selected_person_id <- reactive({
    patient_data()[input$patienttable_cells_selected[1], ]$person_id
  })

  rv$selected_recommendation_url <- reactive({
    if ((length(input$patienttable_cells_selected) > 0)) {
      # Apparently shiny DT starts with a column index of 0 (?)
      col_index <- input$patienttable_cells_selected[2] + 1

      # get a list of datatable column indices with visible data - these are the ones
      # that are selectable and the length of that vector is the same as the length
      # of the selected recommendations. thus, by finding the position at which the
      # selected column index is in that list, we can determine which recommendation url
      # was selected
      recommendation_idx <- which(dataColumnIndices(rv$columnDefs()) == col_index)
      assertthat::assert_that(length(recommendation_idx) > 0)

      # select the recommendation_url corresponding to the selected column index
      input$recommendation_url[recommendation_idx]
    }
  })

  # used to observe if either person_id or recommendation_url has changed
  rv$selection_changed <- reactive({
    list(rv$selected_person_id(), rv$selected_recommendation_url())
  })

  # observer to show modal dialog (with population/intervention data)
  # does fire every time a cell is clicked, because rv$selected_recommendation_url
  # is set every time (it does not check whether a different cell/recommendation)
  # has been selected compared to the previous selection
  # rv$selection_changed shouldn't be used here because event is fired when
  # the data table is initalized --> would display the modal dialog before clicking
  observeEvent(rv$selected_recommendation_url(), {
    showModal(dataModal())
  })


  ##############################################################################

  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }


  # Patient data table
  options(DT.options = list(pageLength = 20))

  rv$colnames_expanded <- reactive({
    recommendation_names_short <- (recommendations %>% filter(recommendation_url %in% input$recommendation_url))$short
    expand_colnames(recommendation_names_short)
  })

  rv$colnames_comment <- reactive({
    c("Patient", "Ward", "Comment", rv$colnames_expanded())
  })

  rv$columnDefs <- reactive({
    generate_columnDefs(rv$colnames_comment())
  })

  rv$data <- reactive({
    data <- patient_data() %>%
      select(all_of(c("Patient", "Ward", rv$colnames_expanded()))) %>%
      add_column(shinyInput(textAreaInput, nrow(patient_data()), "cbox_"), .after = "Ward")
  })

  output$patienttable <- DT::renderDataTable(
    DT::datatable(
      rv$data(),
      container = htmltools::withTags(table(tableHeader(c(rv$colnames_comment())), tableFooter(rep_along(rv$colnames_comment(), "")))),
      rownames = FALSE,
      filter = list(
        position = "top",
        pageLength = 5,
        autoWidth = TRUE,
        clear = TRUE
      ),
      selection = "none",
      extensions = c("FixedHeader", "Responsive", "Select"),
      escape = FALSE,
      options = list(
        columnDefs = c(
          rv$columnDefs(),
          list(list(className = "dt-center", targets = seq(0, length(rv$colnames_comment()) - 1))) # center the contents of all cells
        ),
        dom = "tipr",
        select = list(
          style = "single",
          items = "cell",
          selector = ".data-cell"
        ),
        autoWidth = FALSE,
        bAutoWidth = FALSE,
        fixedHeader = TRUE,
        footerCallback = JS("function(row, data, start, end, display) { footerSummary(this.api(), row, data, start, end, display); }"),
        rowCallback = JS("function(row, data, displayNum) { processCellData(this.api(), row, data, displayNum); }"),
        reDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
        drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
        initComplete = JS("function() { onInitComplete(this.api()); }")
      )
    ),

    # disable server-side processing of data table input (see https://rstudio.github.io/DT/server.html)
    # - required because we are using tibbles with named lists as cell items which (as of 23-04-03) cannot
    #   be processed by the server (with an error like "DataTables warning: table id=DataTables_Table_0 -
    #   Error in `[<-.data.frame`(`*tmp*`, , j, value = list(structure(list(A = c("A", : replacement element 1 is a matrix/data frame of 3 rows, need 2)"
    server = FALSE,
  )





  ##### Functions for popup window #####

  output$recommendation_text <- renderUI({
    if (!is.null(rv$selected_recommendation_url())) {
      HTML(recommendations %>% filter(recommendation_url == rv$selected_recommendation_url()) %>% pull(recommendation_description))
    }
  })

  observeEvent(rv$selected_recommendation_url(),
    {
      run_id <- patient_overview()$run_id %>%
        filter(url == rv$selected_recommendation_url()) %>%
        pull(run_id)

      min_dt <- as.POSIXct(format(input$observation_window[1]))
      max_dt <- as.POSIXct(format(input$observation_window[2]))

      rv$recommendation_criteria <- load_recommendation_variables(
        run_id = run_id,
        person_id = rv$selected_person_id(),
        start_datetime = min_dt,
        end_datetime = max_dt
      )

      rv$vars_population <- rv$recommendation_criteria %>%
        filter(type == "population") %>%
        pull(variable_name) %>%
        unique() %>%
        as.list()

      rv$vars_intervention <- rv$recommendation_criteria %>%
        filter(type == "intervention") %>%
        pull(variable_name) %>%
        unique() %>%
        as.list()

      #### Create divs ######
      output$population_main <- getPlotUIs(rv$vars_population, "population")
      output$intervention_main <- getPlotUIs(rv$vars_intervention, "intervention")
    },
    priority = 1 # make sure this is run before observeEvent(rv$selection_changed())
  )

  observeEvent(rv$selection_changed(),
    {
      if (is.null(rv$selected_recommendation_url())) {
        return()
      }

      run_id <- patient_overview()$run_id %>%
        filter(url == rv$selected_recommendation_url()) %>%
        pull(run_id)

      min_dt <- as.POSIXct(format(input$observation_window[1]))
      max_dt <- as.POSIXct(format(input$observation_window[2]))

      setPlotUIOutputs(output, person_id = rv$selected_person_id(), run_id = run_id, vars = rv$recommendation_criteria, type = "population", min_dt = min_dt, max_dt = max_dt)
      setPlotUIOutputs(output, person_id = rv$selected_person_id(), run_id = run_id, vars = rv$recommendation_criteria, type = "intervention", min_dt = min_dt, max_dt = max_dt)
    },
    priority = 0 # make sure this is run after observeEvent(rv$selected_recommendation_url(), ...)
  )

  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      wellPanel(
        h1("Guideline Recommendation"),
        htmlOutput("recommendation_text"),
        tags$head(tags$style("#recommendation_text { font-size:18px; max-height: 20%; }")),
      ),
      # recommendation-Population Row
      wellPanel(
        h1("Population"),
        uiOutput("population_main", height = "500px") %>% shinycssloaders::withSpinner(type = 6, proxy.height = "300px", hide.ui = FALSE)
      ),

      # recommendation-Intervention Row
      wellPanel(
        h1("Intervention"),
        uiOutput("intervention_main") %>% shinycssloaders::withSpinner(type = 6, proxy.height = "300px", hide.ui = FALSE)
      ),

      # Comment
      wellPanel(
        h1("Comments"),
        textAreaInput(
          inputId = "comment",
          label = "",
          placeholder = "Make comments on patient's treatment",
          width = "100%",
          height = "96px",
        ),
        verbatimTextOutput("comment"),
        align = "left"
      ),
      footer = tagList(
        modalButton("Dismiss"),
      ),
      size = "l",
      easyClose = TRUE
    )
  }
}

shinyApp(ui = ui, server = server)
