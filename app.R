#  This file is part of CEOsys Recommendation Checker.
#
#  Copyright (c) 2021 CEOsys project team <https://covid-evidenz.de>.
#
#  CEOsys Recommendation Checker is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  CEOsys Recommendation Checker is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with CEOsys Recommendation Checker.  If not, see <https://www.gnu.org/licenses/>.

library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)
library(eeptools)
library(stringr)
library(dplyr)
library(jsonlite)
library(httr)
library(purrr)
library(plotly)
library(glue)
library(dotenv)
library(readr)
library(tidyr)
library(shinycssloaders)

source("load_data.R")
source("dropdownbutton.R")

addResourcePath(prefix = "img", directoryPath = "images")

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$style(type = "text/css", "h1 { margin-top:0;} "),
  #*************************************************************************
  # Title Row
  fluidRow(
    column(6, img(src = "img/celida-logo-white.png", height = 120), align = "right"),
    column(6, img(src = "img/logo_num.jpg", height = 120), align = "left"),
  ),
  fluidRow(
    column(1,
      selectInput(
        inputId = "ward",
        label = h2("Ward"),
        choices = c("All"),
        selected = "All",
        width = "80%"
      ),
      align = "center"
    ),
    column(4,
      h2("Guideline Recommendation"),
      dropdownButton(
        label = "Guideline Recommendation",
        status = "default",
        width = "100%",
        tags$div(
          class = "container",
          checkboxGroupInput(
            inputId = "recommendation_url",
            label = "Guideline Recommendation",
            width = "100%",
            choiceNames = lapply(recommendations %>% str_glue_data("<b>[{short}]</b> {recommendation_title}"), HTML),
            choiceValues = recommendations$recommendation_url,
            selected = recommendations$recommendation_url
          )
        )
      ),
      align = "center"
    ),
    column(2,
      dateRangeInput(
        inputId = "observation_window",
        label = h2("Date range"),
        start = Sys.Date() - 14,
        end = Sys.Date(),
        min = "2021-01-01",
        max = Sys.Date(),
        width = "100%",
      ),
      
      align = "left",
    ),

    column(
      5,
      wellPanel(
        h1("Guideline Recommendation"),
        htmlOutput("recommendation_text"),
        tags$head(tags$style("#recommendation_text { font-size:18px; max-height: 20%; }")),
        
      )
    )
  ),

  #*************************************************************************
  # Content Row

  fluidRow(

    # Patient Table Column
    column(
      7,
      wellPanel(
        h1("Patients"),
        DT::dataTableOutput("patienttable") %>% shinycssloaders::withSpinner(type = 6)
      ),
            wellPanel(
        h1("Statistics"),
        #DT::dataTableOutput("table_statistics") %>% shinycssloaders::withSpinner(type = 6)
        #DT::dataTableOutput('table_statistics')
      ),
      wellPanel(
        h1("Legend"),
        uiOutput("legend_text"), align = "left")
    ),

    # Recommendation Column

    column(
      5,
      # recommendation-Text Row
      wellPanel(
        h1("Comments"),
        textAreaInput(inputId = "comment",
                  label = "", 
                  value = "Make comments on patient's treatment",
                  width = "100%",
                  height = '96px',
                  placeholder = NULL),
        verbatimTextOutput("comment"),
       align = "left"),

      # recommendation-Population Row
      wellPanel(
        h1("Population"),
        uiOutput("population_main", height="500px") %>% shinycssloaders::withSpinner(type = 6, proxy.height = "300px", hide.ui = FALSE)
      ),

      # recommendation-Intervention Row
      wellPanel(
        h1("Intervention"),
        uiOutput("intervention_main") %>% shinycssloaders::withSpinner(type = 6, proxy.height = "300px", hide.ui = FALSE)
      )
    )
  )
)


getPlotUIs <- function(vars, type) {
  #' @title getPlotUIs
  #'
  #' @description
  #' Render a list of tabPanels with plotly outputs for the given variables
  #' This function is used to populate the different tabs for population
  #' and intervention variables each time a new guideline recommendation is
  #' selected. See setPlotUIOutputs() for how the tabs are filled with plots.
  #'
  #' @param vars a vector of variables to display as separate tabs in the tabsetPanel
  #' @param type a string indicating whether this is a population or an intervention plot
  #'
  #' @return A Shiny UI component containing a list of tabPanels, each displaying a plotly plot for one of the given variables
  #'
  #' @examples
  #' getPlotUIs(vars = c("variable1", "variable2"), type = "intervention")
  #'
  return(renderUI({
    myTabs <- map(vars, ~ tabPanel(
      title = as.character(.x), # variable title
      wellPanel(
        plotlyOutput(paste("plot", type, .x, sep = "_"), height = 350),
        style = "padding:0;margin-bottom:0;"
      )
    ))

    do.call(tabsetPanel, c(myTabs, list(id = glue("{type}Panel"))))
  }))
}


setPlotUIOutputs <- function(output, person_id, run_id, vars, type, min_dt, max_dt) {
  #' @title setPlotUIOutputs
  #'
  #' @description
  #' Set plot outputs for the given variables in the Shiny output object.
  #' This function creates plotly output elements for each tab generated by
  #' getPlotUIs().
  #'
  #' @param output a Shiny output object where the plot outputs will be stored
  #' @param person_id an ID for the person whose data will be displayed
  #' @param run_id an ID for the run from which the data will be displayed
  #' @param vars a vector of variables to be displayed as separate plots
  #' @param type a string indicating the type of plots to be displayed
  #' @param min_dt minimal datetime displayed in the plot
  #' @param max_dt maximal datetime displayed in the plot
  #'
  #' @return A Shiny output object with plot outputs for the given variables stored in it
  #'
  #' @examples
  #' setPlotUIOutputs(output, person_id = 123, run_id = 456, vars = c("variable1", "variable2"), type = "intervention")
  #'
  vars <- vars %>%
    filter(type == !!type) %>%
    # TODO: The next line drops unique criterion_name if there are more than 1
    # per variable_name. This is intended at this point in order to show each
    # variable just once, but may not be correct behaviour in general.
    distinct(variable_name, .keep_all = TRUE)

  for (i in seq_len(nrow(vars))) {
    var <- vars$variable_name[i]
    criterion_name <- vars$criterion_name[i]


    local({
      plotname <- paste("plot", type, var, sep = "_")
      localvar <- var

      data <- load_data(person_id = person_id, run_id = run_id, criterion_name = criterion_name)

      output[[plotname]] <- renderPlotly({
        if (is.null(data) || nrow(data) == 0) {
          # no data
          # browser()
          ggp <- ggplot() +
            geom_blank() +
            expand_limits(x = c(min_dt, max_dt), y = c(0, 1))
        } else if (class(data$value) == "factor") {
          # categorical variables
          ggp <- ggplot(data, aes(x = datetime, y = value, group = value)) +
            geom_step(aes(group = 1)) +
            geom_point()
        } else if (any(data$datetime != data$end_datetime)) {
          # time periods
          ggp <- ggplot(data, aes(y = value, yend = value, x = datetime, xend = end_datetime)) +
            geom_segment(linewidth = 1) +
            geom_point()
        } else if (class(data$value) == "logical") {
          # boolean variables
          ggp <- ggplot(data, aes(x = datetime, ymin = 0, y = value, ymax = value)) +
            geom_linerange() +
            geom_point()
        } else {
          # continuous data
          ggp <- ggplot(data, aes(datetime, value)) +
            geom_line() +
            geom_point()
        }

        ggplotly(ggp +
          xlab("Date") +
          ylab(localvar) +
          coord_cartesian(xlim = c(min_dt, max_dt)))
      })
    })
  }
}






############# Server ############
server <- function(input, output, session) {
  # REACTIVE VALUES

  # Overview of patients and the P/I state (for left side)
  # This tibble contains one patient per row, "name" and "ward" as columns and
  # for each selected guideline recommendations a column with one of "PI", "P",
  # "I" or "o", indicating whether the patient belongs to (P)opulation,
  # (I)ntervention, Population & Intervention (PI) or none (o).
  patient_overview <- reactive({
    load_patient_list(isolate(input$recommendation_url), start_datetime = format(input$observation_window[1]), end_datetime = format(input$observation_window[2]))
  })

  # Patient overview filtered by ward
  # Second reactive variable that just filters the patient_overview by the ward
  # This is used to not having to update the patient_overview() each time a
  # filter on ward is selected
  patient_overview_per_ward <- reactive({
    if (input$ward == "All") {
      return(patient_overview()$patients)
    } else {
      return(patient_overview()$patients %>% filter(ward == input$ward))
    }
  })

  # Observe cell clicks and set person_id and recommendation_url accordingly
  rv <- reactiveValues()
  rv$selected_person_id <- reactive({
    patient_overview_per_ward()[input$patienttable_cells_selected[1], ]$person_id
  })
  rv$selected_recommendation_url <- reactive({
    if ((length(input$patienttable_cells_selected) > 0) && (input$patienttable_cells_selected[2] > 1)) {
      input$recommendation_url[input$patienttable_cells_selected[2] - 1]
    }
  })

  # used to observe if either person_id or recommendation_url has changed
  rv$selection_changed <- reactive({
    list(rv$selected_person_id(), rv$selected_recommendation_url())
  })


  # Patient data table
  options(DT.options = list(pageLength = 20))
  observeEvent(input$recommendation_url, {
    updateSelectInput(session, "ward", choices = c("All", sort(unique(patient_overview()$patients$ward))))
    
    recommendation_names_short <- (recommendations %>% filter(recommendation_url %in% input$recommendation_url))$short
    colnames <- c("Name", "Ward", recommendation_names_short)

    output$patienttable <- DT::renderDataTable(
      server = FALSE,
      DT::datatable(patient_overview_per_ward() %>% select(all_of(colnames)),
        rownames = FALSE,
        selection = list(
          mode = "single", 
          target = "cell",
          selectable=as.matrix(expand.grid(1:nrow(patient_overview_per_ward()), 2:length(colnames))),
          selected = matrix(c(1, 2), ncol = 2)
          ),
        colnames = colnames,
        options = list(
          columnDefs = list(
            list(
              # javascript function to change PI/P/I/o to appropriate symbols (checkmarks, crosses)
              render = JS(
                "function(data, type, row, meta) {",
                "if (type === 'display') {",
                "  if (data == 'PI') {",
                "    return '&#10004;'",
                "  } else if (data == 'P') {",
                "    return '&#x2718;'",
                "  } else if (data == 'I') {",
                "    return '(&#10004;)'",
                "  } else if (data == 'o') {",
                "    return '(&#x2718;)';",
                "  }",
                "}",
                "return data",
                "}"
              ),
              className = "dt-center", targets = seq(length(colnames) - 1)
            )
          )
        )
      ) %>% formatStyle(seq(3, length(colnames)),
        backgroundColor = styleEqual(
          # set cell background color of PI to green, of P to red
          c("PI", "P"),
          c(
            rgb(200, 230, 200, 255, 255, 255),
            rgb(230, 200, 200, 255, 255, 255)
          )
        )
      )
    )
  })


  ##### Functions for right column #####
  # observe population tab panel change - not required currently because all tab
  # contents are set simultaneously
  # observeEvent(input$POPULATIONPanel, {
  #  #browser()
  # })

  output$recommendation_text <- renderUI({
    if (!is.null(rv$selected_recommendation_url())) {
      HTML(recommendations %>% filter(recommendation_url == rv$selected_recommendation_url()) %>% pull(recommendation_description))
    }
  })
  
  output$comment_text <- renderText({ input$comment })
  
  output$legend_text <- renderUI({
    HTML("&#10004; - patient is treated according to the recommendation guideline <br>
         &#x2718; - patient is not treated according to the recommendation guideline <br>
         (&#10004;) - patient is treated according to the recommendation guideline, but is not in the population<br>
         (&#x2718;) - patient is not treated according to the recommendation guideline, but is also not in the population
         ")
    })
  ############################################
  output$table_statistics <- DT::renderDataTable(DT::datatable(dummy_table %>% select(all_of(colnames)),
                                                               colnames = recommendation))
###################################################
  observeEvent(rv$selected_recommendation_url(),
    {
      rv$recommendation_criteria <- load_recommendation_variables(rv$selected_recommendation_url())
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

      #### Create divs######
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
}

shinyApp(ui = ui, server = server)
