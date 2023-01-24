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

# load data ####################################################################

source("load_data_new.R")

patient_id <- patients$person_id[1]
recommendation_id <- recommendations$id[1]

# patient_results <- load_recommendation_results(recommendation_id)


# app original #################################################################


variable_name_mappings <- list(
  test_covid19_pcr = "COVID19 PCR Test",
  body_position = "Body position",
  ventilation_mode = "Ventilation Mode",
  RASS = "RASS",
  drug_norepinephrine = "Catecholamine",
  drug_epinephrine = "Epinephrine",
  deltaSOFA = "ΔSOFA",
  drug_dobutamine = "Dobutamine",
  drug_dopamine = "Dopamine",
  oxygenation_index_calc = "P/F ratio",
  drug_vasopressin = "Vasopressin",
  sO2 = "sO2",
  respiratory_rate = "Respiratory rate",
  drug_dexamethason_bolus = "Dexamethasone",
  Measurement_aPTT = "PTT"
)

addResourcePath(prefix = "pics", directoryPath = "pictures")

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$style(type = "text/css", "h1 { margin-top:0;} "),
  #*************************************************************************
  # Title Row

  fluidRow(
    column(7,
      selectInput(
        inputId = "ward", label = h1("Ward"),
        choices = c("All"),
        selected = "All",
        width = "60%"
      ),
      selectInput(
        inputId = "recommendation_id", label = h1("Guideline Recommendation"),
        choices = setNames(recommendations$id, paste("C", recommendations$id, recommendations$title)),
        selected = NULL,
        width = "90%"
      ),
      align = "center"
    ),
    column(5,
      tags$img(src = "pics/logo_ceosys.jpg", height = 100),
      align = "right",
      tags$img(src = "pics/logo_num.jpg", height = 100), align = "right",
      wellPanel(
        h1("Guideline Recommendation"),
        htmlOutput("recommendation_text"),
        tags$head(tags$style("#recommendation_text { font-size:18px; max-height: 20%; }"))
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
        DT::dataTableOutput("patienttable")
      )
    ),

    # Recommendation Column

    column(
      5,

      # recommendation-Text Row



      # recommendation-Population Row
      wellPanel(
        h1("Population"),
        uiOutput("population_main")
      ),

      # recommendation-Intervention Row
      wellPanel(
        h1("Intervention"),
        uiOutput("intervention_main")
      )
    )
  )
)


getPlotUIs <- function(vars, type) {
  return(renderUI({
    myTabs <- map(vars, ~ tabPanel(
      title = as.character(.x), # variable title
      wellPanel(
        plotlyOutput(paste("plot", type, .x, sep = "_"), height = 230),
        style = "padding:0;margin-bottom:0;"
      )
    ))

    do.call(tabsetPanel, myTabs)
  }))
}

setPlotUIOutputs <- function(output, patientdata, vars, type) {
  for (var in vars) {
    local({
      plotname <- paste("plot", type, var, sep = "_")
      localvar <- var

      output[[plotname]] <- renderPlotly({
        if (is.null(patientdata())) {
          # no patientdata available
          return(ggplotly())
        }

        data <- patientdata() %>% filter(variable_name == localvar)
        max_dt <- max(patientdata()$datetime)
        min_dt <- min(patientdata()$datetime)


        if (nrow(data) == 0) {
          # no data
          ggp <- ggplot(data, aes(datetime, value)) +
            geom_blank() +
            expand_limits(x = c(max_dt - 86400 * 4, max_dt), y = c(0, 1))
        } else if (class(data$value) == "factor") {
          # categorical variables
          ggp <- ggplot(data, aes(x = datetime, y = value, group = value)) +
            geom_step(aes(group = 1)) +
            geom_point()
        } else if (any(!is.na(data$datetime_end))) {
          # time periods
          ggp <- ggplot(data, aes(y = value, yend = value, x = datetime, xend = datetime_end)) +
            geom_segment(linewidth = 1) +
            geom_point()
        } else if (grepl("_bolus", localvar)) {
          # drug bolus
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
  tabldat <- reactive({
    if (input$ward == "All") {
      return(tabldspl())
    } else {
      return(tabldspl()[patient_results()$ward == input$ward, ])
    }
  })


  rv <- reactiveValues()
  rv$patient_id <- reactive({
    tabldat()[input$patienttable_rows_selected, ]$Name
  })
  rv$recommendation_id <- reactive({
    input$recommendation_id
  })



  # Patient data table                                                          # font size change
  options(DT.options = list(pageLength = 25))
  observeEvent(input$ward, {
    output$patienttable <- DT::renderDataTable(
      server = FALSE,
      DT::datatable(tabldat(),
        rownames = FALSE,
        selection = list(mode = "single", selected = c(1)),
        colnames = c("Name", "Ward", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10"),
        options = list(
          columnDefs = list(
            list(
              render = JS(
                "function(data, type, row, meta) {",
                "if (type === 'display') {",
                "  if (data == true) {",
                "    return '&#10004;'",
                "  } else if (data == false) {",
                "    return '&#x2718;'",
                "  } else if (data == -1) {",
                "    return '';",
                "  }",
                "}",
                "return data",
                "}"
              ),
              className = "dt-center", targets = 1:11
            )
          )
        )
      ) %>% formatStyle(c(3 + recommendation_id), # +rv$recommendation_id > reaktive Markierung der Spalte entsprehedne der asugewählten Empfehlung
        backgroundColor = styleEqual(
          c(TRUE, FALSE),
          c(
            rgb(200, 230, 200, 255, 255, 255),
            rgb(230, 200, 200, 255, 255, 255)
          )
        )
      )
    )
  })


  ##### Functions for right column #####


  patientdata <- reactive({
    load_patient(rv$patient_id(), rv$recommendation_id())
  })

  patient_results <- reactive({
    load_recommendation_results(rv$recommendation_id())
  })

  tabldspl <- reactive({
    t <- data.frame(
      "Name" = patient_results()$person_id,
      "Name" = patient_results()$ward,
      "C1" = patient_results()$valid_exposure,
      "C2" = patient_results()$valid_exposure,
      "C3" = patient_results()$valid_exposure,
      "C4" = patient_results()$valid_exposure,
      "C5" = patient_results()$valid_exposure,
      "C6" = patient_results()$valid_exposure,
      "C7" = patient_results()$valid_exposure,
      "C8" = patient_results()$valid_exposure,
      "C9" = patient_results()$valid_exposure,
      "C10" = patient_results()$valid_exposure
    )

    # set I and P&I to NA if P doesn't match the patient
    # t[!t$P, c("Compliant")] <- NA

    return(t)
  })





  output$recommendation_text <- renderUI({
    HTML(recommendations[recommendations$id == input$recommendation_id, ]$text)
  })


  observeEvent(input$recommendation_id, {
    updateSelectInput(session, "ward", choices = c("All", unique(patient_results()$ward)))


    # Input change ###
    # recommendation_variables <-

    recommendation_variables <- load_recommendation_variables(input$recommendation_id) # recommendation_variables


    vars_population <- recommendation_variables %>% # patient_results
      filter(type == "POPULATION") %>%
      pull(variable_name) %>%
      unique() %>%
      as.list()
    vars_intervention <- recommendation_variables %>%
      filter(type == "INTERVENTION") %>%
      pull(variable_name) %>%
      unique() %>%
      as.list()

    ##### Create divs######
    output$population_main <- getPlotUIs(vars_population, "POPULATION")
    setPlotUIOutputs(output, patientdata, vars_population, "POPULATION")

    output$intervention_main <- getPlotUIs(vars_intervention, "INTERVENTION")
    setPlotUIOutputs(output, patientdata, vars_intervention, "INTERVENTION")
  })
}

shinyApp(ui = ui, server = server)
