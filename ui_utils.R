generate_columnDefs <- function(colnames) {
  #' Generate Column Definitions for DataTables
  #'
  #' This function generates a list of column definitions and visible column names for use with
  #' DataTables in Shiny apps. It takes into account columns with a '.' in their names and
  #' treats them as grouped columns with a common prefix, showing only the main column
  #' (with suffix ".data") and hiding the rest.
  #'
  #' @param colnames A character vector of column names
  #'
  #' @return A list of column definition objects for use with DataTables
  #'
  #' @export
  #'
  #' @examples
  #' ## Generate column definitions for a sample data frame
  #' df <- data.frame(Name.data = c("John", "Jane"),
  #'                  Name.info = c("M", "F"),
  #'                  Age.data = c(30, 25),
  #'                  Age.info = c("years", "years"),
  #'                  stringsAsFactors = FALSE)
  #'
  #' result <- generate_columnDefs(colnames(df))
  #' print(result)
  visible_colnames <- character(0)

  columnDefs <- lapply(seq_along(colnames), function(index) {
    colname <- colnames[index]

    if (grepl("\\.", colname)) {
      colname_parts <- unlist(strsplit(colname, ".", fixed = TRUE))
      prefix <- colname_parts[1]
      suffix <- colname_parts[2]

      if (suffix == "data") {
        visible_colnames <<- c(visible_colnames, prefix)
        return(list(
          targets = index - 1,
          title = prefix,
          name = colname,
          visible = TRUE
        ))
      } else if (startsWith(colname, prefix)) {
        return(list(
          targets = index - 1,
          visible = FALSE,
          name = colname
        ))
      }
    } else {
      visible_colnames <<- c(visible_colnames, colname)
      return(list(
        targets = index - 1,
        name = colname,
        title = colname,
        visible = TRUE
      ))
    }
    return(NULL)
  })

  # Remove NULL values from the list and return the result
  columnDefs <- columnDefs[!sapply(columnDefs, is.null)]

  return(columnDefs)
}

visibleColumnIndices <- function(columnDefs) {
  #' Get indices of visible columns.
  #'
  #' This function returns the indices of visible columns in a list of column definitions.
  #'
  #' @param columnDefs A list of column definitions, each containing a 'visible' entry.
  #'
  #' @return A numeric vector with the indices of visible columns.
  indices <- which(unlist(lapply(columnDefs, function(x) x$visible)))
  return(indices)
}

dataColumnIndices <- function(columnDefs) {
  #' Get indices of visible columns with names ending in ".data".
  #'
  #' This function returns the indices of visible columns in a list of column definitions
  #' where the column name ends with ".data".
  #'
  #' @param columnDefs A list of column definitions, each containing a 'visible' entry and a 'name' entry.
  #'
  #' @return A numeric vector with the indices of visible columns with names ending in ".data".
  indices <- which(unlist(lapply(columnDefs, function(x) x$visible & grepl("\\.data$", x$name))))
  return(indices)
}

nondataColumnIndices <- function(columnDefs) {
  #' Get indices of entries based on specific conditions and visibility.
  #'
  #' This function takes a list of named lists and returns the indices of all entries that:
  #' 1. do not end in ".data" in the "name" entry,
  #' 2. do not start with the prefix of any of the "name" entries ending in ".data",
  #' 3. have "visible" entry set to TRUE.
  #'
  #' @param named_lists A list of named lists, where each named list contains a "visible" entry and a "name" entry.
  #'
  #' @return A numeric vector with the indices of the entries meeting the specified conditions and with "visible" set to TRUE.
  # Extract names and visible entries
  names_vector <- sapply(columnDefs, function(x) x$name)
  visible_vector <- sapply(columnDefs, function(x) x$visible)

  # Identify the entries ending in ".data"
  data_suffix_entries <- grepl("\\.data$", names_vector)

  # Extract the prefixes of the entries ending in ".data"
  data_prefixes <- sub("\\.data$", "", names_vector[data_suffix_entries])

  # Check if each entry in the names_vector starts with any of the data_prefixes, doesn't end with ".data", and is visible
  indices <- which(!data_suffix_entries & !sapply(names_vector, function(x) any(sapply(data_prefixes, function(y) grepl(paste0("^", y), x)))) & visible_vector)
  indices <- unname(indices)

  return(indices)
}

generate_random_string <- function(length = 10) {
  #' Generate a random string.
  #'
  #' This function generates a random string of a specified length.
  #' The string is composed of lowercase letters, uppercase letters, and digits.
  #'
  #' @param length An integer indicating the length of the random string to be generated. Default is 10.
  #'
  #' @return A character string of the specified length containing random characters.
  characters <- c(letters, LETTERS, 0:9)
  random_string <- paste(sample(characters, length, replace = TRUE), collapse = "")
  return(random_string)
}


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
    concept_id <- vars$concept_id[i]


    local({
      plotname <- paste("plot", type, var, sep = "_")
      localvar <- var

      data <- load_data(person_id = person_id, concept_id = concept_id, start_date = min_dt, end_date = max_dt)

      output[[plotname]] <- renderPlotly({
        if (is.null(data) || nrow(data) == 0) {
          # No data case: Render an empty plot
          ggp <- ggplot() +
            geom_blank() +
            expand_limits(x = c(min_dt, max_dt), y = c(0, 1))
        } else if ("datetime" %in% colnames(data) && "value" %in% colnames(data) && !"end_datetime" %in% colnames(data)) {
          # Case 1: Dot and line plot (datetime + value)
          ggp <- ggplot(data, aes(x = datetime, y = value)) +
            geom_line(color = "blue") + # Connect the dots
            geom_point(size = 2) # Add dots
        } else if ("datetime" %in% colnames(data) && "end_datetime" %in% colnames(data) && !"value" %in% colnames(data)) {
          # Case 2: Horizontal bars for intervals (datetime + end_datetime without value)
          ggp <- ggplot(data, aes(y = factor(1))) +
            geom_tile(
              aes(
                x = as.POSIXct((as.numeric(datetime) + as.numeric(end_datetime)) / 2, origin = "1970-01-01"),
                width = as.numeric(difftime(end_datetime, datetime, units = "secs"))
              ),
              height = 0.5, fill = "darkred", alpha = 0.8
            ) +
            geom_point(aes(x = datetime), size = 3, color = "blue") + # Start point
            geom_point(aes(x = end_datetime), size = 3, color = "blue") + # End point
            xlab("Time Interval") +
            ylab("") +
            theme(
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_blank()
            )
        } else if ("datetime" %in% colnames(data) && "end_datetime" %in% colnames(data) && "value" %in% colnames(data)) {
          # Case 3: Horizontal "point and stick" lines (datetime + end_datetime + value)
          ggp <- ggplot(data, aes(y = value)) +
            geom_linerange(aes(xmin = datetime, xmax = end_datetime), linewidth = 1.2, color = "darkblue") +
            geom_point(aes(x = datetime), size = 2, color = "blue") + # Start point
            geom_point(aes(x = end_datetime), size = 2, color = "blue") + # End point
            xlab("Time Interval") +
            ylab("Value") +
            theme_minimal()
        } else {
          # Default: Handle unexpected cases gracefully with a fallback continuous plot
          ggp <- ggplot(data, aes(datetime, value)) +
            geom_line() +
            geom_point(size = 2, color = "blue")
        }

        # Add plotly interactivity and axis limits
        ggplotly(
          ggp +
            xlab("Date") +
            ylab(localvar) +
            coord_cartesian(xlim = c(min_dt, max_dt))
        )
      })
    })
  }
}
