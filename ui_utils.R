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


# Define a function to generate a random string
generate_random_string <- function(length = 10) {
  characters <- c(letters, LETTERS, 0:9)
  random_string <- paste(sample(characters, length, replace = TRUE), collapse = "")
  return(random_string)
}

