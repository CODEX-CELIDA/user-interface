load_recommendations <- function() {
  #' Load Recommendations
  #'
  #' This function retrieves a list of recommendations from a specified endpoint and converts the response into a tibble data frame.
  #' The response is joined with a pre-defined mapping data frame 'rec_map', by the column 'recommendation_url'.
  #'
  #' @return A tibble data frame containing the recommendations with added columns from the mapping data frame.
  #' @export

  recommendations <- read_csv("data/recommendations.csv", show_col_types = FALSE)
  return(recommendations)
}

recommendations <- load_recommendations()


summarize_category <- function(categories) {
  #' Summarize categories
  #'
  #' This function takes a character vector of categories and returns a summarized representation based on the presence of 'population_intervention', 'population', or 'intervention' categories.
  #'
  #' @param categories A character vector of categories.
  #' @return A character string representation of the summarized category: "PI" for 'population_intervention', "P" for 'population', "I" for 'intervention', and "o" for other categories.
  #' @examples
  #' categories <- c("population_intervention", "other", "another")
  #' summarize_category(categories) # returns "PI"
  #'
  if ("population_intervention" %in% categories) {
    return("PI")
  } else if ("population" %in% categories) {
    return("P")
  } else if ("intervention" %in% categories) {
    return("I")
  } else {
    return("o")
  }
}

load_patient_list <- function(selected_recommendation_urls, start_datetime, end_datetime) {
  #' Load a list of patients based on selected recommendations and time period
  #'
  #' This function loads a list of patients based on selected recommendations and time period.
  #'
  #' @param selected_recommendation_urls character vector of recommendation urls to be used
  #' @param start_datetime Datetime for the start of the time period
  #' @param end_datetime Datetime for the end of the time period
  #'
  #' @return A list containing patients data in tibble format and run_ids in tibble format.
  #'
  #' @examples
  #' result <- load_patient_list(c("recommendation1","recommendation2"), "2021-01-01", "2021-01-31")
  #' patients <- result$patients
  #' run_ids <- result$run_id
  #'

  patients <- read_csv("data/patients.csv", show_col_types = FALSE)
  run_ids <- read_csv("data/run_ids.csv", show_col_types = FALSE)
  stats <- read_csv("data/stats.csv", show_col_types = FALSE)

  return(list(patients = patients, run_id = run_ids, stats = stats))
}

load_recommendation_variables <- function(recommendation_url) {
  #' Load Recommendation Variables
  #'
  #' The function `load_recommendation_variables()` retrieves the criteria information for a specified recommendation URL.
  #'
  #' @param recommendation_url Character string of the recommendation URL
  #'
  #' @return A tibble with columns `type`, `variable_name`, and `criterion_name`
  #'
  #' @examples
  #' criteria <- load_recommendation_variables("www.example.com/recommendation/1234")

  criteria <- read_csv("data/criteria.csv", show_col_types = FALSE)

  return(criteria)
}

load_data <- function(person_id, run_id, criterion_name) {
  #' Load patient data based on person_id, run_id, and criterion_name
  #'
  #' @param person_id character string identifying a person
  #' @param run_id character string identifying a run
  #' @param criterion_name character string identifying a criterion
  #'
  #' @return a tibble with patient data, arranged by datetime. Columns may include:
  #'   - `datetime`: start datetime of the patient data
  #'   - `end_datetime`: end datetime of the patient data (defaults to `datetime` if not present)
  #'   - `value`: value of the patient data, renamed from `value_as_number` or `drug_dose_as_number` if present
  #'
  #' @examples
  #' patientdata <- load_data("12345", "run1", "criterion_a")
  #'
  #' @export
  #'
  if (is.null(person_id) | length(person_id) == 0) {
    return(NULL)
  }

  patientdata <- read_csv("data/patientdata.csv", show_col_types = FALSE)

  return(patientdata)
}
