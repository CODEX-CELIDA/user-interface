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
library(httr)
library(dotenv)
library(stringr)
library(readr)
library(dplyr)
library(lubridate)
# library(urltools)


base_url <- "http://localhost:8001"
# base_url <- Sys.getenv("UI_BACKEND_SERVER")

rec_map <- list(
  "recommendation_url" = c(
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/no-therapeutic-anticoagulation",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/sepsis/recommendation/ventilation-plan-ards-tidal-volume",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/ventilation-plan-ards-tidal-volume",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/covid19-ventilation-plan-ards-peep",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/prophylactic-anticoagulation",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/therapeutic-anticoagulation",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/covid19-abdominal-positioning-ards"
  ),
  "short" = c("No ACT", "Sepsis/Tidal", "C19/Tidal", "PEEP", "p-ACT", "t-ACT", "Proning")
) %>% as_tibble()

load_recommendations <- function() {
  #' Load Recommendations
  #'
  #' This function retrieves a list of recommendations from a specified endpoint and converts the response into a tibble data frame.
  #' The response is joined with a pre-defined mapping data frame 'rec_map', by the column 'recommendation_url'.
  #'
  #' @return A tibble data frame containing the recommendations with added columns from the mapping data frame.
  #' @export

  req <- GET(paste0(base_url, "/recommendation/list"))
  recommendations <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8")) %>%
    as_tibble() %>%
    inner_join(rec_map, by = "recommendation_url")
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

  patients <- tibble()

  if (is.null(selected_recommendation_urls)) {
    return(patients)
  }

  for (recommendation_url in selected_recommendation_urls) {
    req <- GET(paste0(base_url, "/patient/list/?recommendation_url=", URLencode(recommendation_url), "&start_datetime=", URLencode(as.character(start_datetime)), "&end_datetime=", URLencode(as.character(end_datetime))))

    data <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8"))
    run_id <- data$run_id
    pat_data <- data$data %>%
      as_tibble() %>%
      mutate(run_id = run_id, url = recommendation_url)
    patients <- bind_rows(patients, pat_data)
  }

  run_ids <- patients %>% distinct(run_id, url)

  if (nrow(patients) > 0) {
    patients <- patients %>%
      # make up a ward
      mutate(ward = as.factor(sprintf("ITS %02d", (person_id %% 3) + 1))) %>%
      inner_join(rec_map %>% rename(url = recommendation_url), by = "url") %>%
      pivot_wider(id_cols = c("person_id", "ward"), names_from = "short", values_from = "cohort_category", values_fn = summarize_category) %>%
      arrange(person_id) %>%
      mutate(Name = person_id, Ward = ward)
  }

  patients <- patients %>%
    mutate_at(all_of(rec_map$short), ~ round(runif(nrow(patients), 0, 100)))
  # mutate_at(all_of(rec_map$short), recode, "P"="✘", "PI"="✔", "o"="(✘)", "I"="(✔)") %>%
  # mutate_at(all_of(rec_map$short), as.factor)


  stats <- patients %>%
    select(all_of(rec_map$short)) %>%
    mutate(across(everything(), ~ if_else(. == "P", 1, if_else(. == "PI", -1, 0)))) %>%
    summarize(across(everything(), ~ sum(. == 1) / sum(. != 0))) %>%
    mutate(across(everything(), ~ if_else(is.na(.), 0, .)))

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

  req <- GET(paste0(base_url, "/recommendation/criteria/?recommendation_url=", URLencode(recommendation_url)))
  data <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8"))
  criteria <- data$criterion %>%
    as_tibble() %>%
    rename(type = cohort_category, variable_name = concept_name, criterion_name = unique_name)

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

  req <- GET(paste0(base_url, "/patient/data/?person_id=", URLencode(as.character(person_id)), "&run_id=", URLencode(as.character(run_id)), "&criterion_name=", URLencode(criterion_name)))

  if (req$status_code != 200) {
    stop("Error encountered during load_data")
  }

  patientdata <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8")) %>% as_tibble()

  patientdata <- patientdata %>%
    rename(datetime = start_datetime) %>%
    arrange(datetime)

  if (("value_as_number" %in% names(patientdata))) {
    patientdata <- patientdata %>%
      rename(value = value_as_number)
  } else if ("drug_dose_as_number" %in% names(patientdata)) {
    patientdata <- patientdata %>%
      rename(value = drug_dose_as_number)
  } else {
    patientdata <- patientdata %>%
      mutate(value = TRUE)
  }

  if (!("end_datetime" %in% names(patientdata))) {
    patientdata <- patientdata %>%
      mutate(end_datetime = datetime)
  }

  if (nrow(patientdata) > 0) {
    patientdata <- patientdata %>%
      mutate(end_datetime = coalesce(end_datetime, datetime)) %>%
      mutate(datetime = parse_datetime(datetime)) %>%
      mutate(end_datetime = parse_datetime(end_datetime))
  }

  return(patientdata)
}
