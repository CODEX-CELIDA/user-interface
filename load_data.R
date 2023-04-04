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


COLUMN_SUFFIXES <- c(".data", ".days", ".comment")


expand_colnames <- function(colnames, suffixes=COLUMN_SUFFIXES) {
  #' Expand Column Names with Custom Suffixes
  #'
  #' This function takes a character vector of column names and a character vector
  #' of suffixes, and expands the column names by appending each suffix to each
  #' column name. The expanded column names are returned in a specific order,
  #' with each group of suffixes appearing together in the sequence.
  #'
  #' @param colnames A character vector containing the original column names
  #' @param suffixes A character vector containing the custom suffixes to be
  #'        appended to the column names
  #'
  #' @return A character vector containing the expanded column names in the
  #'         specified order, with each group of suffixes appearing together
  #'         in the sequence
  #'
  #' @examples
  #' ## Expand a vector of column names with custom suffixes
  #' original_colnames <- c("A", "B")
  #' custom_suffixes <- c(".data", ".days", ".comment")
  #' expanded_colnames <- expand_colnames(original_colnames, custom_suffixes)
  #' print(expanded_colnames)
  
  ordered_colnames <- unname(unlist(lapply(colnames, function(x) sapply(suffixes, function(s) paste0(x, s)))))
  
  return(ordered_colnames)
}


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
  rec_short_names <- rec_map$short

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
      # make up a ward TODO: should be real ward
      mutate(Ward = as.factor(sprintf("ITS %02d", (person_id %% 3) + 1))) %>%
      inner_join(rec_map %>% rename(url = recommendation_url), by = "url") %>%
      pivot_wider(id_cols = c("person_id", "Ward"), names_from = "short", values_from = "cohort_category", values_fn = summarize_category) %>%
      arrange(person_id) %>%
      mutate(Patient = person_id)
  } else {
    # no patients received - return a valid tibble but without rows
    patients <- bind_cols(
      tibble(Patient=character(), person_id=character(), Ward=character(), .rows=0),
      tibble(!!!rec_short_names, .rows=0, .name_repair = ~ rec_short_names)
    )
  }


  # make up comment data
  t_comment <- patients %>%
    mutate_at(all_of(rec_short_names), ~ runif(nrow(patients)) > 0.5)
  
  # make up day data
  generate_random_strings <- function(x, n) {
    random_strings <- sapply(1:x, function(i) {
      random_digits <- sample(0:2, n, replace = TRUE)
      random_string <- paste0(random_digits, collapse = "")
      return(random_string)
    })
    return(random_strings)
  }
  
  n_days <- 10
  t_days <- patients %>%
    mutate_at(vars(all_of(rec_short_names)), ~ generate_random_strings(length(.), n_days))
  
  # determine percentage data
  percentage <- function(input_vector) {
    result <- sapply(input_vector, function(input_string) {
    split <- strsplit(input_string, "")[[1]]
    
    # Count the occurrences of 1 and 2
    count_1 <- sum(split == "1")
    count_2 <- sum(split == "2")
    
    # Divide the number of 2s by the sum of the number of 1s and 2s
    result <- round(count_2 / (count_1 + count_2) * 100, 2)
    
    return(result)
    })
    
    return(result)
  }
  
  t_percentage <- t_days %>% mutate(across(all_of(rec_short_names), percentage))
  
  # combine days, percentage and comment data into a single tibble
  t_percentage <- t_percentage %>% rename_with(~paste0(., ".data"))
  t_days <- t_days %>% rename_with(~paste0(., ".days"))
  t_comment <- t_comment %>% rename_with(~paste0(., ".comment"))
  
  # Combine tibbles
  combined_tibble <- bind_cols(t_percentage, t_days, t_comment)
  
  # Generate the desired order of column names
  expanded_colnames <- expand_colnames(rec_short_names)
  
  # Reorder columns
  ordered_tibble <- combined_tibble %>% select(all_of(expanded_colnames))
  
  result <- bind_cols(patients %>% select(-all_of(rec_short_names)), ordered_tibble)
  return(list(patients = result, run_id = run_ids))
}

#t_days<-load_patient_list(rec_map$recommendation_url, start_datetime="2023-01-01", end_datetime="2023-04-03")



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
