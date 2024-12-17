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

OFFLINE <- nchar(Sys.getenv("OFFLINE")) > 0
OFFLINE_PATH <- "offline-data/"

base_url <- "http://localhost:8081"
# base_url <- Sys.getenv("UI_BACKEND_SERVER")

rec_map <- list(
  "recommendation_url" = c(
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/no-therapeutic-anticoagulation",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/sepsis/recommendation/ventilation-plan-ards-tidal-volume",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/ventilation-plan-ards-tidal-volume",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/covid19-ventilation-plan-peep",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/prophylactic-anticoagulation",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/therapeutic-anticoagulation",
    "https://www.netzwerk-universitaetsmedizin.de/fhir/codex-celida/guideline/covid19-inpatient-therapy/recommendation/covid19-abdominal-positioning-ards"
  ),
  "short" = c("No ACT", "Sepsis/Tidal", "C19/Tidal", "PEEP", "p-ACT", "t-ACT", "Proning")
) %>% as_tibble()


COLUMN_SUFFIXES <- c(".data", ".days", ".comment")

read_offline_data <- function(name) {
  return(read_csv(paste0(OFFLINE_PATH, name, ".csv"), show_col_types = FALSE))
}


expand_colnames <- function(colnames, suffixes = COLUMN_SUFFIXES) {
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

  if (!OFFLINE) {
    req <- GET(paste0(base_url, "/execution_run/list"))

    recommendations <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8")) %>%
      as_tibble() %>%
      inner_join(rec_map, by = "recommendation_url")
  } else {
    recommendations <- read_offline_data("recommendations")
  }

  # write_csv(recommendations, "data-new/recommendations.csv")
  return(recommendations)
}

recommendations <- load_recommendations()

load_patient_list <- function(selected_recommendation_urls, start_datetime, end_datetime, n_days = 10) {
  #' Load a list of patients based on selected recommendations and time period
  #'
  #' @param selected_recommendation_urls character vector of recommendation URLs to be used
  #' @param start_datetime Datetime for the start of the time period
  #' @param end_datetime Datetime for the end of the time period
  #' @param n_days integer, number of days for which data is constructed (default = 10)
  #'
  #' @return A list with two elements:
  #'   - patients: A tibble containing patient-level data with the following structure:
  #'       - person_id (integer): The unique patient ID.
  #'       - Ward (factor): Simulated ward assignment for the patient (e.g., "ITS 01", "ITS 02").
  #'       - Patient (integer): Alias for the person ID.
  #'       - <short>.days (character): A 10-character string where each position represents a day.
  #'         - "0" = No data for the day.
  #'         - "1" = Population cohort data.
  #'         - "2" = Population + Intervention cohort data.
  #'       - <short>.data (numeric): Percentage of days with "2" relative to the sum of "1" and "2".
  #'       - <short>.comment (logical): Randomly generated TRUE/FALSE values for comments.
  #'
  #'       Columns labeled <short> correspond to the recommendation short names from `rec_map`.
  #'
  #'   - run_id: A tibble containing unique run IDs with the following structure:
  #'       - run_id (integer): The unique run identifier for a recommendation.
  #'       - url (character): The recommendation URL associated with the run.
  #'
  #' @examples
  #' result <- load_patient_list(c("recommendation1", "recommendation2"), "2024-01-01", "2024-01-10")
  #' patients <- result$patients
  #' run_ids <- result$run_id
  #'
  #' @export


  if (is.null(selected_recommendation_urls)) {
    return(list(patients = tibble(), run_id = tibble()))
  }

  patients <- tibble()
  rec_short_names <- rec_map$short

  if (!OFFLINE) {
    recommendations <- tibble()


    # 1. Get the latest run_id for each recommendation URL
    req <- GET(paste0(base_url, "/execution_run/list"))
    runs <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8")) %>%
      as_tibble() %>%
      filter(recommendation_url %in% selected_recommendation_urls) %>%
      arrange(recommendation_url, desc(run_datetime))

    # Get the latest run_id for each recommendation_url
    recommendations <- runs %>%
      group_by(recommendation_url) %>%
      slice(1) %>%
      ungroup()



    # 2. Fetch full day coverage for each run_id and construct .days strings
    for (i in 1:nrow(recommendations)) {
      run_id <- recommendations$run_id[i]
      short_name <- rec_map$short[rec_map$recommendation_url == recommendations$recommendation_url[i]]

      # Call full_day_coverage endpoint
      req <- GET(paste0(
        base_url, "/full_day_coverage/", run_id,
        "?valid_date=", URLencode(as.character(end_datetime)),
        "&n_days=", n_days
      ))

      coverage_data <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8"))

      # Construct .days string for each person_id
      day_sequence <- seq.Date(as.Date(end_datetime) - n_days + 1, as.Date(end_datetime), by = "day")
      patient_days <- coverage_data %>%
        group_by(person_id) %>%
        summarise(
          days = paste(sapply(day_sequence, function(d) {
            if (any(valid_date == as.character(d) & cohort_category == "POPULATION_INTERVENTION")) {
              return("2")
            } else if (any(valid_date == as.character(d) & cohort_category == "POPULATION")) {
              return("1")
            } else {
              return("0")
            }
          }), collapse = "")
        ) %>%
        mutate(run_id = run_id, url = recommendations$recommendation_url[i])

      patients <- bind_rows(patients, patient_days)
    }
  } else {
    patients <- read_offline_data("patients")
  }

  # 3. Combine data and construct output
  run_ids <- patients %>% distinct(run_id, url)

  if (nrow(patients) > 0) {
    # Add Ward and reshape data with ".days" suffix for short names
    patients <- patients %>%
      mutate(Ward = as.factor(sprintf("ITS %02d", (person_id %% 3) + 1))) %>%
      inner_join(rec_map %>% rename(url = recommendation_url), by = "url") %>%
      pivot_wider(
        id_cols = c("person_id", "Ward"),
        names_from = "short",
        values_from = "days",
        names_glue = "{.name}.days" # Append ".days" to the column names
      ) %>%
      arrange(person_id) %>%
      mutate(Patient = person_id)

    # Define percentage calculation function
    calculate_percentage <- function(input_vector) {
      # Vectorized version to handle each string in the input vector
      sapply(input_vector, function(input_string) {
        split <- strsplit(input_string, "")[[1]]
        count_1 <- sum(split == "1")
        count_2 <- sum(split == "2")
        if ((count_1 + count_2) == 0) {
          return(NaN)
        } # Avoid division by zero
        round(count_2 / (count_1 + count_2) * 100, 2)
      })
    }

    # Dynamically identify ".days" columns
    existing_days_columns <- grep("\\.days$", colnames(patients), value = TRUE)

    # Step 1: Generate .data columns based on .days columns
    patients <- patients %>%
      mutate(across(
        all_of(existing_days_columns),
        ~ calculate_percentage(.x), # Apply percentage calculation
        .names = "{.col}.data"
      ))

    # Step 2: Generate .comment columns with random TRUE/FALSE values
    patients <- patients %>%
      mutate(across(
        all_of(existing_days_columns),
        ~ runif(nrow(patients)) > 0.9, # Generate random TRUE/FALSE
        .names = "{.col}.comment"
      ))

    # Step 3: Clean up column names by replacing ".days" in .data and .comment columns
    patients <- patients %>%
      rename_with(~ gsub("\\.days\\.data$", ".data", .), ends_with(".days.data")) %>%
      rename_with(~ gsub("\\.days\\.comment$", ".comment", .), ends_with(".days.comment"))
  } else {
    patients <- bind_cols(
      tibble(Patient = character(), person_id = character(), Ward = character(), .rows = 0),
      tibble(!!!existing_rec_short_names, .rows = 0, .name_repair = ~existing_rec_short_names)
    )
  }

  result <- list(patients = patients, run_id = run_ids)
  return(result)
}


# t_days<-load_patient_list(rec_map$recommendation_url, start_datetime="2023-01-01", end_datetime="2023-04-03")



load_recommendation_variables <- function(run_id, person_id, start_datetime, end_datetime) {
  #' Load Recommendation Variables
  #'
  #' The function `load_recommendation_variables()` retrieves unique concept_id and type pairs
  #' for a specified run ID and person ID within a given time range.
  #'
  #' @param run_id Integer ID for the recommendation run
  #' @param person_id Integer ID for the person
  #' @param start_datetime Datetime specifying the start of the time range
  #' @param end_datetime Datetime specifying the end of the time range
  #'
  #' @return A tibble with unique columns `concept_id` and `type`
  #'
  #' @examples
  #' criteria <- load_recommendation_variables(1234, 5678, "2024-01-01", "2024-01-31")

  if (!OFFLINE) {
    req <- GET(paste0(
      base_url, "/criteria/", run_id, "/", person_id,
      "?start_datetime=", URLencode(as.character(start_datetime)),
      "&end_datetime=", URLencode(as.character(end_datetime))
    ))

    # Parse the response
    data <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8"))

    # Convert to tibble, rename columns, and filter for unique concept_id - type pairs
    criteria <- data %>%
      as_tibble() %>%
      rename(
        concept_id = concept_id, # Ensure concept_id is exported
        type = cohort_category, # Map cohort_category to type
        variable_name = concept_name
      ) %>%
      select(concept_id, type, variable_name) %>% # Keep only the required columns
      mutate(type = tolower(type)) %>% # Convert 'type' column to lowercase
      distinct() # Retain only unique concept_id-type pairs
  } else {
    # Load offline data and filter by run_id
    criteria <- read_offline_data("criteria") %>%
      filter(run_id == !!run_id & person_id == !!person_id) %>%
      select(concept_id, cohort_category) %>%
      rename(type = cohort_category) %>%
      distinct()
  }

  # Return the final tibble
  return(criteria)
}


load_data <- function(person_id, concept_id, start_datetime, end_datetime) {
  #' Load patient data based on person_id, run_id, and criterion_name
  #'
  #' @param person_id character string identifying a person
  #' @param concept_id character string identifying a criterion
  #' @param start_date date of the beginning of the observation window
  #' @param end_date date of the end of the observation window
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


  if (is.null(person_id) | length(person_id) == 0) {
    return(NULL)
  }

  if (OFFLINE) {
    tbl <- generate_tibble(person_id, start_datetime, end_datetime, criterion_name)
    return(tbl)
  }

  req <- GET(paste0(
    base_url,
    "/person/", URLencode(as.character(person_id)),
    "/data?concept_id=", URLencode(as.character(concept_id)),
    "&start_datetime=", URLencode(as.character(start_datetime)),
    "&end_datetime=", URLencode(as.character(end_datetime))
  ))

  if (req$status_code != 200) {
    stop("Error encountered during load_data")
  }

  patientdata <- jsonlite::fromJSON(content(req, as = "text", encoding = "UTF-8")) %>% as_tibble()

  if (nrow(patientdata) == 0) {
    # empty response
    return(tibble(
      datetime = as.POSIXct(character(), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      end_datetime = as.POSIXct(character(), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ))
  }

  patientdata <- patientdata %>%
    rename(datetime = start_datetime) %>%
    arrange(datetime)

  if (("end_datetime" %in% names(patientdata))) {
    patientdata <- patientdata %>%
      mutate(end_datetime = coalesce(end_datetime, datetime)) %>%
      mutate(end_datetime = parse_datetime(end_datetime))
  }

  patientdata <- patientdata %>%
    mutate(datetime = parse_datetime(datetime))


  # Parse "value" column into float if it exists
  if ("value" %in% names(patientdata)) {
    patientdata <- patientdata %>%
      mutate(value = as.numeric(value))
  }

  # write_csv(patientdata, glue("data-new/patientdata-{person_id}.csv"))

  return(patientdata)
}


generate_tibble <- function(person_id, start_date, end_date, criterion_name) {
  # seed the random number generator
  md5_hash <- digest::digest(criterion_name, "md5")
  set.seed(as.integer(person_id) + strtoi(substr(md5_hash, 1, 5), 16))

  # initialize the parameter_concept_id to 0
  parameter_concept_id <- 0

  # calculate the number of days in the observation period
  num_days <- as.integer(difftime(end_date, start_date, units = "days"))

  type_range <- TRUE

  # determine the type of the criterion and number of entries per day
  if (any(startsWith(criterion_name, c("Measurement_", "TidalVolumePerIdealBodyWeight_", "ConceptCriterion_")))) {
    type_range <- FALSE
    if (grepl("aPTT", criterion_name)) {
      entries_per_day <- sample(4:6, 1)
    } # aPTT: 4-6 times per day
    else if (grepl("Tidal-volume", criterion_name)) {
      entries_per_day <- sample(24:24 * 4, 1)
    } # Tidal volume: 1-2 times per day
    else if (grepl("D-dimer", criterion_name)) {
      entries_per_day <- sample(1:2, 1)
    } # D-dimer: 1-2 times per day
    else if (grepl("PEEP", criterion_name)) {
      entries_per_day <- sample(24:24 * 4, 1)
    } # PEEP: 1-4 times per day
    else if (grepl("Body-weight", criterion_name)) {
      entries_per_day <- 1 / num_days
    } # Body weight: once per day
    else if (grepl("Inhaled-oxygen-concentration", criterion_name)) {
      entries_per_day <- sample(24:24 * 4, 1)
    } # Inhaled oxygen concentration: 1-2 times per day
    else if (grepl("Horowitz-index", criterion_name)) {
      entries_per_day <- sample(24:24 * 4, 1)
    } # Horowitz index: 1-2 times per day
    else if (grepl("Pressure-max", criterion_name)) entries_per_day <- sample(24:24 * 4, 1) # Maximal pressure during respiration: 1-4 times per day
  } else if (startsWith(criterion_name, "DrugExposure_")) {
    entries_per_day <- sample(0:2, 1) # drug exposures: 0 to 2 times per day
  } else if (any(startsWith(criterion_name, c("ProcedureOccurrence_")))) {
    entries_per_day <- sample(0:1, 1) # occurrences: 0 to 1 times per day
  } else if (any(startsWith(criterion_name, c("ConditionOccurrence_", "VisitOccurrence_")))) {
    entries_per_day <- sample(0:1, 1) / num_days # occurrences: 0 to 1 times per stay
  } else {
    stop("No entry found for criterion_name = ", criterion_name)
  }

  # calculate the total number of entries
  num_entries <- num_days * entries_per_day

  # generate datetime and end_datetime
  datetimes <- sample(seq(as.POSIXct(start_date), as.POSIXct(end_date), by = "min"), num_entries)
  if (num_entries == 1) {
    sample_range <- 300:3000
  } else {
    sample_range <- 5:720
  }

  end_datetimes <- datetimes

  if (type_range) {
    end_datetimes <- end_datetimes + as.difftime(sample(sample_range, num_entries, replace = TRUE), units = "mins") # random duration for occurrences
  }

  # generate values
  if (any(startsWith(criterion_name, c("Measurement_", "TidalVolumePerIdealBodyWeight_", "ConceptCriterion_")))) {
    if (grepl("aPTT", criterion_name)) {
      values <- runif(num_entries, 20, 40)
    } # adjust as per clinically plausible values
    else if (grepl("Tidal-volume", criterion_name)) {
      values <- runif(num_entries, 5, 10)
    } else if (grepl("D-dimer", criterion_name)) {
      values <- runif(num_entries, 0, 0.5)
    } else if (grepl("PEEP", criterion_name)) {
      values <- runif(num_entries, 5, 20)
    } else if (grepl("Body-weight", criterion_name)) {
      values <- runif(num_entries, 50, 100)
    } else if (grepl("Inhaled-oxygen-concentration", criterion_name)) {
      values <- runif(num_entries, 21, 100)
    } else if (grepl("Horowitz-index", criterion_name)) {
      values <- runif(num_entries, 200, 500)
    } else if (grepl("Pressure-max", criterion_name)) values <- runif(num_entries, 10, 30)
  } else if (startsWith(criterion_name, "DrugExposure_")) {
    values <- runif(num_entries, 0.1, 1.0) # adjust as per clinically plausible values
  } else if (any(startsWith(criterion_name, c("ConditionOccurrence_", "ProcedureOccurrence_", "VisitOccurrence_")))) {
    values <- rep(1, num_entries)
  }

  # create a tibble
  df <- tibble(
    person_id = rep(person_id, num_entries),
    parameter_concept_id = rep(parameter_concept_id, num_entries),
    datetime = datetimes,
    end_datetime = end_datetimes,
    value = values
  ) %>% arrange(datetime)

  return(df)
}
