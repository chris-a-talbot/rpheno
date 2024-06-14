#' Create a Phenology Object
#'
#' This function creates a phenology object from a data frame of iNaturalist occurrences.
#'
#' @param data A data frame containing iNaturalist occurrences with columns: `scientific_name`, `latitude`, `longitude`, `time_observed_at`, `positional_accuracy`.
#'
#' @return A phenology object containing:
#'   \item{name}{The scientific name of the species.}
#'   \item{occurrences}{A data table of occurrences with selected columns: `latitude`, `longitude`, `time_observed_at`, `positional_accuracy`.}
#'   \item{median_day}{Median day of the year (1 to 365) of observations.}
#'   \item{yearly_counts}{A list indicating the number of occurrences for each year represented.}
#' @export
#' @import data.table
phenology <- function(data) {
  data <- as.data.table(data)
  scientific_names <- unique(data$scientific_name)
  if (length(scientific_names) != 1) {
    stop("Multiple scientific names found. Please provide data for a single species.")
  }
  name <- scientific_names[1]

  occurrences <- data[, .(latitude, longitude, time_observed_at, positional_accuracy)]
  occurrences[, day_of_year := as.integer(format(as.Date(time_observed_at), "%j"))]
  median_day <- median(occurrences$day_of_year, na.rm = TRUE)
  occurrences[, year := as.integer(format(as.Date(time_observed_at), "%Y"))]
  yearly_counts <- as.list(table(occurrences$year))

  phenology_obj <- list(
    name = name,
    occurrences = occurrences,
    median_day = median_day,
    yearly_counts = yearly_counts
  )
  class(phenology_obj) <- "phenology"
  return(phenology_obj)
}

#' Print a Phenology Object
#'
#' Print method for objects of class `phenology`.
#'
#' @param obj A phenology object.
#' @method print phenology
#' @export
print.phenology <- function(obj) {
  cat("Phenology object for:", obj$name, "\n")
  cat("Earliest day of year:", min(obj), "\n")
  cat("Latest day of year:", max(obj), "\n")
  cat("Median day of year:", obj$median_day, "\n")
  cat("Yearly occurrence counts:\n")
  print(obj$yearly_counts)
  cat("\nOccurrences data:\n")
  print(head(obj$occurrences))
}

#' Summary of a Phenology Object
#'
#' Summary method for objects of class `phenology`.
#'
#' @param obj A phenology object.
#' @method summary phenology
#' @export
summary.phenology <- function(obj) {
  cat("Summary of phenology object for:", obj$name, "\n")
  cat("Earliest day of year:", min(obj), "\n")
  cat("Latest day of year:", max(obj), "\n")
  cat("Median day of year:", obj$median_day, "\n")
  cat("Yearly occurrence counts:\n")
  print(obj$yearly_counts)
}

#' Subset a Phenology Object
#'
#' Subset method for objects of class `phenology`.
#'
#' This method allows subsetting of a phenology object using standard bracket notation.
#'
#' @param obj A phenology object.
#' @param subset_expr An expression to subset the occurrences data table within the phenology object.
#' @return A phenology object updated with only occurrences matching the subset expression.
#' @export
`[.phenology` <- function(obj, subset_expr) {
  # Evaluate the subsetting expression within the context of the occurrences data.table
  subset_occurrences <- obj$occurrences[eval(substitute(subset_expr), obj$occurrences, parent.frame())]

  # Recalculate yearly counts
  yearly_counts <- as.list(table(subset_occurrences$year))

  # Create a new phenology object with the subsetted data
  subset_obj <- list(
    name = obj$name,
    occurrences = subset_occurrences,
    median_day = median(subset_occurrences$day_of_year, na.rm = TRUE),
    yearly_counts = yearly_counts
  )

  class(subset_obj) <- "phenology"
  return(subset_obj)
}

#' Get the earliest day of the year from a phenology object
#'
#' @param obj A phenology object.
#' @return The earliest day of the year (1 to 365) of observations.
#' @method min phenology
#' @export
min.phenology <- function(obj) {
  days <- obj$occurrences$day_of_year
  return(min(days))
}

#' Get the latest day of the year from a phenology object
#'
#' @param obj A phenology object.
#' @return The latest day of the year (1 to 365) of observations.
#' @method max phenology
#' @export
max.phenology <- function(obj) {
  days <- obj$occurrences$day_of_year
  return(max(days))
}
