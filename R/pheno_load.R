#' Load Phenology Data from CSV
#'
#' This function reads a CSV file and creates a phenology object.
#'
#' @param filename The path to the CSV file containing iNaturalist occurrences.
#' @return A phenology object containing:
#'   \item{name}{The scientific name of the species.}
#'   \item{occurrences}{A data table of occurrences with selected columns: `latitude`, `longitude`, `time_observed_at`, `positional_accuracy`.}
#'   \item{median_day}{Median day of the year (1 to 365) of observations.}
#'   \item{yearly_counts}{A list indicating the number of occurrences for each year represented.}
#' @export
#' @import data.table
pheno_load <- function(filename) {
  data <- fread(filename)

  # Validate required columns
  required_cols <- c("scientific_name", "latitude", "longitude", "time_observed_at", "positional_accuracy")
  if (!all(required_cols %in% names(data))) {
    stop("The CSV file must contain the following columns: ", paste(required_cols, collapse = ", "))
  }

  # Create phenology object
  phenology_obj <- phenology(data)

  return(phenology_obj)
}
