#' Get Occurrences of a Species from iNaturalist given a phenophase
#'
#' This function retrieves occurrences of a specified species from iNaturalist within defined spatial and temporal bounds, filtering by phenophase and other criteria.
#'
#' @param species Character string specifying the species name to query.
#' @param spatial_bounds Numeric vector of length 4 specifying the spatial bounds for the query in the form \code{c(latitude_min, longitude_min, latitude_max, longitude_max)}. Default is \code{c(15, -125, 65, -60)}.
#' @param year_bounds Numeric vector of length 2 specifying the year bounds for the query. Default is \code{c(2015, 2024)}.
#' @param phenophase Character string specifying the phenophase to filter occurrences. Must be one of \code{"flowering"}, \code{"fruiting"}, \code{"budding"}, or \code{"none"}. Default is \code{"flowering"}.
#' @param max_results Integer specifying the maximum number of results to retrieve. Default is 10000.
#' @param min_results Integer specifying the minimum number of results required. If fewer results are found, the function will stop with an error. Default is 3.
#' @param write Character string specifying the path to write the results to a CSV file. If \code{NULL}, the results are not written to a file. Default is \code{NULL}.
#'
#' @return A \code{data.table} containing the occurrences of the specified species, with an additional column for the year of observation.
#'
#' @details The function queries the iNaturalist database for occurrences of the specified species within the given spatial and temporal bounds, filtered by the specified phenophase. The results are filtered to include only research-grade, georeferenced observations. If the number of occurrences found is less than \code{min_results}, the function stops with an error. If a file path is provided to \code{write}, the results are written to a CSV file.
#'
#' @examples
#' \dontrun{
#' # Get occurrences of "Abutilon theophrasti" flowering from 2015 to 2024
#' occurrences <- get_occurrences("Abutilon theophrasti")
#'
#' # Get occurrences of "Abutilon theophrasti" fruiting from 2015 to 2024 and write to a file
#' occurrences <- get_occurrences("Abutilon theophrasti", phenophase="fruiting", write="occurrences.csv")
#' }
#'
#' @importFrom data.table data.table tstrsplit fwrite
#' @importFrom rinat get_inat_obs
#' @importFrom stringr word
#' @export
phenoccurrences <- function(species, spatial_bounds=c(15,-125,65,-60),
                                year_bounds=c(2015, 2024), phenophase="flowering",
                                max_results=10000, min_results=3, write=NULL) {

  if(phenophase == "flowering") {
    pheno_id = 13
  } else if(phenophase == "fruiting") {
    pheno_id = 14
  } else if(phenophase == "budding") {
    pheno_id = 15
  } else if(phenophase == "none") {
    pheno_id = 21
  } else {
    stop("Invalid phenophase. Must be 'flowering', 'fruiting', 'budding', or 'none'.")
  }

  # Get the iNaturalist occurrences
  obs = data.table(get_inat_obs(taxon_name=species, annotation=c(12,pheno_id),
                                maxresults=max_results, bounds=spatial_bounds,
                                quality="research", geo=TRUE))

  # Add a year column and filter by specified year boundaries
  obs[, year := tstrsplit(observed_on, "-", fixed = TRUE)[[1]]]
  obs[, year := as.numeric(year)]

  # Ensure the 'coordinates_obscured' and 'captive_cultivated' columns are character
  obs[, coordinates_obscured := as.character(coordinates_obscured)]
  obs[, captive_cultivated := as.character(captive_cultivated)]

  # Filter the observations
  obs <- obs[
    year >= year_bounds[1] & year <= year_bounds[2] &
      coordinates_obscured != "true" &
      captive_cultivated != "true" &
      num_identification_disagreements <= 0 &
      time_observed_at != ""
  ]

  obs$scientific_name <- word(obs$scientific_name, 1, 2, sep=" ")

  if(nrow(obs) < min_results) {
    stop("Not enough occurrences found.")
  }

  if(!is.null(write)) {
    fwrite(obs, write)
  }

  return(obs)
}
