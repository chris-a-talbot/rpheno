#' Check for Phenological Overlap Between Two Species
#'
#' This function determines if two species have any overlap in their range of days of year. By default, it uses the full phenology object. It can also evaluate if there is any phenological overlap specifically within a specified latitude range.
#'
#' @param species1_data A data frame containing phenology data for the first species with columns 'day_of_year' and 'latitude'.
#' @param species2_data A data frame containing phenology data for the second species with columns 'day_of_year' and 'latitude'.
#' @param lat_range A numeric vector of length 2 specifying the latitude range (c(lat_min, lat_max)) to restrict the analysis. Default is NULL, meaning the full data is used.
#' @param mode A string specifying the mode of overlap calculation. 'binary' (default) checks for any overlap in the day of year range. 'gam' uses Generalized Additive Models (GAM) to calculate overlap.
#' @return A logical value or a numeric overlap area depending on the mode. Returns TRUE/FALSE in binary mode and a numeric value in gam mode.
#' @import mgcv
#' @import dplyr
#' @export
#' @examples
#' species1_data <- data.frame(
#'   day_of_year = sample(100:200, 500, replace = TRUE),
#'   latitude = runif(500, min = 30, max = 60)
#' )
#' species2_data <- data.frame(
#'   day_of_year = sample(150:250, 500, replace = TRUE),
#'   latitude = runif(500, min = 30, max = 60)
#' )
#' # Check for overlap in binary mode
#' pheno_overlap(species1_data, species2_data)
#' # Check for overlap in gam mode
#' pheno_overlap(species1_data, species2_data, mode = "gam")
#' # Check for overlap with specified latitude range in binary mode
#' pheno_overlap(species1_data, species2_data, lat_range = c(40, 50))
#' # Check for overlap with specified latitude range in gam mode
#' pheno_overlap(species1_data, species2_data, lat_range = c(40, 50), mode = "gam")

pheno_overlap <- function(species1_data, species2_data, lat_range = NULL, mode = "binary") {
  # Function to create smooth curves using GAM
  create_gam_curve <- function(data) {
    gam_model <- gam(latitude ~ s(day_of_year), data = data)
    return(gam_model)
  }

  # Function to predict latitude given a day of year using the GAM model
  predict_latitude <- function(gam_model, day_of_year, data) {
    day_min <- min(data$day_of_year)
    day_max <- max(data$day_of_year)
    predicted_latitude <- ifelse(day_of_year < day_min | day_of_year > day_max, NA,
                                 predict(gam_model, newdata = data.frame(day_of_year = day_of_year)))
    return(predicted_latitude)
  }

  # Function to calculate overlap between two species using GAM
  calculate_overlap <- function(species1_data, species2_data) {
    species1_data <- species1_data$occurrences
    species2_data <- species2_data$occurrences
    day_range_1 <- min(species1_data$day_of_year):max(species1_data$day_of_year)
    day_range_2 <- min(species2_data$day_of_year):max(species2_data$day_of_year)
    day_range <- min(c(day_range_1, day_range_2)):max(c(day_range_1, day_range_2))

    lat_range_1 <- min(species1_data$latitude):max(species1_data$latitude)
    lat_range_2 <- min(species2_data$latitude):max(species2_data$latitude)
    latitude_range <- if (length(lat_range_1) > length(lat_range_2)) lat_range_1 else lat_range_2

    # Create GAM curves for each species
    gam1 <- create_gam_curve(species1_data)
    gam2 <- create_gam_curve(species2_data)

    # Create a grid of day_of_year and latitude values
    grid <- expand.grid(day_of_year = day_range, latitude = latitude_range)

    # Predict latitude for each day of year for both species
    grid$lat_pred1 <- predict_latitude(gam1, grid$day_of_year, species1_data)
    grid$lat_pred2 <- predict_latitude(gam2, grid$day_of_year, species2_data)

    # Determine the overlap
    grid$overlap <- abs(grid$lat_pred1 - grid$latitude) < 2 & abs(grid$lat_pred2 - grid$latitude) < 2

    # Calculate the overlap area
    overlap_area <- sum(grid$overlap, na.rm = TRUE) / (length(day_range) * length(latitude_range))

    return(overlap_area)
  }

  # Filter data based on latitude range if provided
  if (!is.null(lat_range)) {
    species1_data <- species1_data %>%
      filter(latitude >= lat_range[1] & latitude <= lat_range[2])
    species2_data <- species2_data %>%
      filter(latitude >= lat_range[1] & latitude <= lat_range[2])
  }

  if (mode == "binary") {
    # Get the range of days of year for each species
    range1 <- min.phenology(species1_data):max.phenology(species1_data)
    range2 <- min.phenology(species2_data):max.phenology(species2_data)

    # Check for overlap
    overlap <- (length(intersect(range1, range2)) > 0)
  } else if (mode == "gam") {
    # Calculate overlap using GAM
    overlap <- calculate_overlap(species1_data, species2_data)
  } else {
    stop("Invalid mode. Use 'binary' or 'gam'.")
  }

  return(overlap)
}
