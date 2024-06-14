#' Clean Phenology Data by Removing Outliers
#'
#' This function takes a phenology object, removes the top and bottom 5% outliers
#' based on the day of the year, removes outliers based on residuals
#' from a Generalized Additive Model (GAM) fitted to the latitude vs. day of the year,
#' and removes occurrences with day_of_year outliers within species groups.
#'
#' @param phenology A phenology object containing occurrence data with day_of_year and latitude.
#' @return A cleaned phenology object with outliers removed.
#' @import dplyr
#' @import mgcv
#' @export
#' @examples
#' \dontrun{
#' # Assuming 'pheno' is a valid phenology object
#' cleaned_pheno <- clean_pheno(pheno)
#' }
pheno_clean <- function(phenology) {
  # Ensure input is a phenology object
  if(class(phenology) != "phenology") {
    stop("Input must be a phenology object.")
  }

  data <- phenology$occurrences

  if(nrow(data) >= 75) {
    # Calculate the 5th and 95th percentiles
    P025 <- quantile(data$day_of_year, 0.025)
    P975 <- quantile(data$day_of_year, 0.975)

    # Remove the top and bottom 5% outliers
    data_clean <- data %>%
      filter(day_of_year >= P025 & day_of_year <= P975)
  } else {
    data_clean <- data
  }

  # Fit a GAM model
  model <- gam(day_of_year ~ s(latitude), data = data_clean)

  # Calculate residuals from the model
  data_clean$residuals <- residuals(model)

  # Define a threshold for outliers (e.g., residuals greater than 2 standard deviations)
  threshold <- 1.5 * sd(data_clean$residuals)

  # Remove outliers based on residuals
  data_final <- data_clean %>%
    filter(abs(residuals) <= threshold)

  # Remove occurrences with day_of_year outliers within species groups
  data_final <- data_final %>%
    filter(if (n() > 20) {
      second_min <- sort(unique(day_of_year))[2]
      second_max <- sort(unique(day_of_year), decreasing = TRUE)[2]
      day_of_year >= (second_min - 7) & day_of_year <= (second_max + 7)
    } else {
      TRUE
    }) %>%
    ungroup()

  data_final$scientific_name <- phenology$name

  # Create the cleaned phenology object
  output <- phenology(data_final)

  return(output)
}
