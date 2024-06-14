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
  if (class(phenology) != "phenology") {
    stop("Input must be a phenology object.")
  }

  data <- phenology$occurrences

  # Handle cases where the data frame is empty or has missing values
  if (nrow(data) == 0 || all(is.na(data$day_of_year)) || all(is.na(data$latitude))) {
    stop("Phenology data contains no valid occurrences or essential columns are NA.")
  }

  # Remove rows with NA values in essential columns
  data <- data %>% filter(!is.na(day_of_year) & !is.na(latitude))

  if (nrow(data) >= 75) {
    # Calculate the 5th and 95th percentiles
    P025 <- quantile(data$day_of_year, 0.025, na.rm = TRUE)
    P975 <- quantile(data$day_of_year, 0.975, na.rm = TRUE)

    # Remove the top and bottom 5% outliers
    data_clean <- data %>%
      filter(day_of_year >= P025 & day_of_year <= P975)
  } else {
    data_clean <- data
  }

  # Fit a GAM model, only if there are enough data points
  if (nrow(data_clean) > 10) {
    model <- gam(day_of_year ~ s(latitude), data = data_clean)

    # Calculate residuals from the model
    data_clean$residuals <- residuals(model)

    # Define a threshold for outliers (e.g., residuals greater than 2 standard deviations)
    threshold <- 1.5 * sd(data_clean$residuals, na.rm = TRUE)

    # Remove outliers based on residuals
    data_final <- data_clean %>%
      filter(abs(residuals) <= threshold)
  } else {
    data_final <- data_clean
  }

  # Remove occurrences with day_of_year outliers within species groups
  data_final <- data_final %>%
    filter(if (n() > 20) {
      sorted_days <- sort(unique(day_of_year))
      if (length(sorted_days) > 2) {
        second_min <- sorted_days[2]
        second_max <- sorted_days[length(sorted_days) - 1]
        day_of_year >= (second_min - 7) & day_of_year <= (second_max + 7)
      } else {
        TRUE
      }
    } else {
      TRUE
    }) %>%
    ungroup()

  data_final$scientific_name <- phenology$name

  # Create the cleaned phenology object
  output <- phenology(data_final)

  return(output)
}
