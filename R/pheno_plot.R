#' Plot phenology data
#'
#' @param phenology_data A data frame containing phenology data with columns 'day_of_year' and 'latitude'.
#' @param plot_type A string specifying the type of plot: 'frequency' (default) or 'latitude_vs_day'.
#' @param bins An integer specifying the number of bins for the 'frequency' plot (default is 30).
#' @return A ggplot object.
#' @import ggplot2
#' @export
#' @examples
#' pheno_plot(phenology_data)
#' pheno_plot(phenology_data, plot_type = 'latitude_vs_day')
pheno_plot <- function(phenology_data, plot_type = 'frequency', bins = 30) {
  if(!'day_of_year' %in% colnames(phenology_data$occurrences) | !'latitude' %in% colnames(phenology_data$occurrences)) {
    stop("The phenology_data must contain 'day_of_year' and 'latitude' columns.")
  }

  if(plot_type == 'frequency') {
    p <- ggplot(phenology_data$occurrences, aes(x = day_of_year)) +
      geom_histogram(bins = bins, fill = 'blue', color = 'black', alpha = 0.7) +
      labs(title = 'Frequency of Occurrences by Day of Year',
           x = 'Day of Year',
           y = 'Frequency') +
      theme_minimal()
  } else if(plot_type == 'latitude_vs_day') {
    p <- ggplot(phenology_data$occurrences, aes(x = day_of_year, y = latitude)) +
      geom_point(color = 'blue', alpha = 0.7) +
      geom_smooth(method = 'loess', color = 'red', se = FALSE) +
      labs(title = 'Latitude vs. Day of Year for Occurrences',
           x = 'Day of Year',
           y = 'Latitude') +
      theme_minimal()
  } else {
    stop("Invalid plot_type. Use 'frequency' or 'latitude_vs_day'.")
  }

  return(p)
}
