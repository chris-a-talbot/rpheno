% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pheno_plot.R
\name{pheno_plot}
\alias{pheno_plot}
\title{Plot phenology data}
\usage{
pheno_plot(phenology_data, plot_type = "frequency", bins = 30)
}
\arguments{
\item{phenology_data}{A data frame containing phenology data with columns 'day_of_year' and 'latitude'.}

\item{plot_type}{A string specifying the type of plot: 'frequency' (default) or 'latitude_vs_day'.}

\item{bins}{An integer specifying the number of bins for the 'frequency' plot (default is 30).}
}
\value{
A ggplot object.
}
\description{
Plot phenology data
}
\examples{
pheno_plot(phenology_data)
pheno_plot(phenology_data, plot_type = 'latitude_vs_day')
}
