% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rollingCorrMatrix.R
\name{rollingCorMatrix}
\alias{rollingCorMatrix}
\title{Calculates the roolingCorrelation matrix and returns the uniqe correlations ranked}
\usage{
rollingCorMatrix(df, removeConstantColumns = T, tolerance = 0.6,
  window = 100)
}
\arguments{
\item{df}{data frame}

\item{removeConstantColumns}{removes columns in df which are constant}

\item{tolerance}{the lowest accepted correlation}

\item{window}{the window of which the correlation should be calculated}
}
\value{
a data frame
}
\description{
alculates the roolingCorrelation matrix and returns the uniqe correlations ranked
}
\author{
Johan Gudmundsson, \email{jgu@blackwoodseven.com}
}

