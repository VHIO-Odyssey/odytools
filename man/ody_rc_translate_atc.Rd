% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap.R
\name{ody_rc_translate_atc}
\alias{ody_rc_translate_atc}
\title{Translate ATC Codes}
\usage{
ody_rc_translate_atc(rc_df)
}
\arguments{
\item{rc_df}{A dataframe obtained from a RedCap import using the function \code{ody_rc_select}.}
}
\value{
A dataframe with ATC codes substituted by their corresponding labels.
}
\description{
Replace ATC codes in the dataframe with their corresponding descriptions.
}
