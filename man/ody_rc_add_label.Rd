% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap.R
\name{ody_rc_add_label}
\alias{ody_rc_add_label}
\title{Adds labels to a data frame using REDCap metadata.}
\usage{
ody_rc_add_label(df, redcap_data = NULL)
}
\arguments{
\item{df}{A data frame to which the labels will be added.}

\item{redcap_data}{Optional. A REDCap data frame containing the metadata as
attribute. If not supplied, the function will use \code{redcap_data} from the
global environment.}
}
\value{
A data frame with variable labels applied from the REDCap metadata.
}
\description{
This function takes a data frame and applies variable labels derived from
the REDCAp metadata column \code{field_label}. If no \code{redcap_data} from
which to extract the metadata is provided, it will look for an object named
\code{redcap_data} in the global environment.
}
