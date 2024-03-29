% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap_project.R
\name{ody_rc_timetravel}
\alias{ody_rc_timetravel}
\title{Paradox-Free Time Travelling}
\usage{
ody_rc_timetravel(timepoint)
}
\arguments{
\item{timepoint}{Timepoint pattern.}
}
\description{
Replace the current redcap_data and datasets with the ones from a previous backup stored in the data/imports folder.
}
\details{
The back-ups are named after the project and the import date. The timepoint pattern is a regular expression to match the name of the back-up file. The pattern must match one and only one back-up file.
}
