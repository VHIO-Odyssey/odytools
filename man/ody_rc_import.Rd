% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap.R
\name{ody_rc_import}
\alias{ody_rc_import}
\title{Import a RedCap Project}
\usage{
ody_rc_import(
  token = NULL,
  form = NULL,
  url = "https://redcap.vhio.net/redcap/api/",
  label = TRUE,
  nest = TRUE
)
}
\arguments{
\item{token}{Project token. If not provided, a dialog promp will ask for it}

\item{form}{Form name to import. If NULL, all the forms will be imported.}

\item{url}{URL of the RedCap server (VHIO server by default).}

\item{label}{Logical. Should the variables be labelled according to the metadata?}

\item{nest}{Logical. Should the data be nested?}
}
\value{
A Tibble wiht metadata attributes (nested if nest = TRUE)
}
\description{
Import a RedCap Project
}
