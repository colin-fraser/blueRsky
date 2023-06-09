% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bluesky.R
\name{bsky_search_actors}
\alias{bsky_search_actors}
\title{Find actors matching search criteria.}
\usage{
bsky_search_actors(
  term = NULL,
  limit = NULL,
  cursor = NULL,
  ...,
  credentials = default_credentials(wrapper),
  action = default_action,
  decode_if_success = decode_if_success_default_value
)
}
\arguments{
\item{term}{Search term}

\item{limit}{number between 1 and 100}

\item{cursor}{cursor}

\item{...}{Additional arguments to requestor}

\item{credentials}{Credential.}

\item{action}{Either 'perform' or 'dry-run'}

\item{decode_if_success}{If FALSE, returns the whole response object. If true, returns the decoded response.}
}
\value{
List of actors
}
\description{
Find actors matching search criteria.
}
