% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_history_audio_features.R
\name{get_history_audio_features}
\alias{get_history_audio_features}
\title{get_history_audio_features}
\usage{
get_history_audio_features(my_data, user)
}
\arguments{
\item{my_data}{a last.fm scrobble dataframe with at least track artist and title columns , as created by get_history.}

\item{user}{should be last.fm username or user's real name, used to name track audio features .csv and dataframe}
}
\value{
A lastfm scrobble, or music listening history dataframe with corresponding spotify track audiofeatures
}
\description{
This function pulls Spotify audio features for a user's entire last.fm listening history, with detailed track audio analysis and information.
}
