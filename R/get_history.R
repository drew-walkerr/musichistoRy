#' get_history
#'
#' This function pulls a last.fm user's music listening history
#'
#' @param user Last.fm username
#' @return A lastfm scrobble, or music listening history dataframe 
#' @export
get_history <- function(user) {
  my_data <- scrobbler::download_scrobbles(username = user, api_key = "50d7685d484772f2ff42c45891b31c7b")
  return(my_data)
}
