get_history <- function(user) {
  my_data <- scrobbler::download_scrobbles(username = user, api_key = "50d7685d484772f2ff42c45891b31c7b")
  return(my_data)
}