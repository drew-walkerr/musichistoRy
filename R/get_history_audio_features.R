get_history_audio_features <- function(my_data,user){
  Sys.setenv(SPOTIFY_CLIENT_ID = '2c46a5d6764f425ab746a56a1c8791b9')
  Sys.setenv(SPOTIFY_CLIENT_SECRET = '9b809cd5be004e8fbbc72ad74b0e19a7')
  access_token <- spotifyr::get_spotify_access_token(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                                     client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"))
  track_audio_features <- function(artist, title, type = "track") {
  search_results <- spotifyr::search_spotify(paste(artist, title), type = type)
  track_audio_feats <- spotifyr::get_track_audio_features(search_results$id[[1]])
  return(track_audio_feats)
}
  # create a version of this function which can handle errors
  possible_af <- purrr::possibly(track_audio_features, otherwise = tidyr::tibble())
  colnames(my_data)
  colnames(my_data)[colnames(my_data)=="song_title"] = "title"
  tictoc::tic()
  future::plan(future::multiprocess)
  possible_feats <- purrr::possibly(track_audio_features, otherwise = tidyr::tibble("NA"))
  totalaudio_features <- my_data %>%
    dplyr::mutate(audio_features = furrr::future_map2(artist, title, possible_feats)) %>%
    tidyr::unnest(cols = c(audiofeatures)) %>%
    tidyr::as_tibble()
  tictoc::toc()
  totalaudio_features$date <- lubridate::dmy_hm(totalaudio_features$date)
  totalaudio_features$date <- lubridate::with_tz(totalaudio_features$date, tzone = "US/Eastern")
  csvFileName <- paste(user,format(Sys.time(),"%d-%b-%Y %H.%M"),".csv")
  write.csv(totalaudio_features, file = csvFileName)
  save.image(file = paste(user,"raw_music_pull_data.RData"))
  return(totalaudio_features)
}
