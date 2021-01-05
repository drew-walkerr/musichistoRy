#' get_session_summaries
#'
#' This function performs further variable computation to aid in analysis, including date manipulation, track start/end times, differentials to help determine skipping, as well as session length, determined by minute long difference in music listening end and start times. 
#'
#' @param audio_features is a last.fm scrobble/spotify audio features dataframe, as created by get_history_audio_features.
#' @return a spotify/last.fm dataframe with computed values on date, sessions, and listening times 
#' @export
get_session_df  <- function(audio_features){
audio_features$date <- lubridate::as_datetime(audio_features$date)
# ADDING SESSION VARIABLES -------------------------------------
#Convert duration_ms to millisecond period value 
milli <- lubridate::dmilliseconds(audio_features$duration_ms)
#adding variables to start to create the intervals
raw_music_pull_dataframe <- audio_features %>% 
  arrange(date) %>% 
  dplyr::mutate(year = lubridate::year(date), 
         month = lubridate::month(date), 
         day = lubridate::day(date), 
         hour = lubridate::hour(date),
         last_play = dplyr::lead(date), 
         recentstart = lubridate::as_datetime(date),
         end = (lubridate::as_datetime(date) + milli),
         last_theo_end = dplyr::lead(end)) %>%
  dplyr::add_count(id) %>% 
  dplyr::rename(song_play_count = n) %>%
  dplyr::add_count(artist) %>% 
  dplyr::rename(artist_play_count = n)
#make time.interval object between when the previous song could have theoretically ended and the start of the track played most recently to indicate if there was time after it finished playing or was skipped
time.interval <- lubridate::interval(raw_music_pull_dataframe$recentstart,raw_music_pull_dataframe$last_theo_end)
#Create variable that is the difference between start time and theoretical last time played in duration
#This part isn't working for some reason-- does not accurately separate these into sessions
raw_music_pull_dataframe <- raw_music_pull_dataframe %>% 
  dplyr::mutate(diff = lubridate::as.duration(time.interval))
#REORDER oldest to newest 
raw_music_pull_dataframe <- dplyr::arrange(raw_music_pull_dataframe, date) 
#ASSIGN SESSION LABELS 
as.numeric(raw_music_pull_dataframe$diff)
session_dataframe <- raw_music_pull_dataframe %>% 
  dplyr::mutate(new_interval = ifelse(abs(as.numeric(diff)) > 60, 1, 0),
                session_number = 1 + cumsum(new_interval))
#Summarizing session variable summaries, session lengths, session midpoints 
by_session_number <- group_by(session_dataframe, session_number)
session_summary <- dplyr::summarise(by_session_number,
                                    session_song_count = dplyr::n(),
                                    session_valence_mean = mean(valence, na.rm = TRUE),
                                    session_energy_mean = mean(energy, na.rm = TRUE),
                                    session_key_mean = mean(music_key, na.rm = TRUE),
                                    session_loudness_mean = mean(loudness, na.rm = TRUE),
                                    sessionmonth = mean(month, na.rm = TRUE),
                                    lastsongdatetime = max(end, na.rm = TRUE),
                                    firstsongdatetime = min(recentstart, na.rm = TRUE))
#Making session length duration objects
sessionlength <- lubridate::interval(session_summary$lastsongdatetime,session_summary$firstsongdatetime)
midpointinterval <- sessionlength/2
#Adding session duration, midpointdatetime variables
session_summary <- session_summary %>% 
  dplyr::mutate(midpointdatetime = firstsongdatetime + lubridate::as.duration(midpointinterval),
         duration = lubridate::as.duration(sessionlength))
#Merging these summaries with session
merged_session_summary<- left_join(session_dataframe,session_summary, by = "session_number")
return(merged_session_summary)
}