get_session_summaries <- function(audio_features) {
raw_music_pull_dataframe <- audio_features
lubridate::as_datetime(raw_music_pull_dataframe$date)
# ADDING SESSION VARIABLES -------------------------------------
#Convert duration_ms to millisecond period value 
milli <- lubridate::dmilliseconds(raw_music_pull_dataframe$duration_ms)
#adding variables to start to create the intervals
raw_music_pull_dataframe <- raw_music_pull_dataframe %>% 
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
time.interval <- raw_music_pull_dataframe$last_theo_end %--% raw_music_pull_dataframe$recentstart
#Create variable that is the difference between start time and theoretical last time played in duration
raw_music_pull_dataframe <- raw_music_pull_dataframe %>% 
  dplyr::mutate(diff = lubridate::as.duration(time.interval))
#REORDER
raw_music_pull_dataframe <- dplyr::arrange(raw_music_pull_dataframe, date) 
#ASSIGN SESSION LABELS 
raw_music_pull_dataframe <- raw_music_pull_dataframe %>% dplyr:: mutate(new_interval = diff > lubridate::as.duration(3600),
                                                                new_interval = ifelse(is.na(new_interval), FALSE, new_interval),
                                                                session_number = cumsum(new_interval))
#Summarizing session variable summaries, session lengths, session midpoints 
session_summary <- raw_music_pull_dataframe %>% 
  dplyr::group_by(session_number) %>% 
  dplyr::summarise(by_session_number,
            session_song_count = dplyr::n(),
            session_valence_mean = mean(valence, na.rm = TRUE),
            session_energy_mean = mean(energy, na.rm = TRUE),
            session_key_mean = mean(key, na.rm = TRUE),
            session_loudness_mean = mean(loudness, na.rm = TRUE),
            sessionmonth = mean(month, na.rm = TRUE),
            lastsongdatetime = max(end, na.rm = TRUE),
            firstsongdatetime = min(recentstart, na.rm = TRUE))
#Making session length duration objects
sessionlength <- session_summary$firstsongdatetime %--% session_summary$lastsongdatetime
midpointinterval <- sessionlength/2
#Adding session duration, midpointdatetime variables
session_summary <- session_summary %>% 
  dplyr::mutate(midpointdatetime = firstsongdatetime + lubridate::as.duration(midpointinterval),
         duration = lubridate::as.duration(sessionlength))
merged_session_summary<- left_join(raw_music_pull_dataframe,session_summary, by = "session_number")
return(merged_session_summary)
}