#############################################
#############################################
# Take table containing antibiotic 
# administration dates and determine
# which dates are consecutive.

# Use resulting data frame to calculate
# consecutive days of antibiotic
# therapy for each subject (record_id).
#############################################
#############################################

df <- as.data.frame(abx_data)
df_new <- df[FALSE, ]
df$taken_day <- as.Date(df$taken_day)
record_ids <- df %>% distinct(record_id) %>% pull

for (rec_id in record_ids) {
  df_subset <- df %>% filter(record_id == rec_id)
  first_day <- min(df_subset$taken_day)
  taken_days <- df_subset %>% distinct(taken_day) %>% pull
  consecutive_days <- first_day
  curr_day <- first_day
  while (TRUE) {
    curr_day <- curr_day + days(1)
    if (curr_day %in% taken_days) {
      consecutive_days <- c(consecutive_days, curr_day)
    } else {
      break
    }
  }
  df_new <- rbind(df_new, df_subset %>% filter(taken_day %in% consecutive_days))
}
