library(dplyr)
library(readr)
library(stringr)

df <- read_csv("data/raw/your_file.csv")

df <- df %>%
  mutate(
    Installs = str_remove_all(Installs, ","),
    Installs = str_remove_all(Installs, "\\+"),
    Installs = as.numeric(Installs)
  )

df <- df %>%
  mutate(
    Successful_App = ifelse(Rating >= 4.5 & Installs >= 1000000, 1, 0)
  )

write_csv(df, "data/processed/cleaned_data.csv")
