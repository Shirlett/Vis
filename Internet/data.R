read_csv("All_Data.csv") %>%
  mutate(id=str_replace(paste0(longitude, latitude), "-", "")) %>%
  mutate(Years=(round(Year,0))) %>%
  write_rds("internet_use.rds")
