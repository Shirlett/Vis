read_csv("All_Data.csv") %>%
  mutate(id=str_replace(paste0(longitude, latitude), "-", "")) %>%
  mutate(Years=(round(Year,0))) %>%
  filter(Year %in% c(2008:2014)) %>%
  mutate(National_Income = (GNI_per_cap/1000)) %>%
  mutate(Schooling = Primary_Compl_Rate) %>%
  mutate(Internet_Users = Internet_Users_per_100) %>%
  mutate(Unemployment = Per_Adult_Unemployment) %>%
  mutate(with_Electricity = Per_Access_Electricity) %>%
  mutate(Life_Exp = Median_Life_Exp) %>%
  filter(National_Income < 100) %>%
  write_rds("internet_use.rds")


