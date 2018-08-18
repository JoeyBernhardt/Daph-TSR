library(tidyverse)
library(lubridate)


data3 <- read_csv("data-processed/data3.csv")


data3 %>% 
	mutate(clutch3_age = interval(date_of_birth_m_d_y, date_of_3rd_clutch)/ddays(1)) %>% 
	mutate(clutch1_data = ifelse(!is.na(size_of_first_clutch), 1, 0)) %>% 
	mutate(clutch2_data = ifelse(!is.na(size_of_2nd_clutch), 1, 0)) %>% 
	mutate(clutch3_data = ifelse(!is.na(size_of_3rd_clutch), 1, 0)) %>% 
	mutate(size_of_first_clutch = ifelse(is.na(size_of_first_clutch), 0, size_of_first_clutch)) %>% 
	mutate(size_of_2nd_clutch = ifelse(is.na(size_of_2nd_clutch), 0, size_of_2nd_clutch)) %>% 
	mutate(size_of_3rd_clutch = ifelse(is.na(size_of_3rd_clutch), 0, size_of_3rd_clutch)) %>% 
	mutate(total_clutch_data = rowSums(.[26:28])) %>% 
	mutate(average_clutch_size = (size_of_first_clutch + size_of_2nd_clutch + size_of_3rd_clutch)/total_clutch_data) %>% 
	mutate(total_clutch_size = average_clutch_size*3) %>% 
	mutate(babies_per_day_av = total_clutch_size/clutch3_age) %>% 
	# ggplot(aes(x = temperature, y = clutch3_age)) + geom_point()
	# filter(!is.na(size_of_first_clutch), !is.na(size_of_2nd_clutch), !is.na(size_of_3rd_clutch)) %>% 
	mutate(total_babies = size_of_first_clutch + size_of_2nd_clutch + size_of_3rd_clutch) %>% 
	mutate(babies_per_day = total_babies/clutch3_age) %>% 
	ggplot(aes(x = length_at_3rd_clutch, y = babies_per_day_av, color = factor(temperature))) + geom_point() +
	geom_smooth(method = "lm")
