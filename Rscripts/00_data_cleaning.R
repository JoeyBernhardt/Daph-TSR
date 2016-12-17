#### Loading and cleaning the daph-tsr data
#### Last updated Dec 05 2016 by JB

# libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)


# load data ---------------------------------------------------------------

data_raw <- read_csv("data-raw/DAPH-TSR-2.csv")

data <- data_raw %>% 
	clean_names() 


data$date_of_1st_clutch[data$date_of_1st_clutch == "July 16"] <- "July 16 2016"
data$date_of_4th_clutch[data$date_of_4th_clutch == "August 11"] <- "August 11 2016"
data$date_of_4th_clutch[data$date_of_4th_clutch == "Dead"] <- NA

str(data)

names(data)
data2 <- data %>% 
	mutate(date_of_birth_m_d_y = str_replace(date_of_birth_m_d_y, ",", "")) %>% 
	mutate(date_of_birth_m_d_y = mdy(date_of_birth_m_d_y)) %>% 
	mutate(date_of_1st_clutch = mdy(date_of_1st_clutch)) %>% 
	mutate(date_of_2nd_clutch = mdy(date_of_2nd_clutch)) %>% 
	mutate(date_of_3rd_clutch = mdy(date_of_3rd_clutch)) %>% 
	mutate(date_of_4th_clutch = mdy(date_of_4th_clutch)) %>% 
	mutate(temperature = str_replace(temperature, "C", "")) %>% 
	mutate(temperature = as.numeric(temperature))

data3 <- data2 %>% 
	mutate(time_to_first_clutch = interval(date_of_birth_m_d_y, date_of_1st_clutch)/dhours(1)) %>% 
	mutate(time_btw_1_2 = interval(date_of_1st_clutch, date_of_2nd_clutch)/dhours(1)) %>% 
	mutate(time_bwn_2_3 = interval(date_of_2nd_clutch, date_of_3rd_clutch)/dhours(1)) %>% 
	mutate(time_bwn_3_4 = interval(date_of_3rd_clutch, date_of_4th_clutch)/dhours(1)) 
				 	
names(data3)

data3 <- data3 %>% 
	unite(unique_id, id, temperature, remove = FALSE)

write_csv(data3, "data-processed/data3.csv")

# initial plots! ----------------------------------------------------------

data3 %>% 
	filter(unique_id != "K_16") %>% 
ggplot(data = ., aes(x = temperature, y = time_to_first_clutch, label = id)) + geom_point(size = 4) 
ggplot(data = data3, aes(x = temperature, y = length_at_1st_clutch)) + geom_point(size = 4)

