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
	
str(data)

data2 <- data %>% 
	mutate(date_of_birth_m_d_y = str_replace(date_of_birth_m_d_y, ",", "")) %>% 
	mutate(date_of_birth_m_d_y = mdy(date_of_birth_m_d_y)) %>% 
	mutate(date_of_1st_clutch = mdy(date_of_1st_clutch)) %>% 
	mutate(temperature = str_replace(temperature, "C", "")) %>% 
	mutate(temperature = as.numeric(temperature))

data3 <- data2 %>% 
	mutate(time_to_first_clutch = interval(date_of_birth_m_d_y, date_of_1st_clutch)/dhours(1)) 
				 	
names(data3)


# initial plots! ----------------------------------------------------------

ggplot(data = data3, aes(x = temperature, y = time_to_first_clutch)) + geom_point()
ggplot(data = data3, aes(x = temperature, y = length_at_3rd_clutch)) + geom_point()

