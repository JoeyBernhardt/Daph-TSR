

library(tidyverse)
library(janitor)
library(broom)


kimmy_data_raw <- read_csv("data-raw/daph-kimmy.csv")

kd <- clean_names(kimmy_data_raw)

kd %>% 
	filter(actual_size_um != 0) %>% 
	filter(stage != "neonate") %>% 
	# filter(stage == "clutch1") %>% 
	group_by(temperature_c) %>% 
	summarise(mean_length = mean(actual_size_um)) %>% 
ggplot(aes(x = temperature_c, y = mean_length)) + geom_point()


kd %>% 
	filter(actual_size_um != 0) %>% 
	filter(stage != "neonate") %>% 
	filter(stage == "clutch3") %>% 
	ggplot(aes(x = temperature_c, y = actual_size_um)) + geom_point() + geom_smooth(method = "lm")


kd %>% 
	filter(actual_size_um != 0) %>% 
	filter(stage != "neonate") %>%
	filter(stage == "clutch3") %>% 
	do(tidy(lm(actual_size_um ~ temperature_c, data = .), conf.int = TRUE)) %>% View
	
