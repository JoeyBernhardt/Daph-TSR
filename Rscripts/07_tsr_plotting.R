

library(googlesheets)
library(dplyr)
library(readr)
library(janitor)
library(ggplot2)
library(broom)
library(lubridate)
library(viridis)
library(plotrix)
library(tidyr)
library(car)
library(FSA)


# read in data ------------------------------------------------------------

size <- read_csv("data-raw/daph_tsr_body_size.csv")

size2 <- clean_names(size) %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	filter(actual_size_um > 0) %>% 
	mutate(size_um = actual_size_um) %>% 
	filter(!is.na(size_um))


size2 %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage == "clutch3") %>% 
	ggplot(aes(x = temperature, y = actual_size_um)) + geom_point() +
	geom_smooth(method = "lm") + theme_bw() + ylab("Body length") + xlab("Temperature")


# now get it in the right format!! ----------------------------------------


size3 <- size2 %>% 
	select(stage, size_um, temperature, replicate) %>% 
	spread(key = stage, value = size_um) %>% 
	select(temperature, replicate, neonate, everything()) %>% 
	filter(!is.na(clutch1), !is.na(clutch2), !is.na(clutch3))


date_10 <- acc3 %>% 
	filter(temperature == 10) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, date_measured, stage) %>%
	spread(key = stage, value = date_measured) %>% 
	rename(neonate_date = neonate,
				 clutch1_date = clutch1,
				 clutch2_date = clutch2,
				 clutch3_date = clutch3,
				 clutch4_date = clutch4,
				 clutch5_date = clutch5)


size_10 <- acc3 %>% 
	filter(temperature == 10) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, size_um, stage) %>%
	spread(key = stage, value = size_um) %>% 
	rename(neonate_size = neonate,
				 clutch1_size = clutch1,
				 clutch2_size = clutch2,
				 clutch3_size = clutch3,
				 clutch4_size = clutch4,
				 clutch5_size = clutch5)


all_10 <- left_join(date_10, size_10, by = c("temperature", "replicate"))


wide10 <- all_10 %>% 
	mutate(clutch1_age = interval(neonate_date, clutch1_date)/ddays(1)) %>%
	mutate(clutch2_age = interval(neonate_date, clutch2_date)/ddays(1)) %>%  
	mutate(clutch3_age = interval(neonate_date, clutch3_date)/ddays(1)) %>%  
	mutate(clutch4_age = interval(neonate_date, clutch4_date)/ddays(1)) %>%
	mutate(clutch5_age = interval(neonate_date, clutch3_date)/ddays(1)) %>% 
	select(-contains("date")) %>% 
	mutate(neonate_age = 0)

age_10 <- wide10 %>% 
	select(temperature, replicate, contains("age")) %>%
	gather(key = clutch, value = age, 3:8) %>%
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)


length_10 <- wide10 %>% 
	select(temperature, replicate, contains("size")) %>% 
	gather(key = clutch, value = length, 3:8) %>%
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)

data_10 <- left_join(age_10, length_10, by = c("temperature", "replicate", "clutch"))




size2 %>% 
	ggplot(aes(x = date_measured, y = actual_size_um, group = replicate, color = factor(replicate))) + geom_point() +
	geom_line() +
	facet_wrap( ~ temperature, scales = "free")



model_results <- size2 %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage == "clutch3") %>%
	lm(actual_size_um ~ temperature, data = .) %>% 
	tidy

write_csv(model_results, "data-processed/model_results.csv")