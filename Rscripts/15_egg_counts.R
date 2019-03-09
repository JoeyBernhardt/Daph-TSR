

library(tidyverse)
library(here)
library(cowplot)


daph_files <- list.files(path= "data-raw/daphnia-clutches-imagej/done", pattern="*.csv", recursive = TRUE, full.names = TRUE)

names(daph_files) <- daph_files %>% 
	gsub(pattern = ".csv$", replacement = "")


all_daph <- map_df(daph_files, read_csv, .id = "file_name") %>% 
	separate(col = file_name, into = c("path", "photo_id"), sep = "imagej/") %>% 
	mutate(photo_id = str_replace(photo_id, "-size", ""))


lengths <- all_daph %>% 
	filter(!is.na(Length)) %>% 
	select(photo_id, Length)

clutch_numbers <- all_daph %>% 
	filter(is.na(Length)) %>% 
	group_by(photo_id) %>% 
	tally()
	

all_clutches <- left_join(clutch_numbers, lengths) %>% 
	separate(photo_id, into= c("path2", "temperature", "replicate", "clutch", sep = "-")) %>% 
	rename(date = temperature, 
				 temperature = replicate,
				 replicate = clutch, 
				 clutch = `-`) 


all_clutches %>% 
	ggplot(aes(x = Length, y = n)) + geom_point() + geom_smooth(method = "lm") +
	ylab("Number of eggs") + xlab("Length")
