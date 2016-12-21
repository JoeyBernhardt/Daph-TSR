#### Daph TSR lengths



# load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)
library(lubridate)
library(plotrix)

# read in data ------------------------------------------------------------

lengths_raw <- read_csv("data-raw/daph-tsr-lengths.csv")

## do I need to add the last lengths to this file??
## lengths_raw only has data up until August 16, need to add the subsequent data!

lengths_data7 <- read_csv("data-processed/data7.csv")
lengths_data3 <- read_csv("data-processed/data3.csv")



lengths <- lengths_raw %>% 
	mutate(date = mdy(date)) %>% 
	mutate(length = as.numeric(length))

lengths %>% 
	# filter(version == 2) %>%
	filter(temperature > 10) %>% 
	filter(length > 1000) %>% 
	group_by(temperature) %>% 
	summarise_each(funs(mean, std.error), length) %>%
	ggplot(aes(x = temperature, y = mean)) + geom_point(size = 4) +
	# geom_smooth() + 
	geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.1)


lengths_adults <- lengths %>% 
	filter(length > 1000)

## now need to bring in the data from after august 16

str(lengths_data3)

lengths_long4 <- read_csv("data-processed/data_long4.csv")
str(lengths_long4)
str(lengths_raw)

lengths_long4 %>% 
	filter(date < as.Date("2016-08-16"))

### ok merge the lengths_long4 with lengths
### first rename some of the columns so they match up

lengths_2 <- lengths %>%
	rename(life_stage = `clutch no`) %>% 
	mutate(life_stage = as.character(life_stage))
	
lengths_all <- bind_rows(lengths_long4, lengths_2)

write_csv(lengths_all, "data-processed/lengths_all.csv")

lengths_all <- read_csv("data-processed/lengths_all.csv")


lengths_all_adult <- lengths_all %>% 
	filter(length > 1000) %>% 
	distinct(length, .keep_all = TRUE)
write_csv(lengths_all_adult, "data-processed/lengths_all_adult.csv")

lengths_all_adult <- read_csv("data-processed/lengths_all_adult.csv")

lengths_all_adult %>% 
filter(temperature > 10) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	filter(length > 1000) %>% 
	group_by(inverse_temp) %>% 
	summarise_each(funs(mean, min, max, std.error), length) %>%
	ggplot(aes(x = inverse_temp, y = min)) + geom_point(size = 4) +
	# geom_smooth() + 
	geom_errorbar(aes(ymin = min - std.error, ymax = min + std.error), width = 0.1) + 
	scale_x_reverse() + xlab("temperature (1/kT)") + ylab("average adult body length (um)") +
	theme_minimal() + 
	theme(axis.text.y   = element_text(size=20),
				axis.text.x   = element_text(size=20),
				axis.title.y  = element_text(size=20),
				axis.title.x  = element_text(size=20),
				panel.background = element_blank(),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				axis.line = element_line(colour = "black"),
				axis.ticks = element_line(size = 1),
				legend.title = element_blank()) +
	theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=1, lineend="square"))


### OK next step is clean up the lengths_all_adult so we can get growth rates over time

### let's take lengths_all_adult, and see if we can extract the letter associated with each individual
#nope this doesn't work
# lengths_all_adult %>% 
# 	separate(ID, into = c("letter", "other"), sep = "[A-Z]", remove = FALSE) %>% View

lengths_ann_raw <- read_csv("data-processed/lengths_all_adult_annotated.csv")
lengths_all <- read_csv("data-processed/lengths_all.csv")


lengths_ann <- lengths_ann_raw %>% 
	mutate(date = mdy(date))

lengths_birth <- lengths_all %>%
	filter(life_stage == "birth") %>% 
	mutate(unique_id = str_replace(unique_id, "2N1.2", "N")) %>% 
	mutate(unique_id = str_replace(unique_id, "2N1.3", "N")) %>%
	mutate(unique_id = str_replace(unique_id, "2N1.4", "N")) %>% 
	mutate(unique_id = str_replace(unique_id, "2N1.8", "N")) %>%
	mutate(unique_id = str_replace(unique_id, "2N1.5", "N")) %>% 
	mutate(unique_id = str_replace(unique_id, "2N1.1", "N")) %>%
	separate(unique_id, into = c("letter", "temp"), remove = FALSE) 


str(lengths_ann)
	
lengths_b2m <- bind_rows(lengths_ann, lengths_birth)

str(lengths_b2m)

lengths_b2m %>% 
	mutate(certainty = ifelse(life_stage == "birth", "yes", certainty)) %>% View

### yes this is it!!
lengths_b2m %>% 
	mutate(certainty = ifelse(life_stage == "birth", "yes", certainty)) %>%
	# filter(version == 2) %>%
	filter(certainty == "yes") %>% 
	# filter(life_stage == "birth") %>% 
	# filter(temperature == 16) %>% 
	ggplot(aes(date, y = length, color = letter, group = letter)) + geom_point(size = 3) +
	geom_line() + facet_wrap( ~ temperature)
