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


lengths_all_adult <- lengths_all %>% 
	filter(length > 1000) %>% 
	distinct(length, .keep_all = TRUE)

lengths_all_adult %>% 
filter(temperature > 10) %>% 
	filter(length > 1000) %>% 
	group_by(temperature) %>% 
	summarise_each(funs(mean, std.error), length) %>%
	ggplot(aes(x = temperature, y = mean)) + geom_point(size = 4) +
	# geom_smooth() + 
	geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.1)


### OK next step is clean up the lengths_all_adult so we can get growth rates over time

### let's take lengths_all_adult, and see if we can extract the letter associated with each individual
#nope this doesn't work
# lengths_all_adult %>% 
# 	separate(ID, into = c("letter", "other"), sep = "[A-Z]", remove = FALSE) %>% View


?separate
separate
