#### Daph TSR lengths



# load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)
library(lubridate)
library(plotrix)

# read in data ------------------------------------------------------------

lengths_raw <- read_csv("data-raw/daph-tsr-lengths.csv")

lengths <- lengths_raw %>% 
	mutate(date = mdy(date))

lengths %>% 
	filter(version == 2) %>%
	filter(temperature > 10) %>% 
	mutate(length = as.numeric(length)) %>% 
	filter(length > 1000) %>%
	group_by(temperature) %>% 
	summarise_each(funs(mean, std.error), length) %>% 
	ggplot(aes(x = temperature, y = mean)) + geom_point() +
	geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.1)
