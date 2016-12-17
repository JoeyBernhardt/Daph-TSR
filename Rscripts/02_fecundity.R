### Daph TSR fecundity


# load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)
library(broom)
library(lubridate)
library(plotrix)

# read in data ------------------------------------------------------------
data_raw <- read_csv("data-raw/DAPH-TSR-clutches.csv")

data3 <- read_csv("data-processed/data3.csv")
data7 <- read_csv("data-processed/data7.csv")

ggplot(data = data_raw, aes(x = temperature, y = individuals)) + geom_point()


data_raw %>% 
	filter(grepl("V2", ID)) %>%
	group_by(temperature, ID) %>% 
	summarise_each(funs(mean, std.error), individuals) %>% 
	ggplot(aes(x = temperature, y = mean)) + geom_point() +
	geom_smooth(method = "lm")


data_raw %>% 
	filter(grepl("V2", ID)) %>%
	group_by(temperature, ID) %>% 
	summarise_each(funs(mean, std.error), individuals) %>% 
	ungroup() %>% 
	do(tidy(lm(mean ~ temperature, data = .), conf.int = TRUE)) %>% View

## what's the relationship between size and fecundity?
## also plot the growth over time

## need to get body size and fecundity side by side
### are we missing some fecundity data from the 24s?

v2_babies <- data_raw %>% 
	filter(grepl("V2", ID)) %>% 
	separate(ID, into = c("V", "letter"), sep = 2) %>% 
	unite(unique_id, letter, temperature, sep = "_")


all <- left_join(data7, v2_babies, by = "unique_id")

ggplot(data = all, aes(x = max_length, y = individuals, color = factor(temperature))) + geom_point(size = 4)
