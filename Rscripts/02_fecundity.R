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

