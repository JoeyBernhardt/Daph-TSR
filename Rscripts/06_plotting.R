library(googlesheets)
library(dplyr)
library(tidyverse)
library(broom)
library(lubridate)

# Reading data ------------------------------------------------------------

tsrsize <- read_csv("data-raw/daph_tsr_body_size.csv")

install.packages("janitor")
library(janitor)

tsrsize2 <- clean_names(tsrsize)

tsrsize2 %>% 
	filter(stage=="clutch3") %>% 
	filter(actual_size_um > 0) %>% 
	ggplot(aes(x=temperature, y=actual_size_um)) + geom_point() +
	geom_smooth(method="lm") + theme_bw() + ylab("Body Size (um)") + xlab("Temperature (°C)")

model_results <- tsrsize2 %>% 
	filter(stage=="clutch3") %>% 
	filter(actual_size_um > 0) %>% 
	lm(actual_size_um ~ temperature, data=.) %>% 
	tidy

write_csv(model_results, "data-processed/model_results.csv")


# Acc Daph Body Size ------------------------------------------------------

accdaph <- read_csv("data-raw/acc_daph_body_size.csv")
accdaph2 <- clean_names(accdaph)

accdaph3 <- accdaph2 %>%
	mutate(date_measured=mdy(date_measured)) %>% 
	filter(!is.na(size_um))

accdaph3 %>% 
	filter(temperature==27) %>%
	ggplot(aes(x=date_measured, y=size_um, color=factor(replicate), group=replicate)) + 
	# geom_point()
	facet_wrap(~temperature) + geom_line(size=2) + theme_bw()


