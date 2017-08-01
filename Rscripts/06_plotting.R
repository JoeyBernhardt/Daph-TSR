library(googlesheets)
library(dplyr)
library(tidyverse)

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