

### acclimated experiment

library(cowplot)
library(tidyverse)
library(lubridate)
library(janitor)
library(viridis)

acc <- read_csv("data-raw/acclimated-daphnia-body-size.csv") %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	clean_names()


acc %>% 
	ggplot(aes(x = date_measured, y = size_um, color = replicate, group = replicate)) + geom_point() +
	facet_wrap( ~ temperature) + scale_color_viridis() + geom_line() +
	ylab("Size (um)") + xlab("Date")
ggsave("figures/acclimated_daphnia_sizes.png", width = 7, height = 5)

acc %>% 
	group_by(temperature, replicate) %>% 
	summarise_each(funs(max), size_um) %>% 
	ggplot(aes(x = temperature, y = size_um)) + geom_point()

