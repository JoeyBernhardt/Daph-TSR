

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


acc %>% 
	mutate(mass =  0.00402*((size_um/1000)^2.66)) %>% 
	filter(stage %in% c("clutch1","clutch2", "clutch3", "clutch4", "clutch5", "clutch6")) %>% 
	ggplot(aes(x = temperature, y = mass)) + geom_point() + 
	geom_smooth(method = "lm") +
	facet_wrap( ~ stage)

### now get the clutch sizes for the acclimated daphnia

acc_clutch <- read_csv("data-raw/acclimated-daphnia-clutches.csv") 

acc_clutch %>% 
	filter(clutch_number < 7) %>% 
	ggplot(aes(x = temperature, y = final_baby_count)) + geom_point() +
	facet_wrap( ~ clutch_number, scales = "free") + geom_smooth()
