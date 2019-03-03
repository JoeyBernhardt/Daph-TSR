

library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
### generation times


# tsr1 <- read_csv("data-raw/DAPH-TSR-long.csv")
summer2016 <- read_csv("data-raw/DAPH-TSR-2.csv") %>% 
	clean_names()
# tsr3 <- read_csv("data-raw/daph-kimmy.csv")
winter2017 <- read_csv("data-processed/all_growth.csv")
all_generation_times <- read_csv("data-processed/all_generation_times.csv") %>% 
	mutate(replicate = as.character(replicate))
acclimated_gens <- read_csv("data-processed/acclimated-clutches-processed.csv") %>% 
	mutate(replicate = as.character(replicate))

tsr_generation_times <- winter2017 %>% 
	rename(generation_time = clutch1_age) %>% 
	select(temperature, replicate, generation_time) %>% 
	mutate(experiment = "tsr") %>% 
	mutate(replicate = as.character(replicate))


gens_summer2016 <- summer2016 %>% 
	mutate(date_of_birth_m_d_y = mdy(date_of_birth_m_d_y)) %>% 
	mutate(clutch1_date = mdy(date_of_1st_clutch)) %>%
	mutate(generation_time = interval(date_of_birth_m_d_y, clutch1_date)/ddays(1)) %>% 
	select(id, temperature, generation_time) %>% 
	mutate(temperature = str_replace(temperature, "C", "")) %>%
	mutate(temperature = as.numeric(temperature)) %>% 
	filter(generation_time < 40) %>% 
	rename(replicate = id) %>% 
	mutate(experiment = "summer2016")

gens_acclimated <- acclimated_gens %>% 
	filter(stage == "clutch1") %>%
	mutate(experiment = "acclimated") %>% 
	rename(generation_time = age) %>% 
	select(temperature, replicate, generation_time, experiment) %>% 
	mutate(replicate = as.character(replicate))
	

str(gens_summer2016)
str(gens_acclimated)
str(all_generation_times)
all_gens <- bind_rows(gens_summer2016, gens_acclimated, tsr_generation_times)

all_gens %>% 
	ggplot(aes(x = temperature, y = generation_time, color = experiment)) + geom_point() +
	geom_smooth() + facet_wrap( ~ experiment)


all_gens %>% 
	# filter(experiment != "summer2016") %>% 
	mutate(inv_temp = (1/(.00008617*(temperature + 273.15)))) %>% 
	group_by(experiment) %>% 
	do(tidy(lm(log(generation_time) ~ inv_temp, data = .), conf.int = TRUE)) %>% 
	ggplot(aes(x = experiment, y = estimate)) + geom_point() +
	geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
	facet_wrap( ~ term, scales = "free") +
	geom_hline(yintercept = 0.65)

ggsave("figures/generation_times_slopes.pdf", width = 8, height = 4)

all_gens %>% 
	filter(experiment != "summer2016") %>% 
	mutate(inv_temp = (1/(.00008617*(temperature + 273.15)))) %>% 
	group_by(experiment) %>% 
	ggplot(aes(x = inv_temp, y = log(generation_time), color = experiment)) + geom_point(size = 2) +
	scale_x_reverse() + geom_smooth(method = "lm", aes(fill = experiment)) +
	scale_color_viridis_d() + scale_fill_viridis_d() +
	ylab("ln(Generation time) (days)") + xlab("Temperature (1/kT)") +
	geom_point(size = 2, shape = 1) +
	geom_point(size = 2) 
ggsave("figures/gen-times-color.pdf", width = 6, height = 4)

write_csv(all_gens, "data-processed/all_generation_times.csv")
	