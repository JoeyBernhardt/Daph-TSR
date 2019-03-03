library(tidyverse)


acclimated_clutches <- read_csv("data-processed/acclimated-fecundity.csv") %>% 
	rename(fecundity = final_baby_count)

ls_clutches <- read_csv("data-raw/lifespan-clutches.csv") %>% 
	select(temperature, replicate, clutch_number, baby_count) %>% 
	mutate(experiment = "lifespan") %>% 
	rename(fecundity = baby_count)

winter_tsr_babies <- read_csv("data-raw/winter_tsr_babies.csv") %>% 
	select(temperature, replicate, clutch_number, number_of_babies) %>% 
	mutate(experiment = "tsr") %>%
	rename(fecundity = number_of_babies)


all_fecundities <- bind_rows(acclimated_clutches, ls_clutches, winter_tsr_babies) 


all_generation_times <- read_csv("data-processed/generation_times_all.csv") %>% 
	filter(experiment != "summer2016") %>% 
	mutate(replicate = as.integer(replicate)) %>% 
	filter(experiment != "lifespan")

all_gens_old <- read_csv("data-processed/all_generation_times.csv") %>% 
	filter(experiment == "lifepsan") %>% 
	mutate(experiment = ifelse(experiment == "lifepsan", "lifespan", experiment)) %>% 
	mutate(replicate = as.integer(replicate))

all_gens2 <- bind_rows(all_generation_times, all_gens_old)

all_generation_times %>% 
	filter(experiment == "lifepsan") %>% 
	

unique(all_generation_times$experiment)
unique(all_fecundities$experiment)

all_nt <- left_join(all_fecundities, all_gens2)

all_nt %>% 
	filter(experiment == "lifespan") %>% View


all_nt %>% 
	filter(clutch_number == 1) %>% 
	ggplot(aes(x = fecundity, y = generation_time, color = factor(temperature))) + geom_point() +
	geom_smooth(method = "lm") 

all_nt %>% 
	filter(clutch_number == 1) %>% 
	ggplot(aes(x = fecundity, y = generation_time, fill = factor(temperature), color = factor(temperature))) + geom_point() +
	geom_smooth(method = "lm") + ylab("Generation time (days)") + xlab("Fecundity (babies/clutch)")
ggsave("figures/generation_time_fecundity_colour.png", width = 8, height = 6)
