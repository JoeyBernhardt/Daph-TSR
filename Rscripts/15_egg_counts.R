

library(tidyverse)
library(here)
library(cowplot)
library(broom)


daph_files <- list.files(path= "data-raw/daphnia-clutches-imagej/done", pattern="*.csv", recursive = TRUE, full.names = TRUE)

names(daph_files) <- daph_files %>% 
	gsub(pattern = ".csv$", replacement = "")


all_daph <- map_df(daph_files, read_csv, .id = "file_name") %>% 
	separate(col = file_name, into = c("path", "photo_id"), sep = "imagej/") %>% 
	mutate(photo_id = str_replace(photo_id, "-size", ""))


lengths <- all_daph %>% 
	filter(!is.na(Length)) %>% 
	select(photo_id, Length)

clutch_numbers <- all_daph %>% 
	filter(is.na(Length)) %>% 
	group_by(photo_id) %>% 
	tally()
	

all_clutches <- left_join(clutch_numbers, lengths) %>% 
	separate(photo_id, into= c("path2", "temperature", "replicate", "clutch", sep = "-")) %>% 
	rename(date = temperature, 
				 temperature = replicate,
				 replicate = clutch, 
				 clutch = `-`) %>% 
	mutate(clutch = str_replace(clutch, "clutch", "")) %>% 
	mutate(clutch_number = as.numeric(clutch) + 1) %>% 
	mutate(length_mm = Length/1.593) %>% 
	mutate(unique_id = paste(temperature, replicate, sep = "_")) %>% 
	mutate(temperature = as.numeric(temperature)) %>% 
	mutate(replicate = as.numeric(replicate)) %>% 
	filter(!grepl("apr", date))  # to just get the acclimated experiment version

### scale is 1.593 pixels = 1mm

length(unique(all_clutches$unique_id))

all_clutches %>% 
	filter(!grepl("apr", date)) %>% 
	ggplot(aes(x = length_mm, y = n)) + geom_point()+
	geom_smooth(method = "lm", color = "black") +
	ylab("Number of eggs") + xlab("Body length (mm)")
ggsave("figures/eggs_v_length.png", width = 4.5, height = 4)

all_clutches %>%
	group_by(temperature) %>% 
	do(tidy(lm(n ~ Length, data = .), conf.int = TRUE)) %>% View


all_clutches %>%
	filter(!grepl("apr", date)) %>% 
	lm(n ~ Length, data = .) %>% summary()

library(lme4)
library(MuMIn)

mm <- all_clutches %>%
	filter(!grepl("apr", date)) %>% 
	lmer(n ~ Length + (1|clutch_number) + (1|unique_id), data = .,  REML=FALSE)

summary(mm)
r.squaredGLMM(mm)
anova(mm)
coef(mm)

fecundity <- read_csv("data-processed/acclimated-fecundity.csv") 


all_fecund <- left_join(fecundity, all_clutches)


all_fecund %>% 
	ggplot(aes(x = length_mm, y = initial_baby_count)) + geom_point()

all_fecund %>% 
	ggplot(aes(x = n, y = final_baby_count)) + geom_point() +
	geom_abline(yintercept = 0, slope = 1)

all_fecund %>% 
	ggplot(aes(x = final_baby_count, y = initial_baby_count)) + geom_point()


all_fecund %>%
	rename(egg_number = n) %>% 
	gather(key = fecundity_stage, value = fecundity, egg_number, final_baby_count) %>% 
	ggplot(aes(x = length_mm, y = fecundity, color = fecundity_stage)) + geom_point() +
	geom_smooth(method = "lm") +
	facet_wrap( ~ fecundity_stage)


all_fecund %>%
	rename(egg_number = n) %>%
	ggplot(aes(x = length_mm, y = egg_number)) + geom_point() + geom_smooth(method = "lm")

all_fecund %>%
	rename(egg_number = n) %>%
	gather(key = fecundity_stage, value = fecundity, egg_number, initial_baby_count) %>% 
	group_by(fecundity_stage) %>% 
	do(tidy(lm(fecundity ~ length_mm, data = .), conf.int = TRUE)) %>% View

	