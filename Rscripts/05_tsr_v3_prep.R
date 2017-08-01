

library(tidyverse)
library(janitor)
library(broom)
library(lubridate)
library(stringr)


kimmy_data_raw <- read_csv("data-raw/daph-kimmy.csv")

kd <- clean_names(kimmy_data_raw)

kd <- kd %>% 
	mutate(date_measured = mdy(date_measured)) 

kd %>% 
	filter(actual_size_um != 0) %>% 
	filter(stage != "neonate") %>% 
	filter(stage == "clutch3") %>% 
	filter(temperature_c > 10) %>% 
	group_by(temperature_c) %>%
	summarise(mean_length = mean(actual_size_um)) %>% 
ggplot(aes(x = temperature_c, y = mean_length)) + geom_point()


kd %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage != "neonate") %>% 
	filter(stage == "clutch3") %>% 
	filter(temperature_c > 10) %>% 
	ggplot(aes(x = temperature_c, y = actual_size_um)) + geom_point(size = 4, alpha = 0.5, color = "blue") + geom_smooth(method = "lm") +
	theme_bw() + xlab("temperature (C)") + ylab("body length (um)")


kd %>% 
	filter(actual_size_um > 0) %>% 
	filter(temperature_c > 10) %>% 
	filter(stage != "neonate") %>% 
	# filter(stage == "clutch3") %>% 
	ggplot(aes(x = temperature_c, y = actual_size_um, color = stage, label = replicate)) + geom_point() + geom_smooth(method = "lm") +
facet_wrap( ~ stage) + geom_text(nudge_x = 0.5)



### how much does body size decrease with increasing temperature?
kd %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage != "neonate") %>%
	filter(stage == "clutch3") %>% 
	filter(temperature_c > 10) %>% 
	do(tidy(lm(actual_size_um ~ temperature_c, data = .), conf.int = TRUE)) %>% View

## slope here is -23.00446 which corresponds to 
## without the 10C, it's -28.52905, which corresponds to 1.19% change per C

kd %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage != "neonate") %>%
	filter(stage == "clutch3") %>% 
	do(tidy(lm(actual_size_um ~ temperature_c, data = .), conf.int = TRUE)) %>% View

	

kd %>% 
	filter(actual_size_um > 0) %>%
	unite(unique_id, temperature_c, replicate, remove = FALSE) %>% 
	group_by(unique_id) %>% 
	ggplot(aes(x = date_measured, y = actual_size_um, color = factor(temperature_c), group = unique_id)) + geom_point() + geom_line() +
	facet_wrap( ~ temperature_c)


## try this again, need to split by lifestage

neonate <- kd %>% 
	filter(stage == "neonate") %>% 
	select(temperature_c, replicate, actual_size_um, stage, date_measured) %>% 
	unite(unique_id, temperature_c, replicate, remove = FALSE) %>% 
	rename(neonate_date = date_measured) %>% 
	rename(neonate_size = actual_size_um) %>% 
	select(-stage) %>% 
	select(-temperature_c)

clutch1 <- kd %>% 
	filter(stage == "clutch1") %>% 
	select(temperature_c, replicate, actual_size_um, stage, date_measured) %>% 
	unite(unique_id, temperature_c, replicate, remove = FALSE) %>% 
	rename(clutch1_date = date_measured) %>% 
	rename(clutch1_size = actual_size_um) %>% 
	select(-stage)


clutch2 <- kd %>% 
	filter(stage == "clutch2") %>% 
	select(temperature_c, replicate, actual_size_um, stage, date_measured) %>% 
	unite(unique_id, temperature_c, replicate, remove = FALSE) %>% 
	rename(clutch2_date = date_measured) %>% 
	rename(clutch2_size = actual_size_um) %>% 
	select(-stage)

clutch3 <- kd %>% 
	filter(stage == "clutch3") %>% 
	select(temperature_c, replicate, actual_size_um, stage, date_measured) %>% 
	unite(unique_id, temperature_c, replicate, remove = FALSE) %>% 
	rename(clutch3_date = date_measured) %>% 
	rename(clutch3_size = actual_size_um) %>% 
	select(-stage)


all <- left_join(neonate, clutch1)
all2 <- left_join(all, clutch2)
all3 <- left_join(all2, clutch3)

### size rate trade-off figure
all3 %>% 
	filter(clutch1_size > 0) %>% 
	# filter(clutch2_size > 0) %>% 
	filter(clutch3_size > 0) %>% 
	# filter(temperature_c < 27) %>% 
	filter(clutch1_size > 1750) %>%
	rename(temperature = temperature_c) %>% 
	mutate(temperature = as.factor(temperature)) %>% 
	mutate(time_to_1st_clutch = clutch1_date - neonate_date) %>%
	mutate(time_to_1st_clutch = as.numeric(as.character(time_to_1st_clutch))) %>% 
	mutate(somatic_growth_rate = ((clutch1_size - neonate_size)/time_to_1st_clutch)) %>% 
	mutate(growth_rate_per_hour = somatic_growth_rate/24) %>% 
	mutate(growth_per_mass = growth_rate_per_hour/clutch1_size) %>% 
	ggplot(aes(x = growth_per_mass, y = clutch3_size, color = temperature)) + geom_point(size = 4) +
	geom_smooth(method = "lm", color = "#619CFF") +
	xlab("size-specific somatic growth rate (um/hour*um)") + ylab("clutch 3 length (um)") +
	theme_minimal() 

### growth rate vs temperature
all3 %>% 
	filter(clutch1_size > 0) %>% 
	filter(clutch1_size > 1750) %>%
	mutate(time_to_1st_clutch = clutch1_date - neonate_date) %>%
	mutate(time_to_1st_clutch = as.numeric(as.character(time_to_1st_clutch))) %>% 
	mutate(somatic_growth_rate = ((clutch1_size - neonate_size)/time_to_1st_clutch)) %>% 
	mutate(growth_rate_per_hour = somatic_growth_rate/24) %>% 
	ggplot(aes(x = temperature_c, y = somatic_growth_rate, color = factor(temperature_c))) + geom_point(size = 4, alpha = 0.5) +
	geom_smooth(method = "lm", color = "#619CFF") +
	xlab("temperature") + ylab("somatic growth rate") +
	theme_minimal() 


### time to first clutch vs temperature
all3 %>% 
	filter(clutch1_size > 0) %>% 
	filter(clutch1_size > 1750) %>%
	mutate(time_to_1st_clutch = clutch1_date - neonate_date) %>%
	mutate(time_to_1st_clutch = as.numeric(as.character(time_to_1st_clutch))) %>% 
	mutate(somatic_growth_rate = ((clutch1_size - neonate_size)/time_to_1st_clutch)) %>% 
	mutate(growth_rate_per_hour = somatic_growth_rate/24) %>% 
	ggplot(aes(x = temperature_c, y = log(time_to_1st_clutch))) + geom_jitter(size = 4, alpha = 0.5, width = 0.2, color = "#619CFF") +
	geom_smooth(method = "lm", color = "#619CFF") +
	xlab("temperature") + ylab("time to first clutch") +
	theme_minimal()


### slope on the size rate trade-off
all3 %>% 
	filter(clutch1_size > 0) %>% 
	# filter(clutch2_size > 0) %>%
	filter(clutch3_size > 0) %>%
	# filter(temperature_c < 27) %>%
	filter(clutch1_size > 1750) %>% 
	mutate(time_to_1st_clutch1 = interval(neonate_date, clutch1_date)/ddays(1)) %>%
	mutate(time_to_1st_clutch = clutch1_date - neonate_date) %>% 
	mutate(time_to_1st_clutch = as.numeric(as.character(time_to_1st_clutch))) %>% 
	mutate(time_to_3rd_clutch = clutch3_date - neonate_date) %>% 
	mutate(time_to_3rd_clutch = as.numeric(as.character(time_to_3rd_clutch))) %>% 
	mutate(somatic_growth_rate = ((clutch1_size - neonate_size)/time_to_1st_clutch1)) %>%
	mutate(growth_rate_per_hour = somatic_growth_rate/24) %>% 
	mutate(growth_per_mass = growth_rate_per_hour/clutch1_size) %>% 
do(tidy(lm(log(clutch1_size) ~ log(growth_per_mass), data = .), conf.int = TRUE)) %>% View

### slope we get here is -0.1301637


#### Now bring in the clutch size data

babies <- read_csv("data-raw/daph-babies.csv")

kb <- clean_names(babies)

kb2 <- kb %>% 
	mutate(clutch_number = str_replace(clutch_number, "1", "clutch1")) %>%
	mutate(clutch_number = str_replace(clutch_number, "2", "clutch2")) %>%
	mutate(clutch_number = str_replace(clutch_number, "3", "clutch3")) %>%
	unite(unique_id, temperature_c, replicate, remove = FALSE) %>% 
	select(unique_id, clutch_number, number_of_babies, temperature_c, replicate)

kd2 <- kd %>% 
	select(temperature_c, replicate, actual_size_um, stage) %>% 
	unite(unique_id, temperature_c, replicate, remove = FALSE) %>% 
	filter(stage != "neonate") %>% 
	rename(clutch_number = stage)
	

kb3 <- left_join(kd2, kb2, by = c("clutch_number", "unique_id"))


### figure with clutch size vs body size
kb3 %>% 
	filter(actual_size_um > 0) %>% 
	# filter(clutch_number == "clutch1") %>% 
	# group_by(temperature_c.x) %>% 
	ggplot(aes(x = actual_size_um, y = number_of_babies)) + geom_point(size = 3) +
	geom_smooth(method = "lm") + facet_wrap( ~ temperature_c.x) + theme_bw() + xlab("body length (um)") + ylab("clutch size")


