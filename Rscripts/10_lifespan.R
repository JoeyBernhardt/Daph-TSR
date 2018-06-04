

library(tidyverse)
library(lubridate)
library(cowplot)
library(broom)


lifespan <- read_csv("data-raw/lifespan-data.csv") %>% 
	filter(!is.na(temperature))


ls2 <- lifespan %>% 
	gather(birth_date, death_date, 9:50, key = "clutch_no", value = "date") %>% 
	mutate(date = mdy(date)) %>% 
	spread(key = clutch_no, value = date)

ls3 <- ls2 %>% 
	# filter(temperature > 10) %>% 
	mutate(lifespan_calc = interval(birth_date, death_date)/ddays(1)) %>% 
	mutate(mass =  0.00402*((max_body_size/1000)^2.66)) %>% 
	mutate(inv_temp = (1/(.00008617*(temperature + 273.15))))


ls3 %>% 
	ggplot(aes(x = temperature, y = lifespan_calc)) + geom_point() +
	geom_smooth(method = "lm") +
	ylab("Lifespan (days)") + xlab("Temperature (°C)")

ls3 %>% 
	lm(lifespan_calc ~ temperature, data = .) %>% summary()


ls3 %>% 
	ggplot(aes(x = log(mass), y = log(lifespan_calc))) + geom_point() +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Lifespan) (days)") + xlab("ln(Mass)")


ls3 %>% 
	filter(temperature > 10) %>%
	ggplot(aes(x = log(mass), y = log(days_to_clutch1*(mass^0.25)))) + geom_point() +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Generation time) (days)") + xlab("ln(Mass)")

ls3 %>% 
	filter(temperature > 10) %>%
	ggplot(aes(x = log(mass), y = log(lifespan_calc*(mass^0.25)))) + geom_point() +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Generation time) (days)") + xlab("ln(Mass)")


ls3 %>% 
	filter(temperature > 10) %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	ggplot(aes(x = inv_temp, y = log(lifespan_calc*(mass^0.25)))) + geom_point() +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Lifespan x M^0.25) (days)") + xlab("Temperature") +
	scale_x_reverse()
ggsave("figures/mass_corr_lifespan_v_temp.pdf", width = 6, height = 4)


### generation time
ls3 %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	filter(temperature > 10) %>% 
	ggplot(aes(x = inv_temp, y = log(days_to_clutch1))) + geom_jitter(width = 0.01) +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Generation time) (days)") + xlab("Temperature (1/kT)") +
	scale_x_reverse()
ggsave("figures/generationtime_v_temp.pdf", width = 6, height = 4)





ls3 %>% 
	# filter(temperature > 10) %>% 
	lm(log(days_to_clutch1) ~ inv_temp, data = .) %>% tidy(conf.int = TRUE)

ls3 %>% 
	lm(log(lifespan_calc) ~ log(max_body_size), data = .) %>% summary()

ls3 %>% 
	filter(temperature > 10) %>% 
	lm(log(lifespan_calc) ~ log(mass), data = .) %>% tidy(conf.int = TRUE)

ls3 %>% 
	filter(temperature > 10) %>% 
	lm(log(lifespan_calc) ~ inv_temp, data = .) %>% tidy(conf.int = TRUE)

ls4 <- ls3 %>% 
	mutate(temp_kelvin = temperature + 273.15) %>%
	mutate(temp_corr_lifespan = lifespan_calc*(exp((0.51/0.00008617)*((1/temp_kelvin)-(1/293.15))))) %>% 
	mutate(temp_corr_gen = days_to_clutch1*(exp((0.51/0.00008617)*((1/temp_kelvin)-(1/293.15)))))

### somehow figure out how to get generation time corrected to 20C
ls4 %>% 
	filter(temperature > 10) %>% 
	lm(log(temp_corr_lifespan) ~ log(mass), data = .) %>% tidy(conf.int = TRUE)


ls4 %>% 
	filter(temperature > 10) %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	ggplot(aes(x = log(mass), y = log(temp_corr_lifespan))) + geom_jitter(width = 0.1) +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Lifespan at 20°C) (days)") + xlab("ln(Mass)") 
ggsave("figures/lifespan_v_mass.pdf", width = 6, height = 4)


ls4 %>% 
	filter(temperature > 10) %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	ggplot(aes(x = log(mass), y = log(temp_corr_gen))) + geom_jitter(width = 0.1) +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Generation time at 20°C) (days)") + xlab("ln(Mass)") +
	scale_x_reverse()
ggsave("figures/generationtime_v_mass.pdf", width = 6, height = 4)

ls3 %>% 
	filter(temperature > 10) %>% 
	lm(log(days_to_clutch1) ~ inv_temp, data = .) %>% summary()

ls3 %>% 
	filter(temperature > 10) %>% 
	lm(log(days_to_clutch1) ~ inv_temp, data = .) %>% tidy(conf.int = TRUE)

## get total clutches per lifetime

total_clutches <- ls3 %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	gather(starts_with("clutch"), key = "clutch_number", value = "clutch_date") %>% 
	filter(!is.na(clutch_date)) %>% 
	group_by(temperature, replicate) %>% 
	distinct(clutch_number) %>% 
	tally()


## total clutches over lifetime
total_clutches %>% 
	filter(temperature > 10) %>% 
	ungroup() %>% 
	ggplot(aes(x  = temperature, y = n)) + geom_point() + 
	geom_smooth(method = "lm", color = "black") + xlab("Temperature (°C)") + ylab("Lifetime number of clutches")
ggsave("figures/lifetime_clutches.pdf", width = 6, height = 4)


ls5 <- left_join(ls4, total_clutches, by = c("temperature", "replicate"))


ls5 %>% 
	filter(temperature > 10) %>% 
	mutate(clutches_per_time = n/lifespan_calc) %>% 
	ggplot(aes(x = inv_temp, y = log(clutches_per_time))) + geom_point() + scale_x_reverse() +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Clutches per day)") + xlab("Temperature (1/kT)")
ggsave("figures/ln_clutches_per_time.pdf", width = 6, height = 4)

ls5 %>% 
	filter(temperature > 10) %>% 
	mutate(clutches_per_time = n/lifespan_calc) %>% 
	ggplot(aes(x = inv_temp, y = clutches_per_time)) + geom_point() + scale_x_reverse() +
	geom_smooth(method = "lm", color = "black") + ylab("Clutches per day") + xlab("Temperature (1/kT)")
ggsave("figures/clutches_per_time.pdf", width = 6, height = 4)

ls5 %>% 
	filter(temperature > 10) %>% 
	mutate(clutches_per_time = n/lifespan_calc) %>%
	lm(log(clutches_per_time) ~ inv_temp, data = .) %>% tidy(conf.int = TRUE)

ls5 %>% 
	filter(temperature > 10) %>% 
	mutate(clutches_per_time = n/lifespan_calc) %>%
	lm(log(lifespan_calc) ~ inv_temp, data = .) %>% tidy(conf.int = TRUE)


ls5 %>% 
	filter(temperature > 10) %>% 
	mutate(clutches_per_time = n/lifespan_calc) %>% 
	ggplot(aes(x = inv_temp, y = log(lifespan_calc))) + geom_point() + scale_x_reverse() +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Lifespan) (days)") + xlab("Temperature (1/kT)")
ggsave("figures/lifespan_v_temp.pdf", width = 6, height = 4)
