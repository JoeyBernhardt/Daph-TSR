

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
	ggplot(aes(x = log(mass), y = log(days_to_clutch1*(mass^0.25)))) + geom_point() +
	geom_smooth(method = "lm", color = "black") + ylab("Generation time (days)") + xlab("ln(Mass)")

ls3 %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	ggplot(aes(x = inv_temp, y = log(days_to_clutch1*(mass^0.25)))) + geom_jitter(width = 0.1) +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Generation time) (days)") + xlab("Temperature") +
	scale_x_reverse()

ls3 %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	ggplot(aes(x = inv_temp, y = log(days_to_clutch1))) + geom_jitter(width = 0.1) +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Generation time) (days)") + xlab("Temperature") +
	scale_x_reverse()



ls3 %>% 
	lm(log(lifespan_calc) ~ log(max_body_size), data = .) %>% summary()

ls3 %>% 
	lm(log(lifespan_calc) ~ log(mass^(1/4)), data = .) %>% summary()

ls3 %>% 
	lm(log(days_to_clutch1) ~ inv_temp, data = .) %>% summary()

ls3 %>% 
	lm(log(days_to_clutch1) ~ inv_temp, data = .) %>% tidy(conf.int = TRUE)
