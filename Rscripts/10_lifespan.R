

library(tidyverse)
library(lubridate)
library(cowplot)


lifespan <- read_csv("data-raw/lifespan-data.csv") %>% 
	filter(!is.na(temperature))


ls2 <- lifespan %>% 
	gather(birth_date, death_date, 9:50, key = "clutch_no", value = "date") %>% 
	mutate(date = mdy(date)) %>% 
	spread(key = clutch_no, value = date)

ls3 <- ls2 %>% 
	filter(temperature > 10) %>% 
	mutate(lifespan_calc = interval(birth_date, death_date)/ddays(1)) 


ls3 %>% 
	ggplot(aes(x = temperature, y = lifespan_calc)) + geom_point() +
	geom_smooth(method = "lm") +
	ylab("Lifespan (days)") + xlab("Temperature (°C)")

ls3 %>% 
	lm(lifespan_calc ~ temperature, data = .) %>% summary()


ls3 %>% 
	ggplot(aes(x = log(max_body_size), y = log(lifespan_calc), color = temperature)) + geom_point() +
	geom_smooth(method = "lm")
	

ls3 %>% 
	filter(temperature > 10) %>% 
	lm(log(lifespan_calc) ~ log(max_body_size), data = .) %>% summary()
