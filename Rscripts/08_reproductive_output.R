

library(dplyr)
library(readr)
library(janitor)
library(ggplot2)
library(broom)
library(lubridate)
library(viridis)
library(plotrix)
library(tidyr)
library(car)
library(FSA)
library(lmodel2)



# read data ---------------------------------------------------------------

number_of_clutches <- read_csv("data-raw/lifespan_clutches.csv", n_max = 40)
total_clutches_27 <- read_csv("data-raw/total_clutches_27.csv")
babies <- read_csv("data-raw/lifespan_clutch_numbers.csv")

babies27 <- babies %>% 
	filter(temperature == 27)

clutch27 <- number_of_clutches %>% 
	filter(temperature == 27) %>% 
	mutate(mass =  0.00402*((max_body_size)^2.66))

all27 <- left_join(clutch27, total_clutches_27)

all27 %>% 
	mutate(lifespan = as.numeric(lifespan)) %>% 
	ggplot(aes(x = total_clutches, y = mass)) + geom_point() +
	geom_smooth(method = "lm") 

all27 %>% 
	mutate(lifespan = as.numeric(lifespan)) %>% 
	do(tidy(lm(log(total_clutches)/log(mass) ~ log(mass), data = .), conf.int = TRUE)) %>% 
	View



baby_sum <- babies27 %>% 
	group_by(replicate) %>% 
	summarise_each(funs(mean, std.error), baby_count)

all27_2 <- left_join(all27, baby_sum, by = "replicate")

all3 <- all27_2 %>% 
	mutate(rep_out = baby_count_mean*total_clutches)

all3 %>% 
	ggplot(aes(x = mass, y = rep_out)) + geom_point() +
	geom_smooth(method = "lm") + ylab("Lifetime reproductive output") + xlab("maximum body mass (mg)")

all3 %>% 
	do(tidy(lm(total_clutches ~ mass, data = .), conf.int = TRUE)) %>% View
	summary()