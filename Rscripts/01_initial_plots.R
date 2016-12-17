## Daph TSR initial plots and analysis
## Last updated by JB Dec 12 2016



# load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)
library(broom)

# read in data ------------------------------------------------------------

data3 <- read_csv("data-processed/data3.csv")



# plots! ------------------------------------------------------------------

data3 %>% 
	filter(unique_id != "K_16") %>% ## something weird is going on here!
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	ggplot(data = ., aes(x = inverse_temp, y = log(time_to_first_clutch), label = id)) + geom_point(size = 4, color = "#619CFF") +
	geom_smooth(method = "lm", color = "#619CFF") +
	scale_x_reverse() + xlab("temperature (1/kT)") + ylab("ln time to reproductive maturity") + 
	theme_minimal() + 
	theme(axis.text.y   = element_text(size=20),
				axis.text.x   = element_text(size=20),
				axis.title.y  = element_text(size=20),
				axis.title.x  = element_text(size=20),
				panel.background = element_blank(),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				axis.line = element_line(colour = "black"),
				axis.ticks = element_line(size = 1)) +
	theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=1, lineend="square"))

data3 %>% 
	filter(unique_id != "K_16") %>% ## something weird is going on here!
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	do(tidy(lm(log(time_to_first_clutch) ~ inverse_temp, data = .), conf.int = TRUE)) %>% 
	View


# time between clutches ---------------------------------------------------

data3 %>% 
	filter(unique_id != "K_16") %>% ## something weird is going on here!
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	ggplot(data = ., aes(x = inverse_temp, y = log(time_btw_1_2), label = id)) + geom_point(size = 4, color = "#619CFF") +
	geom_smooth(method = "lm", color = "#619CFF") +
	scale_x_reverse() + xlab("temperature (1/kT)") + ylab("time between clutches") +
	theme_minimal() + 
	theme(axis.text.y   = element_text(size=20),
				axis.text.x   = element_text(size=20),
				axis.title.y  = element_text(size=20),
				axis.title.x  = element_text(size=20),
				panel.background = element_blank(),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				axis.line = element_line(colour = "black"),
				axis.ticks = element_line(size = 1)) +
	theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=1, lineend="square"))

## time between clutches
data3 %>% 
	filter(unique_id != "K_16") %>% ## something weird is going on here!
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	do(tidy(lm(log(time_btw_1_2) ~ inverse_temp, data = .), conf.int = TRUE)) %>% 
	View


# body size ---------------------------------------------------------------


data3 %>% 
	filter(unique_id != "K_16") %>% ## something weird is going on here!
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	ggplot(data = ., aes(x = inverse_temp, y = log(length_at_4th_clutch), label = id)) + geom_point(size = 4, color = "#619CFF") +
	geom_smooth(method = "lm", color = "#619CFF") +
	scale_x_reverse() + xlab("temperature (1/kT)") + ylab("length at 1st clutch") 

data4 <- data3 %>% 
	gather(clutch_number, length, starts_with("length")) %>% 
	mutate(clutch_number = str_replace(clutch_number, "length_at_1st_clutch", "1st clutch")) %>% 
	mutate(clutch_number = str_replace(clutch_number, "length_at_2nd_clutch_um", "2nd clutch")) %>% 
	mutate(clutch_number = str_replace(clutch_number, "length_at_3rd_clutch", "3rd clutch")) %>% 
	mutate(clutch_number = str_replace(clutch_number, "length_at_4th_clutch", "4th clutch")) %>% 
	mutate(clutch_number = str_replace(clutch_number, "length_at_birth_um", "birth")) 
	

data4 %>% 
	filter(unique_id != "K_16") %>% ## something weird is going on here!
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	group_by(clutch_number) %>% 
	ggplot(data = ., aes(x = inverse_temp, y = log(length), color = factor(clutch_number))) + geom_point(size = 5, alpha = 0.5) +
	geom_smooth(method = "lm") +
	scale_x_reverse() + xlab("temperature (1/kT)") + ylab("ln length (um)") +
	theme_minimal() + 
	theme(axis.text.y   = element_text(size=20),
				axis.text.x   = element_text(size=20),
				axis.title.y  = element_text(size=20),
				axis.title.x  = element_text(size=20),
				panel.background = element_blank(),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				axis.line = element_line(colour = "black"),
				axis.ticks = element_line(size = 1),
				legend.title = element_blank()) +
	theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=1, lineend="square"))



# somatic growth rate -----------------------------------------------------

## get to somatic growth rate by taking the difference in length at 1st clutch and length at birth, divided by the days to first clutch

data3 %>% 
	filter(unique_id != "K_16") %>% ## something weird is going on here!
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	mutate(somatic_growth_rate = ((length_at_1st_clutch - length_at_birth_um)/time_to_first_clutch)) %>%
	ggplot(data = ., aes(x = inverse_temp, y = log(somatic_growth_rate), label = id)) + geom_point(size = 4, color = "#619CFF", alpha = 0.5) +
	geom_smooth(method = "lm", color = "#619CFF") +
	scale_x_reverse() + xlab("temperature (1/kT)") + ylab("ln somatic growth rate (um/day)") +
	theme_minimal() + 
	theme(axis.text.y   = element_text(size=20),
				axis.text.x   = element_text(size=20),
				axis.title.y  = element_text(size=20),
				axis.title.x  = element_text(size=20),
				panel.background = element_blank(),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				axis.line = element_line(colour = "black"),
				axis.ticks = element_line(size = 1),
				legend.title = element_blank()) +
	theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=1, lineend="square"))
	

### somatic growth rate vs temperature
data3 %>% 
	filter(unique_id != "K_16") %>% ## something weird is going on here!
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	mutate(somatic_growth_rate = ((length_at_1st_clutch - length_at_birth_um)/time_to_first_clutch)) %>%
do(tidy(lm(log(somatic_growth_rate) ~ inverse_temp, data = .), conf.int = TRUE)) %>% 
	View


data5 <- data3 %>% 
	filter(unique_id != "K_16") %>% ## something weird is going on here!
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	mutate(somatic_growth_rate = ((length_at_1st_clutch - length_at_birth_um)/time_to_first_clutch)) %>% 
	select(unique_id, somatic_growth_rate)


data6 <- left_join(data4, data5)


max_lengths <- data6 %>% 
	filter(clutch_number != "birth") %>% 
	group_by(temperature, unique_id) %>% 
	summarise(max_length = max(length, na.rm = TRUE)) 
	
data7 <- left_join(data6, max_lengths)

write_csv(data7, "data-processed/data7.csv")
	
data7 %>% 
ggplot(aes(x = somatic_growth_rate, y = max_length)) + geom_point(size = 4, color = "#619CFF", alpha = 0.5) +
geom_smooth(method = "lm", color = "#619CFF") +
	xlab("somatic growth rate (um/day)") + ylab("max body length (um)") +
	theme_minimal() + 
	theme(axis.text.y   = element_text(size=20),
				axis.text.x   = element_text(size=20),
				axis.title.y  = element_text(size=20),
				axis.title.x  = element_text(size=20),
				panel.background = element_blank(),
				panel.grid.major = element_blank(), 
				panel.grid.minor = element_blank(),
				axis.line = element_line(colour = "black"),
				axis.ticks = element_line(size = 1),
				legend.title = element_blank()) +
	theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=1, lineend="square"))
