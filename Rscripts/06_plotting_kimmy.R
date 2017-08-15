

library(googlesheets)
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


# read in data ------------------------------------------------------------

size <- read_csv("data-raw/daph_tsr_body_size.csv")

size2 <- clean_names(size)


size2 %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage == "clutch3") %>% 
	ggplot(aes(x = temperature, y = actual_size_um)) + geom_point() +
	geom_smooth(method = "lm") + theme_bw() + ylab("Body length") + xlab("Temperature")


model_results <- size2 %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage == "clutch3") %>%
	lm(actual_size_um ~ temperature, data = .) %>% 
	tidy
	
write_csv(model_results, "data-processed/model_results.csv")


# now moving on to acclimated daphnia -------------------------------------


acc <- read_csv("data-raw/acc_daph_body_size.csv")

acc2 <- clean_names(acc)

acc3 <- acc2 %>% 
	mutate(date_measured = mdy(date_measured))  

acc3 %>% 
	filter(!is.na(size_um)) %>% 
	filter(temperature == 24) %>% 
	ggplot(aes(x = date_measured, y = size_um, color = factor(replicate), group = replicate)) + 
	geom_point(size = 2) + 
	geom_line(size = 1) +
	facet_wrap( ~ temperature) +scale_color_viridis(discrete = TRUE) + theme_bw()


acc3 %>% 
	filter(!is.na(size_um)) %>% 
	group_by(stage, temperature) %>% 
	summarise(mean_size = mean(size_um),
						std_error = std.error(size_um)) %>%
	ggplot(aes(x = stage, y = mean_size, group = temperature, color = factor(temperature))) + geom_point() +
	geom_line() + geom_errorbar(aes(ymin = mean_size - std_error, ymax = mean_size + std_error), width = 0.1)


## now try fitting VBGM

acc4 <- acc3 %>% 
	filter(!is.na(size_um)) %>% 
	select(stage, size_um, temperature, replicate) %>% 
	spread(key = stage, value = size_um) %>% 
	select(temperature, replicate, neonate, everything())


date_10 <- acc3 %>% 
	filter(temperature == 10) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, date_measured, stage) %>%
	spread(key = stage, value = date_measured) %>% 
	rename(neonate_date = neonate,
				 clutch1_date = clutch1,
				 clutch2_date = clutch2,
				 clutch3_date = clutch3,
				 clutch4_date = clutch4,
				 clutch5_date = clutch5)


size_10 <- acc3 %>% 
	filter(temperature == 10) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, size_um, stage) %>%
	spread(key = stage, value = size_um) %>% 
	rename(neonate_size = neonate,
				 clutch1_size = clutch1,
				 clutch2_size = clutch2,
				 clutch3_size = clutch3,
				 clutch4_size = clutch4,
				 clutch5_size = clutch5)


all_10 <- left_join(date_10, size_10, by = c("temperature", "replicate"))
				 

wide10 <- all_10 %>% 
	mutate(clutch1_age = interval(neonate_date, clutch1_date)/ddays(1)) %>%
	mutate(clutch2_age = interval(neonate_date, clutch2_date)/ddays(1)) %>%  
mutate(clutch3_age = interval(neonate_date, clutch3_date)/ddays(1)) %>%  
	mutate(clutch4_age = interval(neonate_date, clutch4_date)/ddays(1)) %>%
	mutate(clutch5_age = interval(neonate_date, clutch3_date)/ddays(1)) %>% 
	select(-contains("date")) %>% 
	mutate(neonate_age = 0)
	
age_10 <- wide10 %>% 
	select(temperature, replicate, contains("age")) %>%
	gather(key = clutch, value = age, 3:8) %>%
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)


length_10 <- wide10 %>% 
	select(temperature, replicate, contains("size")) %>% 
	gather(key = clutch, value = length, 3:8) %>%
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)

data_10 <- left_join(age_10, length_10, by = c("temperature", "replicate", "clutch"))



data_10_2 <- data_10 %>% 
	rename(Length = length) %>% 
	rename(Age = age) %>% 
	filter(!is.na(Length), !is.na(Age)) 
data_10_3 <- data_10 %>% 
	rename(tl = length) %>% 
		filter(!is.na(tl), !is.na(age)) %>% 
	mutate(age = ifelse(age == 0, 1, age))

data_10_2 %>% 
	ggplot(aes(x = age, y = tl, color = factor(replicate))) + geom_point()

svTypical <- list(Linf=2377,K=2.0896,t0=-0.185)
vbTypical <- tl~Linf*(1-exp(-K*(age-t0)))

fitTypical <- nls(vbTypical,data=data_10_3,start=vb)

vb <- vbStarts(tl~age,data=data_10_3,type="typical")
vb

vbT <- vbFuns("typical")


svTF <- vbStarts(tl~age,data=data_10_3,type="typical")

start_list <- c(Linf = 2377.364, t0 = -4, K = 0.1)
fit_model <- nls(tl~vbT(age,Linf,K,t0),data=data_10_3,start=start_list)

fit<-  coef(fit_model)


fitcurve <- function(x){
	res<- fit[["Linf"]]*(1-exp(-fit[["K"]]*(x - fit[["t0"]])))
	res
}


vbcurve <-function(x){
	res<- svTF$Linf*(1-exp(-svTF$K*(x - svTF$t0)))
	res
}

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) 
p + geom_point(aes(x = age, y = tl, color = factor(replicate)), data = data_10_3) +
	stat_function(fun = fitcurve)
