

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
library(lmodel2)
library(cowplot)


# read in data ------------------------------------------------------------

size <- read_csv("data-raw/daph_tsr_body_size.csv")

size2 <- clean_names(size) %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	filter(actual_size_um > 0) %>% 
	mutate(size_um = actual_size_um) %>% 
	filter(!is.na(size_um))


size2 %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage != "neonate") %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	mutate(mass =  0.00402*((actual_size_um/1000)^2.66)) %>% 
	mutate(log_mass =  log(mass)) %>% 
	mutate(stage = ifelse(stage == "clutch1", "A) Size at clutch 1", stage)) %>% 
	mutate(stage = ifelse(stage == "clutch2", "B) Size at clutch 2", stage)) %>% 
	mutate(stage = ifelse(stage == "clutch3", "C) Size at clutch 3", stage)) %>% 
	ggplot(aes(x = inverse_temp, y = log_mass)) + geom_point(size = 3) +
	scale_x_reverse() +
	# scale_x_continuous(sec.axis = sec_axis(~(1/(inverse_temp))+273)) +  
	geom_smooth(method = "lm", color = "black") + theme_bw() + ylab("Log body size (mg DW)") + xlab("Temperature (1/kT)") +
	facet_wrap( ~ stage) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) +
	theme(strip.background = element_rect(colour="white", fill="white")) 
ggsave("figures/size_over_clutches.pdf", width = 9, height = 3.5)
ggsave("figures/size_over_clutches.png", width = 9, height = 3.5)


## what are the slopes on the size vs. temp relationships?
size2 %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage != "neonate") %>% 
	mutate(mass =  0.00402*((actual_size_um/1000)^2.66)) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	mutate(mass =  0.00402*((actual_size_um/1000)^2.66)) %>% 
	mutate(log_mass =  log(mass)) %>% 
	group_by(stage) %>% 
	do(tidy(lm(log_mass ~ inverse_temp, data =.), conf.int = TRUE)) %>% View


max_size <- size2 %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage == "clutch3")

# now get it in the right format!! ----------------------------------------


size3 <- size2 %>% 
	select(stage, size_um, temperature, replicate) %>% 
	spread(key = stage, value = size_um) %>% 
	select(temperature, replicate, neonate, everything()) %>% 
	filter(!is.na(clutch3))


date_10 <- size2 %>% 
	filter(temperature == 10) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, date_measured, stage) %>%
	spread(key = stage, value = date_measured) %>% 
	rename(neonate_date = neonate,
				 clutch1_date = clutch1,
				 clutch2_date = clutch2,
				 clutch3_date = clutch3) %>% 
	filter(!is.na(clutch1_date), !is.na(clutch2_date), !is.na(clutch3_date), !is.na(neonate_date))


size_10 <- size2 %>% 
	filter(temperature == 10) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, size_um, stage) %>%
	spread(key = stage, value = size_um) %>% 
	rename(neonate_size = neonate,
				 clutch1_size = clutch1,
				 clutch2_size = clutch2,
				 clutch3_size = clutch3) %>% 
	# filter(!is.na(clutch1_size), !is.na(clutch2_size), !is.na(clutch3_size), !is.na(neonate_size)) %>% 
	filter(!is.na(clutch3_size))


all_10 <- left_join(size_10, date_10, by = c("temperature", "replicate"))


wide10 <- all_10 %>% 
	mutate(clutch1_age = interval(neonate_date, clutch1_date)/ddays(1)) %>%
	mutate(clutch2_age = interval(neonate_date, clutch2_date)/ddays(1)) %>%  
	mutate(clutch3_age = interval(neonate_date, clutch3_date)/ddays(1)) %>%  
	select(-contains("date")) %>% 
	mutate(neonate_age = 1)

age_10 <- wide10 %>% 
	select(temperature, replicate, contains("age")) %>% 
	gather(key = clutch, value = age, 3:6) %>% 
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)


length_10 <- wide10 %>% 
	select(temperature, replicate, contains("size")) %>% 
	gather(key = clutch, value = length, 3:6) %>%
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)

data_10 <- left_join(age_10, length_10, by = c("temperature", "replicate", "clutch"))



# now fit vb --------------------------------------------------------------
vbT <- vbFuns("typical")

data_10_2 <- data_10 %>% 
	rename(tl = length) %>% 
	filter(!is.na(tl), !is.na(age)) %>% 
	mutate(age = ifelse(age == 0, 1, age))

svTF <- vbStarts(tl~age,data=data_10_2,type="typical")

start_list <- c(Linf = 2377.364, t0 = -4, K = 0.1)



data_10_2 %>% 
	filter(replicate == 1) %>% 
	ggplot(aes(x = age, y = tl)) + geom_line()

## 1, 3, 6

fitted10 <- data_10_2 %>% 
filter(replicate %in% c(2, 4, 5, 7, 8)) %>% 
	group_by(replicate) %>% 
	do(tidy(nls(tl~vbT(age,Linf,K,t0),data=.,start=start_list))) %>% 
	mutate(temperature = 10)

data_10_1 <- data_10_2 %>% 
	filter(replicate == 7) 
fitcurve <- function(x){
	res<- fitted10$estimate[fitted10$term == "Linf" & fitted10$replicate == "7"]*(1-exp(-fitted10$estimate[fitted10$term == "K" & fitted10$replicate == "7"]*(x - fitted10$estimate[fitted10$term == "t0" & fitted10$replicate == "7"])))
	res
}

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) 
p + geom_point(aes(x = age, y = tl, color = factor(replicate)), data = data_10_1) +
	stat_function(fun = fitcurve) + xlim(0, 80)


# now try with 27 ---------------------------------------------------------

size3 <- size2 %>% 
	select(stage, size_um, temperature, replicate) %>% 
	spread(key = stage, value = size_um) %>% 
	select(temperature, replicate, neonate, everything()) %>% 
	filter(!is.na(clutch1), !is.na(clutch2), !is.na(clutch3))


date_27 <- size2 %>% 
	filter(temperature == 27) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, date_measured, stage) %>%
	spread(key = stage, value = date_measured) %>% 
	rename(neonate_date = neonate,
				 clutch1_date = clutch1,
				 clutch2_date = clutch2,
				 clutch3_date = clutch3) %>% 
	filter(!is.na(clutch1_date), !is.na(clutch2_date), !is.na(clutch3_date), !is.na(neonate_date))


size_27 <- size2 %>% 
	filter(temperature == 27) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, size_um, stage) %>%
	spread(key = stage, value = size_um) %>% 
	rename(neonate_size = neonate,
				 clutch1_size = clutch1,
				 clutch2_size = clutch2,
				 clutch3_size = clutch3) %>% 
	filter(!is.na(clutch1_size), !is.na(clutch2_size), !is.na(clutch3_size), !is.na(neonate_size))


all_27 <- left_join(date_27, size_27, by = c("temperature", "replicate"))


wide27 <- all_27 %>% 
	mutate(clutch1_age = interval(neonate_date, clutch1_date)/ddays(1)) %>%
	mutate(clutch2_age = interval(neonate_date, clutch2_date)/ddays(1)) %>%  
	mutate(clutch3_age = interval(neonate_date, clutch3_date)/ddays(1)) %>%  
	select(-contains("date")) %>% 
	mutate(neonate_age = 1)

age_27 <- wide27 %>% 
	select(temperature, replicate, contains("age")) %>% 
	gather(key = clutch, value = age, 3:6) %>% 
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)


length_27 <- wide27 %>% 
	select(temperature, replicate, contains("size")) %>% 
	gather(key = clutch, value = length, 3:6) %>%
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)

data_27 <- left_join(age_27, length_27, by = c("temperature", "replicate", "clutch"))


# now fit vb --------------------------------------------------------------
vbT <- vbFuns("typical")

data_27_2 <- data_27 %>% 
	rename(tl = length) %>% 
	filter(!is.na(tl), !is.na(age)) %>% 
	mutate(age = ifelse(age == 0, 1, age))

svTF <- vbStarts(tl~age,data=data_27_2,type="typical")

start_list <- c(Linf = 2377.364, t0 = -4, K = 0.1)
fit_model <- nls(tl~vbT(age,Linf,K,t0),data=data_27_2,start=start_list)
summary(fit_model)

fit <-  coef(fit_model)


data_27_2 %>% 
	filter(replicate == 1) %>% 
	ggplot(aes(x = age, y = tl)) + geom_line()

fitted27 <- data_27_2 %>% 
	group_by(replicate) %>% 
	do(tidy(nls(tl~vbT(age,Linf,K,t0),data=.,start=start_list))) %>% 
	mutate(temperature = 27)

fitted27 %>% 
	ggplot(aes(x = replicate, y = estimate)) + geom_point() +
	facet_wrap( ~ term, scales = "free")



fitcurve <- function(x){
	res<- fit[["Linf"]]*(1-exp(-fit[["K"]]*(x - fit[["t0"]])))
	res
}


vbcurve <-function(x){
	res<- svTF$Linf*(1-exp(-svTF$K*(x - svTF$t0)))
	res
}

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) 
p + geom_point(aes(x = age, y = tl, color = factor(replicate)), data = data_27_2) +
	stat_function(fun = fitcurve)


size2 %>% 
	ggplot(aes(x = date_measured, y = actual_size_um, group = replicate, color = factor(replicate))) + geom_point() +
	geom_line() +
	facet_wrap( ~ temperature, scales = "free")

# now try with 16 ---------------------------------------------------------

size3 <- size2 %>% 
	select(stage, size_um, temperature, replicate) %>% 
	spread(key = stage, value = size_um) %>% 
	select(temperature, replicate, neonate, everything()) %>% 
	filter(!is.na(clutch1), !is.na(clutch2), !is.na(clutch3))


date_16 <- size2 %>% 
	filter(temperature == 16) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, date_measured, stage) %>%
	spread(key = stage, value = date_measured) %>% 
	rename(neonate_date = neonate,
				 clutch1_date = clutch1,
				 clutch2_date = clutch2,
				 clutch3_date = clutch3) %>% 
	filter(!is.na(clutch1_date), !is.na(clutch2_date), !is.na(clutch3_date), !is.na(neonate_date))


size_16 <- size2 %>% 
	filter(temperature == 16) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, size_um, stage) %>%
	spread(key = stage, value = size_um) %>% 
	rename(neonate_size = neonate,
				 clutch1_size = clutch1,
				 clutch2_size = clutch2,
				 clutch3_size = clutch3) %>% 
	filter(!is.na(clutch1_size), !is.na(clutch2_size), !is.na(clutch3_size), !is.na(neonate_size))


all_16 <- left_join(date_16, size_16, by = c("temperature", "replicate"))


wide16 <- all_16 %>% 
	mutate(clutch1_age = interval(neonate_date, clutch1_date)/ddays(1)) %>%
	mutate(clutch2_age = interval(neonate_date, clutch2_date)/ddays(1)) %>%  
	mutate(clutch3_age = interval(neonate_date, clutch3_date)/ddays(1)) %>%  
	select(-contains("date")) %>% 
	mutate(neonate_age = 1)

age_16 <- wide16 %>% 
	select(temperature, replicate, contains("age")) %>% 
	gather(key = clutch, value = age, 3:6) %>% 
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)


length_16 <- wide16 %>% 
	select(temperature, replicate, contains("size")) %>% 
	gather(key = clutch, value = length, 3:6) %>%
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)

data_16 <- left_join(age_16, length_16, by = c("temperature", "replicate", "clutch"))


# now fit vb --------------------------------------------------------------
vbT <- vbFuns("typical")

data_16_2 <- data_16 %>% 
	rename(tl = length) %>% 
	filter(!is.na(tl), !is.na(age)) %>% 
	mutate(age = ifelse(age == 0, 1, age))

svTF <- vbStarts(tl~age,data=data_16_2,type="typical")

start_list <- c(Linf = 2377.364, t0 = -4, K = 0.1)
fit_model <- nls(tl~vbT(age,Linf,K,t0),data=data_27_2,start=start_list)

fit <-  coef(fit_model)


data_16_2 %>% 
	filter(replicate == 1) %>% 
	ggplot(aes(x = age, y = tl)) + geom_line()

fitted16 <- data_16_2 %>% 
	group_by(replicate) %>% 
	do(tidy(nls(tl~vbT(age,Linf,K,t0),data=.,start=start_list))) %>% 
	mutate(temperature = 16)

# now try with 20 ---------------------------------------------------------

size3 <- size2 %>% 
	select(stage, size_um, temperature, replicate) %>% 
	spread(key = stage, value = size_um) %>% 
	select(temperature, replicate, neonate, everything()) %>% 
	filter(!is.na(clutch1), !is.na(clutch2), !is.na(clutch3))


date_20 <- size2 %>% 
	filter(temperature == 20) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, date_measured, stage) %>%
	spread(key = stage, value = date_measured) %>% 
	rename(neonate_date = neonate,
				 clutch1_date = clutch1,
				 clutch2_date = clutch2,
				 clutch3_date = clutch3) %>% 
	filter(!is.na(clutch1_date), !is.na(clutch2_date), !is.na(clutch3_date), !is.na(neonate_date))


size_20 <- size2 %>% 
	filter(temperature == 20) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, size_um, stage) %>%
	spread(key = stage, value = size_um) %>% 
	rename(neonate_size = neonate,
				 clutch1_size = clutch1,
				 clutch2_size = clutch2,
				 clutch3_size = clutch3) %>% 
	filter(!is.na(clutch1_size), !is.na(clutch2_size), !is.na(clutch3_size), !is.na(neonate_size))


all_20 <- left_join(date_20, size_20, by = c("temperature", "replicate"))


wide20 <- all_20 %>% 
	mutate(clutch1_age = interval(neonate_date, clutch1_date)/ddays(1)) %>%
	mutate(clutch2_age = interval(neonate_date, clutch2_date)/ddays(1)) %>%  
	mutate(clutch3_age = interval(neonate_date, clutch3_date)/ddays(1)) %>%  
	select(-contains("date")) %>% 
	mutate(neonate_age = 1)

age_20 <- wide20 %>% 
	select(temperature, replicate, contains("age")) %>% 
	gather(key = clutch, value = age, 3:6) %>% 
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)


length_20 <- wide20 %>% 
	select(temperature, replicate, contains("size")) %>% 
	gather(key = clutch, value = length, 3:6) %>%
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)

data_20 <- left_join(age_20, length_20, by = c("temperature", "replicate", "clutch"))


data_20 %>% 
	ggplot(aes(x = age, y = length, color = factor(replicate))) + geom_point() +
	geom_line()

# now fit vb --------------------------------------------------------------
vbT <- vbFuns("typical")

data_20_2 <- data_20 %>% 
	filter(replicate != 1) %>% 
	rename(tl = length) %>% 
	filter(!is.na(tl), !is.na(age)) %>% 
	mutate(age = ifelse(age == 0, 1, age))

svTF <- vbStarts(tl~age,data=data_20_2,type="typical")

start_list <- c(Linf = 2377.364, t0 = -4, K = 0.1)
fit_model_20 <- nls(tl~vbT(age,Linf,K,t0),data=data_27_2,start=start_list)

fit <-  coef(fit_model_20)


data_20_2 %>% 
	filter(replicate == 1) %>% 
	ggplot(aes(x = age, y = tl)) + geom_line()

fitted20 <- data_20_2 %>% 
	group_by(replicate) %>% 
	do(tidy(nls(tl~vbT(age,Linf,K,t0),data=.,start=start_list))) %>% 
	mutate(temperature = 20)


# now try with 24 ---------------------------------------------------------

size3 <- size2 %>% 
	select(stage, size_um, temperature, replicate) %>% 
	spread(key = stage, value = size_um) %>% 
	select(temperature, replicate, neonate, everything()) %>% 
	filter(!is.na(clutch1), !is.na(clutch2), !is.na(clutch3))


date_24 <- size2 %>% 
	filter(temperature == 24) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, date_measured, stage) %>%
	spread(key = stage, value = date_measured) %>% 
	rename(neonate_date = neonate,
				 clutch1_date = clutch1,
				 clutch2_date = clutch2,
				 clutch3_date = clutch3) %>% 
	filter(!is.na(clutch1_date), !is.na(clutch2_date), !is.na(clutch3_date), !is.na(neonate_date))


size_24 <- size2 %>% 
	filter(temperature == 24) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, size_um, stage) %>%
	spread(key = stage, value = size_um) %>% 
	rename(neonate_size = neonate,
				 clutch1_size = clutch1,
				 clutch2_size = clutch2,
				 clutch3_size = clutch3) %>% 
	filter(!is.na(clutch1_size), !is.na(clutch2_size), !is.na(clutch3_size), !is.na(neonate_size))


all_24 <- left_join(date_24, size_24, by = c("temperature", "replicate"))


wide24 <- all_24 %>% 
	mutate(clutch1_age = interval(neonate_date, clutch1_date)/ddays(1)) %>%
	mutate(clutch2_age = interval(neonate_date, clutch2_date)/ddays(1)) %>%  
	mutate(clutch3_age = interval(neonate_date, clutch3_date)/ddays(1)) %>%  
	select(-contains("date")) %>% 
	mutate(neonate_age = 1)

age_24 <- wide24 %>% 
	select(temperature, replicate, contains("age")) %>% 
	gather(key = clutch, value = age, 3:6) %>% 
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)


length_24 <- wide24 %>% 
	select(temperature, replicate, contains("size")) %>% 
	gather(key = clutch, value = length, 3:6) %>%
	separate(clutch, into = c("clutch", "word"), sep = "_") %>% 
	select(-word)

data_24 <- left_join(age_24, length_24, by = c("temperature", "replicate", "clutch"))


# now fit vb --------------------------------------------------------------
vbT <- vbFuns("typical")

data_24_2 <- data_24 %>% 
	rename(tl = length) %>% 
	filter(!is.na(tl), !is.na(age)) %>% 
	mutate(age = ifelse(age == 0, 1, age))

svTF <- vbStarts(tl~age,data=data_24_2,type="typical")

start_list <- c(Linf = 2377.364, t0 = -4, K = 0.1)
fit_model_24 <- nls(tl~vbT(age,Linf,K,t0),data=data_27_2,start=start_list)

fit <-  coef(fit_model_24)


data_24_2 %>% 
	filter(replicate == 1) %>% 
	ggplot(aes(x = age, y = tl)) + geom_line()

fitted24 <- data_24_2 %>% 
	group_by(replicate) %>% 
	do(tidy(nls(tl~vbT(age,Linf,K,t0),data=.,start=start_list))) %>% 
	mutate(temperature = 24)


# now bring together the fitted params ------------------------------------

all_fits <- bind_rows(fitted16, fitted27, fitted20, fitted24, fitted10)

all_fits %>% 
	filter(!replicate==1 & !temperature == 20) %>% View

all_fits %>% 
	group_by(term, temperature) %>% 
	summarise_each(funs(mean, std.error), estimate) %>% 
	ggplot(aes(x = temperature, y = estimate_mean, color = factor(temperature))) + geom_point() +
	geom_errorbar(aes(ymin = estimate_mean - estimate_std.error, ymax = estimate_mean + estimate_std.error), width = 0.1) +
	facet_wrap( ~ term, scales = "free")


all_fits %>% 
	filter(term == "K") %>% 
	ggplot(aes(x = temperature, y = estimate)) + geom_point() +
	geom_smooth(method = "lm")

Ks <- all_fits %>% 
	filter(term == "K") %>% 
	rename(K = estimate)

Linfs <- all_fits %>% 
	filter(term == "Linf") %>% 
	rename(Linf = estimate)
## now try to plot max size vs K

all <- left_join(Ks, max_size, by = c("replicate", "temperature"))
all2 <- left_join(Linfs, all, by = c("replicate", "temperature"))

all2 %>% 
	ungroup() %>% 
	mutate(growth_rate = K*24) %>% 
	ggplot(aes( x = log(K), y = log(Linf), color = factor(temperature))) + geom_point() + 
	geom_smooth(method = "lm", color = "blue")

all3 <- all2 %>% 
	ungroup() %>% 
	mutate(growth_rate = K*24) %>% 
	mutate(linf_mass =  0.00402*((Linf/1000)^2.66))

# write_csv(all3, "data-processed/von_bert_mass.csv")



all3 <- read_csv("data-processed/von_bert_mass.csv")

prediction <- function(x) -0.69*x -4.3
trade_off_plot <- all3 %>% 
	mutate(`T (°C)` = as.factor(temperature)) %>% 
	ggplot(aes(x = log(K), y = log(linf_mass), color = `T (°C)`)) + 
	stat_function( fun = prediction, color = "black", linetype = "dashed") +
	geom_point(size = 4) +
	geom_smooth(method = "lm", color = "black") + ylab("log(asymptotic body mass)") + xlab("log(growth constant k, per day)") +
	theme_bw() +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) + scale_color_viridis(discrete = TRUE) +
	# stat_function( fun = prediction, color = "black", linetype = "dashed") +
	annotate("text", label = "Predicted slope = -0.69; CIs (-0.99, -0.53)\n Observed slope = -0.59; CIs (-0.76, -0.46)", x = -2.5, y = -3.6, size = 5)
	
ggsave("figures/winter_trade_off.pdf", width = 8, height = 5)
ggsave("figures/winter_trade_off.png", width = 8, height = 5)


# Residuals ---------------------------------------------------------------


all3 %>% 
	do(tidy(lm(log(linf_mass)~ log(K), data = .), conf.int = TRUE)) %>% View

mod <- lm(log(linf_mass)~ log(K), data = all3)
resids <- augment(mod, data = all3)
write_csv(resids, "data-processed/residuals_size_rate.csv")

## now let's try to maatch up growth rate with metabolic rate




## K vs temperature
all3 %>% 
	ggplot(aes(x = temperature, y = K)) + geom_point() + geom_smooth(method = "lm")


# K vs. temperature -------------------------------------------------------


### K vs temperature
all3 %>% 
	filter(Linf < 4000) %>% 
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	do(tidy(lm(log(K) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View

## ci for slope = -Ea/b: -0.211, -0.92)

all3 %>% 
	filter(Linf < 4000) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	do(tidy(lm(log(linf_mass) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View

prediction_t <- function(x) 0.46*x -21.2
inverse_plot <- all3 %>% 
	filter(Linf < 4000) %>% 
	mutate(`Log(k)` = log(K)) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	ggplot(aes(x = inverse_temp, y = log(linf_mass), color = `Log(k)`)) + geom_point(size = 4) +
	stat_function( fun = prediction_t, color = "black", linetype = "dashed") +
	geom_smooth(method = "lm", color = "black") + 
	theme_bw() +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1)) +
	theme(text = element_text(size=16, family = "Helvetica")) + scale_color_viridis(discrete = TRUE) +
	scale_x_reverse() +
	scale_color_viridis() +
	ylim(-3.6, -2) +
	annotate("text", label = "Predicted slope = -0.46; CIs(-0.21, -0.92)\n Observed slope = -0.30; CIs (-0.46, -0.14)", x = 40, y = -3.5, size = 5) +
	ylab("log (asymptotic body mass)") + xlab("Temperature (1/kT)")
ggsave("figures/mass_vs_inverse_temp.png", width = 5, height = 4)
ggsave("figures/mass_vs_inverse_temp.pdf", width = 5.5, height = 4)
ggsave("figures/mass_vs_inverse_temp_color.png", width = 6, height = 4)
ggsave("figures/mass_vs_inverse_temp_color.pdf", width = 6, height = 4)



# key hypothesis plot -----------------------------------------------------

p <- plot_grid(trade_off_plot, inverse_plot, labels = c("A", "B"), nrow = 2, align = "v")
save_plot("figures/combined_panel_color.png", p,
					ncol = 1, # we're saving a grid plot of 2 columns
					nrow = 2, # and 2 rows
					# each individual subplot should have an aspect ratio of 1.3
					base_aspect_ratio = 1.7)

save_plot("figures/combined_panel_color.pdf", p,
					ncol = 1, # we're saving a grid plot of 2 columns
					nrow = 2, # and 2 rows
					# each individual subplot should have an aspect ratio of 1.3
					base_aspect_ratio = 1.7)


resp.mass <- read_csv("data-raw/resp.mass.csv")

resp <- resp.mass %>% 
	rename(rate = mean) %>% 
	mutate(measurement = "Mass-normalized metablic rate")

Ks <- all3 %>% 
	filter(Linf < 4000) %>% 
	select(temperature, K, replicate) %>% 
	mutate(measurement = "Growth constant (k)") %>% 
	mutate(rate = K)

responses <- bind_rows(resp, Ks) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) 

responses %>% 
	group_by(measurement) %>% 
	do(tidy(lm(log(rate) ~ inverse_temp, data =.), conf.int = TRUE)) %>% 
	filter(term != "(Intercept)") %>% View
	ggplot(aes(x = measurement, y = estimate)) + geom_point(size = 3) +
geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) +
	theme(strip.background = element_rect(colour="white", fill="white")) +
	ylab("Activation energy (eV)") + xlab("")
ggsave("figures/activation_energies.png", width = 6, height = 3)
ggsave("figures/activation_energies.pdf", width = 6, height = 3)	

### figure of growth constant and metabolic rate!

responses %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	ggplot(aes(x = inverse_temp, y = log(rate))) + geom_point(size = 2, alpha = 0.7) +
	facet_wrap( ~ measurement, scales = "free_y") + 
	scale_x_reverse() +
	geom_smooth(method = "lm", color = "black")+
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) +
	theme(strip.background = element_rect(colour="white", fill="white")) + xlab("Temperature (1/kT)") +
	ylab("Log (rate of demand)")
ggsave("figures/metabolic_rate_growth_rate_kT.png", width = 6.5, height = 3)
ggsave("figures/metabolic_rate_growth_rate_kT.pdf", width = 6.5, height = 3)
ggsave("figures/metabolic_rate_growth_rate.png", width = 6.5, height = 3)
ggsave("figures/metabolic_rate_growth_rate.pdf", width = 6.5, height = 3)

all3 %>% 
	filter(Linf < 4000) %>% 
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	do(tidy(lm(log(linf_mass) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View



all3 %>% 
	filter(Linf < 4000) %>% 
	lm(log(linf_mass) ~ log(K), data = .) %>% 
	summary()

### what's the slope of the size-rate tradeoff?
rma <- lmodel2(log(linf_mass) ~ log(K), data = all3, range.y = "interval", range.x = "interval")
rma$regression.results
rma$confidence.intervals
rma$rsquare
augment(rma)

model_results <- size2 %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage == "clutch3") %>%
	lm(actual_size_um ~ temperature, data = .) %>% 
	tidy

# write_csv(model_results, "data-processed/model_results.csv")



met_rate <- all3 %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	mutate(metabolic_rate = linf_mass*exp(0.88/(.00008617*inverse_temp))) 

met_rate %>%
	ggplot(aes(y = log(linf_mass), x = log(metabolic_rate), color = temperature)) + geom_point() + geom_smooth(method = "lm")

rma2 <- lmodel2(log(linf_mass) ~ log(metabolic_rate), data = met_rate, range.y = "interval", range.x = "interval")
rma2$regression.results
rma2$confidence.intervals
rma2$rsquare


# now onto somatic growth rate --------------------------------------------

growth10 <- wide10 %>% 
	mutate(somatic_growth_rate = (clutch1_size - neonate_size)/clutch1_age)

growth16 <- wide16 %>% 
	mutate(somatic_growth_rate = (clutch1_size - neonate_size)/clutch1_age)

growth20 <- wide20 %>% 
	mutate(somatic_growth_rate = (clutch1_size - neonate_size)/clutch1_age)

growth24 <- wide24 %>% 
	mutate(somatic_growth_rate = (clutch1_size - neonate_size)/clutch1_age)

growth27 <- wide27 %>% 
	mutate(somatic_growth_rate = (clutch1_size - neonate_size)/clutch1_age)

all_growth <- bind_rows(growth27, growth24, growth20, growth16, growth10)

# write_csv(all_growth, "data-processed/all_growth.csv")

somatic_growth_plot <- all_growth %>% 
	mutate(growth_mass =  0.00402*((somatic_growth_rate/1000)^2.66)) %>% 
	ggplot(aes(x = temperature, y = growth_mass)) + geom_point(size = 2) +
	geom_smooth(method = "lm", color = "black") + 
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) +  xlab("Temperature (°C)") +
	ylab("Growth rate (mg DW/day)")
# ggsave("figures/somatic_growth_per_day_v_temperature.pdf")
# ggsave("figures/somatic_growth_per_day_v_temperature.png")

## growth rate and generation time plot (generation times plot comes from 08_reproductive output.R, approx line 221)

library(cowplot)
p <- plot_grid(somatic_growth_plot, generation_times_plot, labels = c("A", "B"), align = "v", nrow = 2, ncol  =1)
save_plot("figures/growth_gen_time_panel.png", p, base_height = 7, base_width = 5)
save_plot("figures/growth_gen_time_panel.pdf", p, base_height = 7, base_width = 5)

all_growth %>% 
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	do(tidy(lm(log(somatic_growth_rate) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View
