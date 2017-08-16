

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


# read in data ------------------------------------------------------------

size <- read_csv("data-raw/daph_tsr_body_size.csv")

size2 <- clean_names(size) %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	filter(actual_size_um > 0) %>% 
	mutate(size_um = actual_size_um) %>% 
	filter(!is.na(size_um))


size2 %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage == "clutch3") %>% 
	ggplot(aes(x = temperature, y = actual_size_um)) + geom_point() +
	geom_smooth(method = "lm") + theme_bw() + ylab("Body length") + xlab("Temperature")

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
	mutate(linf_mass =  0.00402*((Linf)^2.66))

write_csv(all3, "data-processed/von_bert_mass.csv")
all3 %>% 
	mutate(temperature = as.factor(temperature)) %>% 
	ggplot(aes(x = log(K), y = log(linf_mass), color = temperature)) + geom_point(size = 4) +
	geom_smooth(method = "lm", color = "black") + theme_bw() + ylab("log(asymptotic body mass)") + xlab("log (K)") +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica"))
ggsave("figures/winter_trade_off.pdf")
ggsave("figures/winter_trade_off.png")


## K vs temperature
all3 %>% 
	ggplot(aes(x = temperature, y = K)) + geom_point() + geom_smooth(method = "lm")

### K vs temperature
all3 %>% 
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	do(tidy(lm(K ~ inverse_temp, data = .), conf.int = TRUE)) %>% View


all3 %>% 
	filter(Linf < 4000) %>% 
	ggplot(aes(x = temperature, y = linf_mass, color = factor(replicate))) + geom_point() +
	geom_smooth(method = "lm", color = "black")


rma <- lmodel2(log(linf_mass) ~ log(K), data = all3, range.y = "interval", range.x = "interval")
rma$regression.results
rma$confidence.intervals
rma$rsquare


model_results <- size2 %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage == "clutch3") %>%
	lm(actual_size_um ~ temperature, data = .) %>% 
	tidy

write_csv(model_results, "data-processed/model_results.csv")



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


all_growth %>% 
	ggplot(aes(x = temperature, y = log(somatic_growth_rate))) + geom_point() +
	geom_smooth(method = "lm")

all_growth %>% 
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	do(tidy(lm(log(somatic_growth_rate) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View
