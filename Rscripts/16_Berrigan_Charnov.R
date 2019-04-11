library(tidyverse)
library(cowplot)

### Berrigan and Charnov model

## first try to recreate the figures in Berrian and Charnov

size_function <- function(x, A, k){
	res <- A*(1-B*exp(-k*x))
	return(res)
}

f <- 212
A <- 4.58
B <- 0.934
j <- 0.1
H <- 358
k <- 0.3
k <- 0.5
k <- 0.7

xes <- seq(0, 20, by = .1)
sizes <- sapply(xes, size_function)
size_data <- data.frame(ages = xes, size = sizes)


sizes2 <- sapply(xes, size_function)
size_data2 <- data.frame(ages = xes, size = sizes2)


sizes3 <- sapply(xes, size_function)
size_data3 <- data.frame(ages = xes, size = sizes3)


size_data %>% 
	ggplot(aes(x = ages, y = size)) + geom_line() +
	geom_line(aes(x = ages, y = size), data = size_data2, color = "red") +
	geom_line(aes(x = ages, y = size), data = size_data3, color = "blue") +
	geom_point(aes(x = a_star_black, y = size_star_black), color = "black") +
	geom_point(aes(x = a_star_red, y = size_star_red), color = "red") +
	geom_point(aes(x = a_star_blue, y = size_star_blue), color = "blue")
	


a_star_black <- (log((f*A*B*(0.3 + j))/ ((f*A-H)*j))) /0.3
a_star_red <- (log((f*A*B*(0.5 + j))/ ((f*A-H)*j))) /0.5
a_star_blue <- (log((f*A*B*(0.7 + j))/ ((f*A-H)*j))) /0.7

size_star_black <- size_function(a_star_black, k = 0.3)
size_star_red <- size_function(a_star_red, k = 0.5)
size_star_blue <- size_function(a_star_blue, k = 0.7)

### with our data
vb_params_acute <- read_csv("data-processed/von_bert_mass.csv") %>% 
	mutate(experiment = "acute")
vb_params_acclimated <- read_csv("data-processed/acc_daph_vb_params_till_clutch4.csv") %>% 
	filter(term %in% c("Linf", "K")) %>% 
	select(unique_id, term, estimate) %>% 
	spread(key = term, value = estimate) %>% 
	separate(unique_id, into = c("temperature", "replicate")) %>% 
	mutate(experiment = "acclimated") %>%
	mutate(temperature = as.numeric(temperature))


vb_params %>% 
	ggplot(aes(x = temperature, y = K)) + geom_point()


### ok plot these curves with variable As and Ks

size_function <- function(x, A, k){
	res <- A*(1-B*exp(-k*x))
	return(res)
}


f <- 21
B <- 1
j <- 0.1
H <- 35



vb2 <- vb_params %>% 
	rename(A = Linf,
				 k = K) %>% 
	select(temperature, replicate, A, k) %>% 
	mutate(a_star = (log((f*A*B*(k + j))/ ((f*A-H)*j))) /k) %>% 
	mutate(s_star = size_function(a_star, A, k))


## predicted optimal size ignoring temperature dependent fecundity effects
vb2 %>% 
	ggplot(aes(x = temperature, y = s_star)) + geom_point()

vb_params %>% 
	lm(log(Linf) ~ log(K), data = .) %>% summary()


library(lmodel2)
library(modelr)
rma <- lmodel2(log(linf_mass) ~ log(K), data = params3, range.y = "interval", range.x = "interval")
rma$regression.results
rma$confidence.intervals
rma$rsquare
tidy(rma)

### find out empiricially what the f's and H's are

fecundities <- read_csv("data-processed/all_fecundities_acclimated.csv")


fecundities %>% 
	ggplot(aes(x = Length, y = egg_number)) + geom_point()

fecundities %>% 
	# filter(clutch == 1) %>% 
	ggplot(aes(x = length_mm, y = egg_number)) + geom_point()


fh <- lm(egg_number ~ length_mm, data = fecundities) %>% 
	tidy()


f_f <- fh$estimate[fh$term == "length_mm"][[1]]
H_f <- fh$estimate[fh$term == "(Intercept)"][[1]] * -1

f <- 2100
B <- 1
j <- 0.1
H <- 35

juvenile_mortality_temps <- fecundities %>% 
	mutate(juvenile_mortality = (initial_baby_count - final_baby_count)/initial_baby_count)%>% 
	filter(!is.na(juvenile_mortality)) %>% 
	group_by(temperature) %>% 
	summarise_each(funs(mean, std.error), juvenile_mortality) %>% 
	mutate(temperature = as.character(temperature)) %>% 
	rename(j = mean) %>% 
	select(temperature, j)


vb_params <- read_csv("data-processed/acc_daph_vb_params_till_clutch4.csv") %>% 
	filter(term %in% c("Linf", "K")) %>% 
	select(unique_id, term, estimate) %>% 
	spread(key = term, value = estimate) %>% 
	separate(unique_id, into = c("temperature", "replicate")) %>% 
	left_join(juvenile_mortality_temps, by = "temperature")


vb_fitted <- vb_params %>% 
	rename(A = Linf,
				 k = K) %>% 
	select(temperature, replicate, A, k) %>% 
	mutate(a_star = (log((f_f*A*B*(k + j))/ ((f_f*A-H_f)*j))) /k) %>% 
	mutate(s_star = size_function(a_star, A, k)) %>% 
	mutate(type = "fitted") %>% 
	mutate(temperature = as.numeric(temperature))

vb_fixed <- vb_params %>% 
	rename(A = Linf,
				 k = K) %>% 
	select(temperature, replicate, A, k) %>% 
	mutate(a_star = (log((f*A*B*(k + j))/ ((f*A-H)*j))) /k) %>% 
	mutate(s_star = size_function(a_star, A, k)) %>% 
	mutate(s_star_direct = A - (((f*A - H)*j)/(f*(k + j)))) %>% 
	mutate(type = "fixed")

all_vb <- left_join(vb_fitted, vb_fixed, by = c("temperature", "replicate"))


## predicted optimal size ignoring temperature dependent fecundity effects
with_fitted <- vb_fixed %>% 
	ggplot(aes(x = temperature, y = s_star)) + geom_point()

with_out_fitted <- vb_fitted %>% 
	ggplot(aes(x = temperature, y = s_star)) + geom_point() +
	ylab("Body length at maturity") + xlab("Temperature (°C)") +
	geom_smooth(method = "lm", color = "black") 
ggsave("figures/optimal-body-size-berrigan-charnov-acute.png", width = 8, height = 6)

vb_params %>% 
	lm(log(Linf) ~ log(K), data = .) %>% summary()

### find out if this matches the size at maturity data from the acclimated experiment!

acc <- read_csv("data-raw/acclimated-daphnia-body-size.csv") %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	clean_names() %>% 
	filter(stage == "clutch1") %>% 
	mutate(temperature = as.numeric(temperature))

str(vb_fitted)

acc2 <- left_join(acc, vb_fitted, by = c("temperature", "replicate"))


vb_fitted %>% 
	ggplot(aes(x = temperature, y = s_star)) + geom_point() +
	# geom_point(aes(x = temperature, y = size_um), data = acc, color = "purple") +
	ylab("Body length at maturity") + xlab("Temperature (°C)") +
	geom_smooth(method = "lm", color = "black") +
	# geom_smooth(aes(x = temperature, y = size_um), data = acc,
							# color = "purple", method = "lm") 
ggsave("figures/optimal-body-size-berrigan-charnov.png", width = 8, height = 6)


## compare the Linfs from the acclimated and acute

vb_params_acclimated %>% 
	ggplot(aes(x = temperature, y = Linf)) + geom_point(color = "blue") +
	geom_point(aes(x = temperature, y = Linf), data = vb_params_acute, color = "red") + 
	geom_smooth(method = "lm", color = "blue") + geom_smooth(aes(x = temperature, y = Linf), data = vb_params_acute, color = "red", method = "lm")

vb_acc <- vb_params_acclimated %>% 
	select(replicate, temperature, Linf, K) %>% 
	mutate(replicate = as.numeric(replicate))

vb_ac <- vb_params_acute %>% 
	select(replicate, temperature, Linf, K)

all_vb <- left_join(vb_acc, vb_ac, by = c("temperature", "replicate"))


#### bring in the body sizes from the acute experiment

size <- read_csv("data-raw/daph_tsr_body_size.csv")

size_acute <- clean_names(size) %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	filter(actual_size_um > 0) %>% 
	mutate(size_um = actual_size_um) %>% 
	filter(!is.na(size_um)) %>% 
	select(temperature, replicate, stage, size_um) %>% 
	filter(stage == "clutch1") %>% 
	mutate(experiment = "acute") %>% 
	mutate(replicate = as.numeric(replicate))

size_acclim <- acc %>% 
	select(temperature, replicate, size_um) %>% 
	mutate(experiment = "acclimated")%>% 
	mutate(replicate = as.numeric(replicate))

vb_fitted_acclimated <- vb_fitted %>% 
	rename(size_um = s_star) %>% 
	mutate(experiment = "optimal size prediction")%>% 
	mutate(replicate = as.numeric(replicate))

all_sizes_maturity <- bind_rows(size_acclim, size_acute, vb_fitted_acclimated)


all_sizes_maturity %>% 
	ggplot(aes(x = temperature, y = size_um, color = experiment)) + geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
	ylab("Size at maturity (um)") + xlab("Temperature (°C)") +
	scale_color_viridis_d(end = 0.7)
ggsave("figures/size_at_maturity_predictions.png", width = 8, height = 6)



ggplot() +
	geom_point(aes(x = temperature, y = size_um), data = size_acclim, color = "purple") +
	geom_point(aes(x = temperature, y = size_um), data = size_acute, color = "blue") +
	geom_smooth(aes(x = temperature, y = size_um), data = size_acclim, color = "purple", method = "lm") +
	geom_smooth(aes(x = temperature, y = size_um), data = size_acute, color = "blue", method = "lm") +
	geom_point(aes(x = temperature, y = s_star), data = vb_fitted, color = "orange") +
	ylab("Size at maturity (um)") + xlab("Temperature (°C)")
	
