

library(tidyverse)
library(here)
library(cowplot)
library(broom)


daph_files <- list.files(path= "data-raw/daphnia-clutches-imagej/done", pattern="*.csv", recursive = TRUE, full.names = TRUE)

names(daph_files) <- daph_files %>% 
	gsub(pattern = ".csv$", replacement = "")


all_daph <- map_df(daph_files, read_csv, .id = "file_name") %>% 
	separate(col = file_name, into = c("path", "photo_id"), sep = "imagej/") %>% 
	mutate(photo_id = str_replace(photo_id, "-size", ""))


lengths <- all_daph %>% 
	filter(!is.na(Length)) %>% 
	select(photo_id, Length)

clutch_numbers <- all_daph %>% 
	filter(is.na(Length)) %>% 
	group_by(photo_id) %>% 
	tally()
	

all_clutches <- left_join(clutch_numbers, lengths) %>% 
	separate(photo_id, into= c("path2", "temperature", "replicate", "clutch", sep = "-")) %>% 
	rename(date = temperature, 
				 temperature = replicate,
				 replicate = clutch, 
				 clutch = `-`) %>% 
	mutate(clutch = str_replace(clutch, "clutch", "")) %>% 
	mutate(clutch_number = as.numeric(clutch) + 1) %>% 
	mutate(length_mm = Length/1.593) %>% 
	mutate(unique_id = paste(temperature, replicate, sep = "_")) %>% 
	mutate(temperature = as.numeric(temperature)) %>% 
	mutate(replicate = as.numeric(replicate)) %>% 
	filter(!grepl("apr", date))  # to just get the acclimated experiment version

### scale is 1.593 pixels = 1mm

length(unique(all_clutches$unique_id))

all_clutches %>% 
	filter(!grepl("apr", date)) %>% 
	ggplot(aes(x = length_mm, y = n)) + geom_point()+
	geom_smooth(method = "lm", color = "black") +
	ylab("Number of eggs") + xlab("Body length (mm)")
ggsave("figures/eggs_v_length.png", width = 4.5, height = 4)

all_clutches %>%
	group_by(temperature) %>% 
	do(tidy(lm(n ~ Length, data = .), conf.int = TRUE)) %>% View


all_clutches %>%
	filter(!grepl("apr", date)) %>% 
	lm(n ~ Length, data = .) %>% summary()

library(lme4)
library(MuMIn)

mm <- all_clutches %>%
	filter(!grepl("apr", date)) %>% 
	lmer(n ~ Length + (1|clutch_number) + (1|unique_id), data = .,  REML=FALSE)

summary(mm)
r.squaredGLMM(mm)
anova(mm)
coef(mm)

fecundity <- read_csv("data-processed/acclimated-fecundity.csv") 


all_fecund <- left_join(fecundity, all_clutches)


all_fecund %>% 
	ggplot(aes(x = length_mm, y = initial_baby_count)) + geom_point()

all_fecund %>% 
	ggplot(aes(x = n, y = final_baby_count)) + geom_point() +
	geom_abline(yintercept = 0, slope = 1)

all_fecund %>% 
	ggplot(aes(x = final_baby_count, y = initial_baby_count)) + geom_point()


all_fecund %>%
	rename(egg_number = n) %>% 
	gather(key = fecundity_stage, value = fecundity, egg_number, final_baby_count) %>% 
	mutate(fecundity_stage = ifelse(fecundity_stage == "egg_number", "Number of eggs per clutch", "Number of surviving neonates")) %>% 
	ggplot(aes(x = length_mm, y = fecundity, color = fecundity_stage)) + geom_point() +
	geom_smooth(method = "lm") +
	facet_wrap( ~ fecundity_stage) + xlab("Body length (mm)") + scale_color_discrete(name = "Fecundity metric")
ggsave("figures/fecundities-acclimated.png", width = 12, height = 6)


all_fecund %>%
	rename(egg_number = n) %>%
	ggplot(aes(x = length_mm, y = egg_number)) + geom_point() + geom_smooth(method = "lm")


all_fecund %>%
	rename(egg_number = n) %>%
	ggplot(aes(x = length_mm, y = egg_number)) + geom_point() + geom_smooth(method = "lm")

all_fecund %>%
	rename(egg_number = n) %>%
	gather(key = fecundity_stage, value = fecundity, egg_number, initial_baby_count) %>% 
	group_by(fecundity_stage) %>% 
	do(tidy(lm(fecundity ~ length_mm, data = .), conf.int = TRUE)) %>% View


### does body size increase with development time, like in Fig 1 in Sibly and Atkinson 1994?
### does development time increase with fecundity? Fig 3 ?

### it is necessary that h < (lmin/(linf - 1))/1 + k/juvenile mortality rate

### let's get juvenile mortality rate
library(plotrix)
library(janitor)


juvenile_mortality <- all_fecund %>% 
	mutate(juvenile_mortality = (initial_baby_count - final_baby_count)/initial_baby_count)%>% 
	filter(!is.na(juvenile_mortality)) %>% 
	summarise_each(funs(mean, std.error), juvenile_mortality) 


body_sizes <- read_csv("data-raw/acclimated-daphnia-body-size.csv") %>% 
	clean_names()

neonate_size <- body_sizes %>% 
	filter(stage == "neonate") %>% 
	summarise_each(funs(mean, std.error), size_um)

vb_params <- read_csv("data-processed/acc_daph_vb_params_till_clutch4.csv") %>% 
	clean_names() %>% 
	separate(unique_id, into = c("temperature", "replicate"), sep = "_")

vb_sum <- vb_params %>% 
	group_by(term) %>% 
	summarise_each(funs(mean, std.error), estimate)

numerator <- (743.2104/(2713.5934614-1))

denominator <- ((1+0.1052055)/0.0860794)

numerator/denominator	


### does juvenile mortality rate vary with temperature?

juvenile_mortality_temps <- all_fecund %>% 
	mutate(juvenile_mortality = (initial_baby_count - final_baby_count)/initial_baby_count)%>% 
	filter(!is.na(juvenile_mortality)) %>% 
	group_by(temperature) %>% 
	summarise_each(funs(mean, std.error), juvenile_mortality) 


juvenile_mortality_temps %>% 
	ggplot(aes(x = temperature, y = mean)) + geom_point() +
	geom_smooth(method = "lm") +
	geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error)) + ylab("Juvenile mortality rate") +
	xlab("Temperature (°C)")


### optimal body size 
s_star <- A - (((f*A - H)*j)/(f*(k + j)))

## size at maturity must be greater than H/f

H <- 3000
f <- 2
H/f


s <- body_sizes %>% 
	filter(stage == "clutch1") %>% 
	filter(!is.na(size_um))

optimal_size_function <- function(f, A, H, j, k){
	s_star <- A - (((f*A - H)*j)/(f*(k + j)))
	return(s_star)
}
	

A_20 <- vb_params %>% 
	filter(term == "Linf", temperature == 20) %>% 
	summarise(A = mean(estimate))

k_20 <- vb_params %>% 
	filter(term == "K", temperature == 20) %>% 
	summarise(A = mean(estimate))

j_20 <- juvenile_mortality_temps %>% 
	filter(temperature == 20) %>% 
	select(mean)

j = juvenile_mortality$mean[[1]]

s_20 <- optimal_size_function(H = H, f = f, A = A_20[[1]], k = k_20[[1]], j = j)


#### 27
A_27 <- vb_params %>% 
	filter(term == "Linf", temperature == 27) %>% 
	summarise(A = mean(estimate))

k_27 <- vb_params %>% 
	filter(term == "K", temperature == 27) %>% 
	summarise(A = mean(estimate))

j_27 <- juvenile_mortality_temps %>% 
	filter(temperature == 27) %>% 
	select(mean)

s_27 <- optimal_size_function(H = H, f = f, A = A_27[[1]], k = k_27[[1]], j = j)

#### 16
A_16 <- vb_params %>% 
	filter(term == "Linf", temperature == 16) %>% 
	summarise(A = mean(estimate))

k_16 <- vb_params %>% 
	filter(term == "K", temperature == 16) %>% 
	summarise(A = mean(estimate))

j_16 <- juvenile_mortality_temps %>% 
	filter(temperature == 16) %>% 
	select(mean)

s_16 <- optimal_size_function(H = H, f = f, A = A_16[[1]], k = k_16[[1]], j = j)

#### 24
A_24 <- vb_params %>% 
	filter(term == "Linf", temperature == 24) %>% 
	summarise(A = mean(estimate))

k_24 <- vb_params %>% 
	filter(term == "K", temperature == 24) %>% 
	summarise(A = mean(estimate))

j_24 <- juvenile_mortality_temps %>% 
	filter(temperature == 24) %>% 
	select(mean)

s_24 <- optimal_size_function(H = H, f = f, A = A_24[[1]], k = k_24[[1]], j = j)


#### 10
A_10 <- vb_params %>% 
	filter(term == "Linf", temperature == 10) %>% 
	summarise(A = mean(estimate))

k_10 <- vb_params %>% 
	filter(term == "K", temperature == 10) %>% 
	summarise(A = mean(estimate))

j_10 <- juvenile_mortality_temps %>% 
	filter(temperature == 10) %>% 
	select(mean)

s_10 <- optimal_size_function(H = H, f = f, A = A_10[[1]], k = k_10[[1]], j = j)


all_s <- c(s_10, s_16, s_20, s_24, s_27)
temps <- c(10, 16, 20, 24, 27)

all_s_temp <- data.frame(s = all_s, temperature = temps)

all_s_temp %>% 
	ggplot(aes(x = temperature, y = s)) + geom_point() +
	geom_point(aes(x = temperature, y = size_um), data= s, color ="red")


vb_params %>% 
	ggplot(aes(x = temperature, y = estimate)) + geom_point() +
	facet_wrap( ~ term, scales = "free")

juvenile_mortality_temps %>% 
	ggplot(aes(x = temperature, y = mean)) + geom_point()
