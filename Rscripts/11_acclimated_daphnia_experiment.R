

### acclimated experiment

library(cowplot)
library(tidyverse)
library(lubridate)
library(janitor)
library(viridis)
library(minpack.lm)
library(nls.multstart)
library(nlstools)
library(FSA)
library(broom)

acc <- read_csv("data-raw/acclimated-daphnia-body-size.csv") %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	clean_names()


acc %>% 
	ggplot(aes(x = date_measured, y = size_um, color = replicate, group = replicate)) + geom_point() +
	facet_wrap( ~ temperature) + scale_color_viridis() + geom_line() +
	ylab("Size (um)") + xlab("Date")
ggsave("figures/acclimated_daphnia_sizes.png", width = 7, height = 5)

acc %>% 
	group_by(temperature, replicate) %>% 
	summarise_each(funs(max), size_um) %>% 
	ggplot(aes(x = temperature, y = size_um)) + geom_point()

acc %>%
	filter(stage == "neonate") %>% View

## figure of size at stage
acc %>% 
	mutate(mass =  0.00402*((size_um/1000)^2.66)) %>% 
	filter(stage %in% c("clutch1","clutch2", "clutch3", "clutch4", "clutch5", "clutch6")) %>% 
	ggplot(aes(x = temperature, y = mass)) + geom_point() + 
	geom_smooth(method = "lm", color = "black") +
	facet_wrap( ~ stage) + ylab("Mass (mg)") + xlab("Temperature (°C)")

ggsave("figures/acclimated_daphnia_clutches_time.pdf", width = 7, height = 5)

acc %>% 
	mutate(mass =  0.00402*((size_um/1000)^2.66)) %>% 
	filter(stage %in% c("clutch1","clutch2", "clutch3", "clutch4", "clutch5", "clutch6")) %>% 
	group_by(stage) %>% 
	do(tidy(lm(mass ~ temperature, data = .), conf.int = TRUE)) %>%  View


## make an age column

ac_age <- acc %>% 
	group_by(replicate, temperature) %>% 
	mutate(birth_date = min(date_measured)) %>% 
	mutate(age = (date_measured - birth_date)/ddays(1)) %>% 
	filter(!is.na(date_measured)) %>% 
	filter(!is.na(age)) %>% 
	unite(unique_id, temperature, replicate, remove = FALSE)

### ok take out the lines where daphnia are declining in size?
ac_age %>% 
	filter(temperature == 27, replicate == 8) %>% View
	ggplot(aes(x = age, y = size_um, color = factor(replicate), group = replicate)) + geom_point() +
	facet_wrap( ~ temperature) +  geom_line() +
	ylab("Size (um)") + xlab("Date")

ac_age2 <- ac_age %>% 
	mutate(keep = NA) %>% 
	# filter(temperature == 16) %>% 
	mutate(keep = case_when(temperature == 16 & replicate == 2 & date_measured < ymd("2017-08-27") ~ "yes",
													temperature == 16 & replicate == 2 & date_measured > ymd("2017-08-27") ~ "no",
													temperature == 16 & replicate == 3 & date_measured < ymd("2017-08-31") ~ "yes",
													temperature == 16 & replicate == 3 & date_measured > ymd("2017-08-31") ~ "no",
													temperature == 16 & replicate == 7 & date_measured < ymd("2017-09-04") ~ "yes",
													temperature == 16 & replicate == 7 & date_measured > ymd("2017-09-04")~ "no",
													temperature == 16 & replicate == 8 & date_measured < ymd("2017-08-30") ~ "yes",
													temperature == 16 & replicate == 8 & date_measured > ymd("2017-08-30") ~ "no",
													temperature == 16 & replicate == 5 & date_measured < ymd("2017-09-30") ~ "yes",
													temperature == 16 & replicate == 5 & date_measured > ymd("2017-09-30") ~ "no",
													temperature == 16 & replicate == 6 & date_measured < ymd("2017-09-30") ~ "yes",
													temperature == 16 & replicate == 6 & date_measured > ymd("2017-09-30") ~ "no",
													temperature == 24 & replicate == 2 & date_measured < ymd("2017-08-15") ~ "yes",
													temperature == 24 & replicate == 2 & date_measured > ymd("2017-08-15") ~ "no",
													temperature == 24 & replicate == 3 & date_measured < ymd("2017-08-16") ~ "yes",
													temperature == 24 & replicate == 3 & date_measured > ymd("2017-08-16") ~ "no",
													temperature == 24 & replicate == 4 & date_measured < ymd("2017-08-07") ~ "yes",
													temperature == 24 & replicate == 4 & date_measured > ymd("2017-08-07") ~ "no",
													temperature == 24 & replicate == 7 & date_measured < ymd("2017-09-04") ~ "yes",
													temperature == 24 & replicate == 7 & date_measured > ymd("2017-09-04") ~ "no",
													temperature == 24 & replicate == 8 & date_measured < ymd("2017-09-04") ~ "yes",
													temperature == 24 & replicate == 8 & date_measured > ymd("2017-09-04") ~ "no",
													temperature == 27 & replicate == 4 & date_measured < ymd("2017-08-11") ~ "yes",
													temperature == 27 & replicate == 4 & date_measured > ymd("2017-08-11") ~ "no",
													temperature == 27 & replicate == 6 & date_measured < ymd("2017-09-06") ~ "yes",
													temperature == 27 & replicate == 6 & date_measured > ymd("2017-09-06") ~ "no",
													temperature == 24 & replicate == 7 & date_measured < ymd("2017-08-08") ~ "yes",
													temperature == 24 & replicate == 7 & date_measured > ymd("2017-08-08") ~ "no")) %>% 
	# filter(temperature == 16) %>%
	filter(keep %in% c(NA, "yes"))

ac_age2 %>% 
	# filter(stage %in% c("clutch1","clutch2", "clutch3", "clutch4", "clutch5", "clutch6")) %>% 
	ggplot(aes(x = date_measured, y = size_um, color = replicate, group = replicate)) + geom_point() +
	facet_wrap( ~ temperature) + scale_color_viridis() + geom_line() +
	ylab("Size (um)") + xlab("Date")

## now get asymptotic mass

?vbFuns
vbT <- vbFuns("typical")
start_list <- c(Linf = 2377.364, t0 = -4, K = 0.1)
svTF <- vbStarts(size_um ~ age, data = ac_age, type = "typical")


ac_split <- ac_age %>% 
	split(.$unique_id)


d_1 <- subset(ac_age, unique_id == "10_1")

# run nls_multstart
fit <- nls_multstart(size_um ~ vbT(age, Linf, K, t0),
										 data = d_1,
										 iter = 500,
										 start_lower = c(Linf = 500, K = 0, t0 = -5),
										 start_upper = c(Linf = 1000, K = 0.5, t0 = 0),
										 supp_errors = 'Y',
										 na.action = na.omit,
										 lower = c(Linf = 100, K = 0, t0 = -15))

fit


ac_age3 <- ac_age2 %>% 
	filter(stage %in% c("neonate", "clutch1", "clutch2", "clutch3", "clutch4"))


fits <- ac_age3 %>% 
	group_by(unique_id) %>%
	nest() %>% 
	mutate(fit = purrr:::map(data, ~ nls_multstart(size_um ~ vbT(age, Linf, K, t0),
																							 data = .x,
																							 iter = 500,
																							 start_lower = c(Linf = 500, K = 0, t0 = -15),
																							 start_upper = c(Linf = 3000, K = 1, t0 = 0),
																							 supp_errors = 'Y',
																							 na.action = na.omit,
																							 lower = c(Linf = 100, K = 0, t0 = -15))))

info <- fits %>%
	unnest(fit %>% map(glance))

# get params
params <- fits %>%
	unnest(fit %>% map(tidy))

CI <- fits %>% 
	unnest(fit %>% map(~ confint2(.x) %>%
										 	data.frame() %>%
										 	rename(., conf.low = X2.5.., conf.high = X97.5..))) %>%
	group_by(., unique_id) %>% 
	mutate(., term = c('Linf', 'K', 't0')) %>%
	ungroup()


params <- merge(params, CI, by = intersect(names(params), names(CI)))
params_clutch4 <- merge(params, CI, by = intersect(names(params), names(CI)))
write_csv(params_clutch4, "data-processed/acc_daph_vb_params_till_clutch4.csv")
write_csv(params, "data-processed/acc_daph_vb_params.csv")

params <- read_csv("data-processed/acc_daph_vb_params.csv")

# get predictions
preds <- fits %>%
	unnest(fit %>% map(augment))

select(info, unique_id, logLik, AIC, BIC, deviance, df.residual) %>% View

new_preds <- ac_age2 %>%
	group_by(temperature, replicate, unique_id) %>% 
	do(., data.frame(age = seq(min(.$age), max(.$age), length.out = 100), stringsAsFactors = FALSE)) %>% 
	ungroup()

max_min <- group_by(ac_age2, temperature, replicate, unique_id) %>%
	summarise(., min_age = min(age), max_age = max(age)) %>%
	ungroup() 

str(max_min)

# create new predictions
preds2 <- fits %>%
	unnest(fit %>% map(augment, newdata = new_preds)) %>% 
	filter(unique_id == unique_id1) %>% 
	merge(., max_min, by = 'unique_id') %>% 
	group_by(unique_id) %>% 
	filter(age > unique(min_age) & age < unique(max_age)) %>%
	rename(size_um = .fitted) %>%
	ungroup()

?augment

ggplot() +
	geom_point(aes(x = age, y = size_um, group = unique_id, color = factor(unique_id)),
						 size = 2, data = ac_age) +
	geom_line(aes(x= age, y = size_um, group = factor(unique_id1), color = factor(unique_id)), data = preds2) +
	# facet_wrap(~ temperature + replicate, labeller = labeller(.multi_line = FALSE)) +
	# scale_colour_manual(values = c('green4', 'black')) +
	theme_bw(base_size = 12, base_family = 'Helvetica') +
	ylab('Size') +
	xlab('Age') +
	theme(legend.position = c(0.9, 0.15)) 
ggsave("figures/acc_vb_fits.pdf", width = 20, height = 20)

preds2 %>% 
	filter(unique_id == "10_1") %>% 
	ungroup() %>% 
	ggplot(aes(x = age, y = size_um, group = unique_id)) + geom_point()


ggplot(params) +
	geom_point(aes(unique_id, estimate)) +
	facet_wrap(~ term, scale = 'free_x', ncol = 4) +
	geom_linerange(aes(unique_id, ymin = conf.low, ymax = conf.high)) +
	coord_flip() +
	scale_color_manual(values = c('green4', 'black')) +
	theme_bw(base_size = 12, base_family = 'Helvetica') +
	theme(legend.position = 'top') +
	xlab('curve') +
	ylab('parameter estimate')

params %>% 
	separate(unique_id, into = c("temperature", "replicate"), remove = FALSE) %>% 
	mutate(temperature = as.numeric(temperature)) %>% 
	ggplot(aes(x = temperature, y = estimate)) + geom_point() +
	facet_wrap(~ term, scales = "free")


### Figure of asymptotic mass vs temperature
params %>% 
	separate(unique_id, into = c("temperature", "replicate"), remove = FALSE) %>% 
	mutate(temperature = as.numeric(temperature)) %>% 
	filter(term == "Linf") %>% 
	mutate(mass =  0.00402*((estimate/1000)^2.66)) %>% 
	ggplot(aes(x = temperature, y = mass)) + geom_point() +
	geom_smooth(method = "lm", color = "black") +
	ylab("Asymptotic mass") + xlab("Temperature (°C)")
ggsave("figures/linf_acc_daph.pdf", width = 6, height = 4)
ggsave("figures/linf_acc_daph_tillclutch4.pdf", width = 6, height = 4)	

params2 <- params %>% 
	separate(unique_id, into = c("temperature", "replicate"), remove = FALSE) %>% 
	mutate(temperature = as.numeric(temperature)) %>% 
	filter(term == "Linf") 

params3 <- params %>% 
	filter(term != "t0") %>%
	separate(unique_id, into = c("temperature", "replicate"), remove = FALSE) %>% 
	mutate(temperature = as.numeric(temperature)) %>% 
	select(temperature, replicate, term, estimate) %>% 
	spread(key = term, value = estimate) %>% 
	mutate(linf_mass =  0.00402*((Linf/1000)^2.66)) %>% 
	mutate(inv_temp = (1/(.00008617*(temperature + 273.15))))

write_csv(params3, "data-processed/acclimated-daphnia-vb-growth-params.csv")


params3 <- read_csv("data-processed/acclimated-daphnia-vb-growth-params.csv")
### test of supply demand model with acclimated daphnia
params3 %>% 
	ggplot(aes(x = log(K), y = log(linf_mass))) + geom_point() +
	geom_smooth(method = "lm")
params3 %>% 
ggplot(aes(x = log(K), y = log(linf_mass), color = temperature)) + 
	# stat_function( fun = prediction, color = "black", linetype = "dashed") +
	# stat_function( fun = prediction1, color = "grey", linetype = "dashed") +
	# stat_function( fun = prediction2, color = "grey", linetype = "dashed") +
	geom_smooth(method = "lm", color = "black") + scale_color_viridis() +
	geom_point(size = 4) +
	geom_point(size = 4, shape = 1) +
	ylab("ln(asymptotic body mass") + xlab("ln(K)")
ggsave("figures/acclimated-daphnia-asymptotic-mass-K.pdf", width = 6, height = 4)


### now find out if greater residuals are associated with lower fitness

ac5 <- left_join(ac4, df2, by = c("temperature", "replicate"))

### larger residuals do not seem to be associated with lower fitness
ac5 %>% 
	ggplot(aes(x = residuals, y = r, color = temperature)) + geom_point() +
	geom_smooth()


mod1 <- lm(log(linf_mass) ~ log(K), data = params3)
df <- as.data.frame(resid(mod1))

df2 <- add_residuals(params3, mod1, var = "residuals")

library(lmodel2)
library(modelr)
rma <- lmodel2(log(linf_mass) ~ log(K), data = params3, range.y = "interval", range.x = "interval")
rma$regression.results
rma$confidence.intervals
rma$rsquare
tidy(rma)



params3 %>% 
	ggplot(aes(x = inv_temp, y = log(linf_mass))) + geom_point() +
	scale_x_reverse() + geom_smooth(method = "lm", color = "black") +
	ylab("Log(Linf)") + xlab("Temperature (1/kT)")
ggsave("figures/acclimated-daphnia-asymptotic-size.pdf")


params3 %>% 
	do(tidy(lm(log(linf_mass) ~ inv_temp, data = .), conf.int = TRUE)) %>% View

### now get the clutch sizes for the acclimated daphnia

acc_clutch <- read_csv("data-raw/acclimated-daphnia-clutches.csv") 

acc_clutch %>% 
	filter(clutch_number < 7) %>% 
	ggplot(aes(x = temperature, y = final_baby_count)) + geom_point() +
	facet_wrap( ~ clutch_number, scales = "free") + geom_smooth()

## now get production rate, i.e. babies per unit time.

sizes <- ac_age2 %>% 
	filter(stage %in% c("clutch1", "clutch2", "clutch3", "clutch4", "clutch5", "clutch6"))


clutches <- acc_clutch %>% 
	mutate(stage = case_when(clutch_number == 1 ~ "clutch1",
													 clutch_number == 2 ~ "clutch2",
													 clutch_number == 3 ~ "clutch3",
													 clutch_number == 4 ~ "clutch4",
													 clutch_number == 5 ~ "clutch5",
													 clutch_number == 6 ~ "clutch6"))

all_clutches <- left_join(sizes, clutches, by = c("temperature", "replicate", "stage"))


ac2 <- all_clutches %>% 
	filter(!is.na(final_baby_count)) %>% 
	select(temperature, replicate, stage, final_baby_count, size_um, age) %>% 
	group_by(temperature, replicate) %>% 
	mutate(cumulative_babies = cumsum(final_baby_count)) %>% 
	unite(unique_id, temperature, replicate, remove = FALSE)

write_csv(ac2, "data-processed/acclimated-clutches-processed.csv")

ac2 <- read_csv("data-processed/acclimated-clutches-processed.csv")

ac2 %>% 
	ggplot(aes(x = age, y = cumulative_babies, group = unique_id, color = factor(temperature))) + geom_point() +
	geom_line() + geom_smooth(method = "lm")

### babies per unit time (reproductive output)

ac2 %>% 
	filter(!is.na(age)) %>% 
	group_by(temperature, replicate) %>% 
	do(tidy(lm(cumulative_babies ~ age, data= .), conf.int = TRUE)) %>% 
	ggplot(aes(x = temperature, y = estimate)) + geom_point() +
	facet_wrap( ~ term, scales = "free")

ac2 %>% 
	filter(!is.na(age)) %>% 
	group_by(temperature, replicate) %>% 
	do(tidy(lm(cumulative_babies ~ age, data= .), conf.int = TRUE)) %>% 
	filter(term == "age") %>% 
	ungroup() %>% 
	ggplot(aes(x = temperature, y = estimate)) + geom_point() +
	geom_smooth() +ylab("Reproductive rate (babies/day)") + xlab("Temperature (°C)") 
ggsave("figures/reproductive-rate-acclimated-daphnia.pdf", width = 6, height = 4)


### ok now let's calculate r for the acclimated daphnia

acc_plot1<- ac2 %>% 
	filter(stage %in% c("clutch3")) %>% 
	mutate(r = log(cumulative_babies)/age) %>% 
	ungroup() %>% 
	ggplot(aes(x = temperature, y = r)) + geom_point() +
	geom_smooth(color = "black") +
	xlab("Temperature (°C)") + ylab("Intrinsic rate of increase (r)")

ac2 %>% 
	# filter(stage %in% c("clutch6")) %>% 
	mutate(r = log(cumulative_babies)/age) %>% 
	ungroup() %>% 
	ggplot(aes(x = temperature, y = r)) + geom_point() +
	geom_smooth(color = "black", method = "lm") +
	facet_wrap( ~ stage)


acc_plot2 <- ac2 %>% 
	filter(stage %in% c("clutch1")) %>% 
	ungroup() %>% 
	ggplot(aes(x = temperature, y = age)) + geom_point() +
	geom_smooth(color = "black") +
	xlab("Temperature (°C)") + ylab("Generation time (days)")
	
acc_plots <- plot_grid(acc_plot1, acc_plot2, align = "v", nrow = 2, ncol = 1)
save_plot("figures/acc_fitness_plots.pdf", acc_plots,
					ncol = 1, 
					nrow = 2, 
					base_aspect_ratio = 2
)


### ok let's merge the fitness metrics with the asymptotic body size estimates to see how 
### fitness changes with body size

ac3 <- ac2 %>% 
	filter(stage %in% c("clutch3")) %>% 
	mutate(r = log(cumulative_babies)/age) %>% 
	ungroup()

params4 <- params3 %>% 
	unite(col = "unique_id", temperature, replicate, remove = FALSE) %>% 
	mutate(replicate = as.integer(replicate))

str(ac3)
str(params4)

ac4 <- left_join(ac3, params4, by = c("unique_id", "temperature", "replicate"))

ac4 %>% 
	ggplot(aes(x = linf_mass, y = r, color = factor(temperature))) + geom_point(size = 3) +
	geom_smooth(method = "lm") + scale_color_viridis_d(name = "Temperature (°C)") + scale_fill_viridis_d() +
	ylab("Intrinsic rate of increase (r)") + xlab("Asymptotic body mass (mg DW)")
ggsave("figures/r_vs_size_acclimated.pdf", width = 6, height = 4)

ac4 %>% 
	ggplot(aes(x = linf_mass, y = r)) + geom_point(size = 3) +
	geom_smooth(method = "lm", color = "black") +
	ylab("Intrinsic rate of increase (r)") + xlab("Asymptotic body mass (mg DW)")
ggsave("figures/r_vs_size_acclimated_no_temperature.pdf", width = 6, height = 4)

ac4 %>% 
	ggplot(aes(x = linf_mass, y = r, color = factor(temperature))) + geom_point(size = 3) +
	geom_smooth(method = "lm", color = "black") +
	ylab("Intrinsic rate of increase (r)") + xlab("Asymptotic body mass (mg DW)")
 

ac4 %>% 
	ggplot(aes(x = temperature, y = r, color = linf_mass)) + geom_point(size = 3) + 
	geom_smooth(method = "lm", color = "grey") + scale_color_viridis_c()
