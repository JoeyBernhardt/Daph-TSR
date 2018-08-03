

### acclimated experiment

library(cowplot)
library(tidyverse)
library(lubridate)
library(janitor)
library(viridis)
library(minpack.lm)
library(nls.multstart)
library(nlstools)

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
	mutate(mass =  0.00402*((size_um/1000)^2.66)) %>% 
	filter(stage %in% c("clutch1","clutch2", "clutch3", "clutch4", "clutch5", "clutch6")) %>% 
	ggplot(aes(x = temperature, y = mass)) + geom_point() + 
	geom_smooth(method = "lm") +
	facet_wrap( ~ stage)

ggsave("figures/acclimated_daphnia_clutches_time.pdf", width = 7, height = 5)


## make an age column

ac_age <- acc %>% 
	group_by(replicate, temperature) %>% 
	mutate(birth_date = min(date_measured)) %>% 
	mutate(age = (date_measured - birth_date)/ddays(1)) %>% 
	filter(!is.na(date_measured)) %>% 
	filter(!is.na(age)) %>% 
	unite(unique_id, temperature, replicate, remove = FALSE)



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


fits <- ac_age %>% 
	group_by(unique_id) %>%
	nest() %>% 
	mutate(fit = purrr:::map(data, ~ nls_multstart(size_um ~ vbT(age, Linf, K, t0),
																							 data = .x,
																							 iter = 500,
																							 start_lower = c(Linf = 500, K = 0, t0 = -5),
																							 start_upper = c(Linf = 1000, K = 0.5, t0 = 0),
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

# get predictions
preds <- fits %>%
	unnest(fit %>% map(augment))

select(info, unique_id, logLik, AIC, BIC, deviance, df.residual) %>% View

new_preds <- ac_age %>%
	group_by(temperature, replicate, unique_id) %>% 
	do(., data.frame(age = seq(min(.$age), max(.$age), length.out = 100), stringsAsFactors = FALSE)) %>% 
	ungroup()

max_min <- group_by(ac_age, temperature, replicate, unique_id) %>%
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
	geom_point(aes(x = age, y = size_um, group = unique_id, color = factor(temperature)),
						 size = 2, data = ac_age) +
	geom_line(aes(x= age, y = size_um, group = factor(unique_id1), color = factor(temperature.x)), data = preds2) +
	facet_wrap(~ temperature + replicate, labeller = labeller(.multi_line = FALSE)) +
	# scale_colour_manual(values = c('green4', 'black')) +
	theme_bw(base_size = 12, base_family = 'Helvetica') +
	ylab('Size') +
	xlab('Age') +
	theme(legend.position = c(0.9, 0.15))

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



params %>% 
	separate(unique_id, into = c("temperature", "replicate"), remove = FALSE) %>% 
	mutate(temperature = as.numeric(temperature)) %>% 
	filter(term == "Linf") %>% 
	ggplot(aes(x = temperature, y = estimate)) + geom_point() +
	geom_smooth(method = "lm")
	


ac_age %>% 
	group_by(replicate, temperature) %>% 
	do(tidy(nls.lm(size_um ~ vbT(age,Linf,K,t0), data=., start = start_list))) %>% View
	mutate(temperature = 24)

	?nls

	mod <- nls(size_um~vbT(age,Linf,K,t0), data=ac_age, start = start_list)
	summary(mod)
	tidy(mod)
### now get the clutch sizes for the acclimated daphnia

acc_clutch <- read_csv("data-raw/acclimated-daphnia-clutches.csv") 

acc_clutch %>% 
	filter(clutch_number < 7) %>% 
	ggplot(aes(x = temperature, y = final_baby_count)) + geom_point() +
	facet_wrap( ~ clutch_number, scales = "free") + geom_smooth()
