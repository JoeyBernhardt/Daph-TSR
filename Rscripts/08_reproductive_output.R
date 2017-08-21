

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
library(viridis)



# read data ---------------------------------------------------------------

number_of_clutches <- read_csv("data-raw/lifespan_clutches.csv", n_max = 40)
total_clutches_27 <- read_csv("data-raw/total_clutches_27.csv")
babies <- read_csv("data-raw/lifespan_clutch_numbers.csv")

babies27 <- babies %>% 
	filter(temperature == 27)

clutch27 <- number_of_clutches %>% 
	filter(temperature == 27) %>% 
	mutate(mass =  0.00402*((max_body_size/1000)^2.66)) ## here length is in mm, so we need to divide by 1000

all27 <- left_join(clutch27, total_clutches_27)

all27 %>% 
	mutate(lifespan = as.numeric(lifespan)) %>% 
	ggplot(aes(x = total_clutches, y = mass)) + geom_point(size = 3) +
	geom_smooth(method = "lm", color = "black") + theme_bw() +ylab("Maximum body size (mg)") + xlab("Lifetime total clutches") +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica"))
ggsave("figures/total_clutches_27.png")

all27 %>% 
	mutate(lifespan = as.numeric(lifespan)) %>% 
	do(tidy(lm(log(total_clutches) ~ log(mass), data = .), conf.int = TRUE)) %>% 
	View

all27 %>% 
	mutate(lifespan = as.numeric(lifespan)) %>% 
	ggplot(aes(x = mass, y = lifespan)) + geom_point()

all27 %>% 
	mutate(lifespan = as.numeric(lifespan)) %>% 
	rename(`mass (mg DW)` = mass) %>% 
	ggplot(aes(x = lifespan, y = total_clutches)) + geom_point(aes(size = `mass (mg DW)`)) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) + ylab("Lifetime total clutches") + xlab("Lifespan (days)") +
	geom_smooth(method = "lm", color = "black") +
	geom_point(aes(size = `mass (mg DW)`)) 
ggsave("figures/clutches_v_lifespan.png")
ggsave("figures/clutches_v_lifespan.pdf")

all27 %>% 
	mutate(lifespan = as.numeric(lifespan)) %>% 
	ggplot(aes(x = mass, y = total_clutches)) + geom_point(size = 2) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) + ylab("Lifetime total clutches") + xlab("Maximum body size (mg DW)") +
	geom_smooth(method = "lm", color = "black") 
ggsave("figures/lifetime_clutches_v_size_27C.pdf")
ggsave("figures/lifetime_clutches_v_size_27C.png")
	
all27 %>% 
	mutate(lifespan = as.numeric(lifespan)) %>% 
	do(tidy(lm(total_clutches ~ mass, data = .), conf.int = TRUE)) %>% View


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
	
	
	
	
	
# now onto the acclimated experiment --------------------------------------

babies3 <- read_csv("data-raw/acc_daph_babies.csv")
size_acc <- read_csv("data-raw/acc_daph_body_size.csv")	
	
acc2 <- clean_names(size_acc)

acc27 <- acc2 %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	filter(temperature == 27) %>% 
	group_by(replicate) %>% 
	top_n(n =1, wt = size_um)
	
babies3_27 <- babies3 %>% 
	filter(temperature == 27) %>% 
	mutate(stage = clutch_number) %>% 
	mutate(stage = ifelse(stage == 1, "clutch1", stage)) %>%
	mutate(stage = ifelse(stage == 2, "clutch2", stage)) %>% 
	mutate(stage = ifelse(stage == 3, "clutch3", stage)) 


all27_acc <- left_join(acc27, babies3_27, by = "replicate")


all27_acc %>% 
	ggplot(aes( x = size_um, y = final_baby_count, color = factor(replicate))) + geom_point() +
	geom_smooth(method = "lm", color = "black") + theme_bw()

all27_acc %>% 
	ungroup() %>% 
	do(tidy(lm(final_baby_count ~ size_um, data = .), conf.int = TRUE)) %>% View


### ok let's pull out the data up to clutch 3, then join with the winter babies

acc27_c3 <- acc2 %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	filter(temperature == 27) %>% 
	filter(stage %in% c("clutch1", "clutch2", "clutch3"))

all27_3 <- left_join(acc27_c3, babies3_27, by = c("replicate", "stage")) %>% 
	filter(!is.na(final_baby_count)) 

babies_27sum <- all27_3 %>% 
	mutate(size_um = ifelse(replicate == 4 & stage == "clutch3", 1935.807, size_um)) %>%
	mutate(size_um = ifelse(replicate == 5 & stage == "clutch1", 2059.762, size_um)) %>%
	mutate(size_um = ifelse(replicate == 7 & stage == "clutch3", 1747.451, size_um)) %>% 
	mutate(size_um = as.numeric(size_um)) %>% 
	group_by(replicate) %>% 
	summarise_each(funs(mean, sum, max), final_baby_count, size_um) 


### get the days to clutch 3


days_to_clutch3 <- acc27 %>% 
	filter(stage %in% c("neonate", "clutch3")) %>%
	filter(!is.na(date_measured)) %>% 
	select(stage, date_measured, replicate) %>% 
	spread(key = stage, value = date_measured) %>%
	mutate(clutch3_age = interval(neonate, clutch3)/ddays(1)) 

summer_27 <- left_join(days_to_clutch3, babies_27sum) %>% 
	mutate(temperature = "27") %>% 
	mutate(temperature = as.numeric(temperature)) %>% 
	mutate(mass =  0.00402*((size_um_max/1000)^2.66)) %>% 
	unite(unique, temperature, replicate, remove = FALSE) %>% 
	mutate(babies_per_time = final_baby_count_sum/clutch3_age) %>%
	mutate(Temperature = as.factor(temperature)) %>% 
	ungroup()

# now to winter TSR babies ------------------------------------------------

winter_tsr_babies <- read_csv("data-raw/winter_tsr_babies.csv")

w_babies <- winter_tsr_babies %>% 
	filter(!is.na(number_of_babies)) %>% 
	mutate(stage = NA) %>% 
	mutate(stage = ifelse(clutch_number == 1, "clutch1", stage)) %>% 
	mutate(stage = ifelse(clutch_number == 2, "clutch2", stage)) %>% 
	mutate(stage = ifelse(clutch_number == 3, "clutch3", stage))

w_babies %>% 
	filter(temperature == 27 & replicate == 4) %>% View

size <- read_csv("data-raw/daph_tsr_body_size.csv")

size2 <- clean_names(size) %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	filter(actual_size_um > 0) %>% 
	mutate(size_um = actual_size_um) %>% 
	filter(!is.na(size_um))


w_babies_size <- left_join(w_babies, size2, by = c("temperature", "replicate", "stage"))

## Clutch size vs. body size
w_babies_size %>% 
	# filter(stage == "clutch3") %>% 
	mutate(mass =  0.00402*((size_um/1000)^2.66)) %>% 
	ggplot(aes(x = mass, y = number_of_babies)) + geom_point() +
	facet_wrap( ~ temperature, scales = "free") + 
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) + ylab("Clutch size") + xlab("Body size (mg DW)") +
	geom_smooth(method = "lm", color = "black") 
ggsave("figures/clutch_size_mass.pdf")
ggsave("figures/clutch_size_mass.png")

mass <- read_csv("data-processed/von_bert_mass.csv")
all_growth <- read_csv("data-processed/all_growth.csv")


all4 <- left_join(w_babies_size, all_growth)




# Generation time ---------------------------------------------------------

## graph of generation time
all_growth %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	filter(clutch1_age < 60) %>% 
	ggplot(aes(x = inverse_temp, y = log(clutch1_age))) + geom_jitter(height = 0.7, width = 0, size = 4, alpha = 0.5) +
	geom_smooth(method = "lm", color = "black") +
	scale_x_reverse() +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) + ylab("log(Generation time, days)") + xlab("Temperature (1/kT)")
ggsave("figures/generation_times_inverse.png", width = 5, height = 4)
ggsave("figures/generation_times_inverse.pdf", width = 5, height = 4)


all_growth %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	filter(clutch1_age < 60) %>% 
	do(tidy(lm(log(clutch1_age) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View

lifespan <- read_csv("data-raw/lifespan_clutches.csv", n_max = 40)

str(lifespan)

generation_time_lifespan <- lifespan %>% 
	mutate(birth_date = mdy(birth_date)) %>% 
	mutate(clutch1_bd = mdy(clutch1_bd)) %>% 
	mutate(generation_time = interval(birth_date, clutch1_bd)/ddays(1)) %>% 
	select(temperature, generation_time, replicate) %>% 
	mutate(experiment = "lifepsan")





generation_time_tsr <- all_growth %>% 
	filter(clutch1_age < 60) %>% 
	select(temperature, clutch1_age) %>% 
	rename(generation_time = clutch1_age) %>% 
	mutate(experiment = "tsr")

all_generation_times <- bind_rows(generation_time_lifespan, generation_time_tsr)

all_generation_times %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	filter(generation_time < 60) %>% 
	do(tidy(lm(log(generation_time) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View

generation_times_plot <- all_generation_times %>%
	# filter(experiment == "tsr") %>% 
	ggplot(aes(x = temperature, y = generation_time)) + 
	# geom_point(size = 2) + 
	geom_jitter(height = 0.7, width = 0, size = 2, alpha = 0.7) +
	geom_smooth(color = "black") +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) + ylab("Generation time (days)") + xlab("Temperature (°C)")
ggsave("figures/all_generation_times.pdf")
ggsave("figures/all_generation_times.png")


babies_sum <- w_babies_size %>% 
	group_by(temperature, replicate) %>% 
	summarise_each(funs(mean, sum, max), number_of_babies, number_of_babies_average, size_um) 


all5 <- left_join(all4, babies_sum)

all6 <- all5 %>% 
	mutate(mass =  0.00402*((size_um_max/1000)^2.66)) %>% 
	unite(unique, temperature, replicate, remove = FALSE) %>% 
	filter(unique != "27_4") %>% 
	mutate(babies_per_time = log(number_of_babies_sum)/clutch3_age) %>% 
	mutate(Temperature = as.factor(temperature)) %>% 
	ungroup()




# all6b <- bind_rows(all6, summer_27)


fitness_plot <- all6 %>% 
	rename(`Temperature (°C)` = Temperature) %>% 
	ggplot(aes(x = mass, y = babies_per_time, group = `Temperature (°C)`, color = `Temperature (°C)`)) + geom_point(size = 3) +
	# facet_wrap( ~ temperature, scales = "free") +
	geom_smooth(method = "lm") +
	# ylim(0, 1.3) +
	xlim(0.025, 0.125) +
	theme_bw() +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) + geom_smooth(method = "lm", aes(fill = `Temperature (°C)`)) +
	xlab("Body size (mg DW)") + ylab("Intrinsic rate of increase (r)") + scale_color_viridis(discrete = TRUE) +
	scale_fill_viridis(discrete = TRUE) 
	# + geom_abline(slope = -31.640328, intercept = 2.979307, color = "grey", linetype = "dashed", size = 1) 
ggsave("figures/offspring_per_day.pdf", width = 8, height = 5)
ggsave("figures/offspring_per_day.png", width = 8, height = 5)


# stats for intrinsic growth rate -----------------------------------------
library(visreg)
library(nlme)
library(cowplot)
mod <- lm(babies_per_time ~ mass*temperature, data = all6)
summary(mod)
visreg(mod)
tidy(mod, conf.int = TRUE) %>% View

# mod2 <- lme(babies_per_time ~ mass*temperature, data = all6)
mod2 <- lmer(babies_per_time ~ mass*temperature + (1|temperature), data = all6)
summary(mod2)
tidy(mod2)

vb <- read_csv("data-processed/von_bert_mass.csv")

all7 <- left_join(vb, all6, by = c("temperature", "replicate"))
all7 %>% 
	# filter(Linf < 4000) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	mutate(mass_corr_r =babies_per_time*linf_mass^(1/4)) %>% 
	do(tidy(lm(log(mass_corr_r) ~ inverse_temp, data = .), conf.int = TRUE)) %>% View

mass_corr_r_plot <- all7 %>% 
	filter(Linf < 4000) %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	mutate(mass_corr_r = babies_per_time*linf_mass^(1/4)) %>% 
	mutate(log_mass = log(linf_mass)) %>% 
	ggplot(aes(x = inverse_temp, y = log(mass_corr_r), color = log_mass)) + geom_point(size = 4) +
	geom_smooth(method = "lm", color = "black") +
	scale_x_reverse() +
	scale_color_viridis() + 
	theme_bw() + 
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) + ylab(bquote('Log(r*'*mass^{1/4}*')')) + xlab("Temperature (1/kT)")


q <- plot_grid(mass_corr_r_plot, fitness_plot, labels = c("A", "B"), nrow = 2, align = "v")
save_plot("figures/fitness_color.png", q,
					ncol = 1, # we're saving a grid plot of 2 columns
					nrow = 2, # and 2 rows
					# each individual subplot should have an aspect ratio of 1.3
					base_aspect_ratio = 1.7)

save_plot("figures/fitness_color.pdf", q,
					ncol = 1, # we're saving a grid plot of 2 columns
					nrow = 2, # and 2 rows
					# each individual subplot should have an aspect ratio of 1.3
					base_aspect_ratio = 1.7)

all6 %>% 
	rename(`Temperature (°C)` = Temperature) %>% 
	ggplot(aes(x = mass, y = number_of_babies_average_mean, group = `Temperature (°C)`, color = `Temperature (°C)`)) + geom_point(size = 3) +
	# facet_wrap( ~ temperature, scales = "free") +
	geom_smooth(method = "lm") +
	# ylim(0, 1.3) +
	xlim(0.025, 0.125) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) + geom_smooth(method = "lm", aes(fill = `Temperature (°C)`)) +
	xlab("Body size (mg DW)") + ylab("Clutch size") + scale_color_viridis(discrete = TRUE) +
	scale_fill_viridis(discrete = TRUE) 

all6 %>% 
	ggplot(aes(x = actual_size_um, y = number_of_babies, color = factor(temperature))) + geom_point() +
	facet_wrap( ~ temperature) + geom_smooth(method = "lm")

all6 %>% 
	group_by(temperature) %>% 
	do(tidy(lm(number_of_babies_average_mean ~ mass, data = .), conf.int = TRUE)) %>%
	filter(term != "(Intercept)") %>% 
	ggplot(aes(x = temperature, y = estimate)) + geom_point() +
	geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) + geom_hline(yintercept = 0) + xlab("Temperature (°C)") +
	ylab("Slope of reproductive output vs. body size")


all6 %>% 
	group_by(temperature) %>% 
	do(tidy(lm(babies_per_time ~ mass, data = .), conf.int = TRUE)) %>%
	filter(term != "(Intercept)") %>% 
	ggplot(aes(x = temperature, y = estimate)) + geom_point() +
	geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				axis.line = element_line(color="black"), 
				panel.border = element_rect(colour = "black", fill=NA, size=1))+
	theme(text = element_text(size=16, family = "Helvetica")) + geom_hline(yintercept = 0) + xlab("Temperature (°C)") +
	ylab("Slope of reproductive output vs. body size")
ggsave("figures/slope_of_rep_output_vs_size.pdf")
ggsave("figures/slope_of_rep_output_vs_size.png")


all6 %>% 
	mutate(mass =  0.00402*((size_um_max/1000)^2.66)) %>% 
	unite(unique, temperature, replicate, remove = FALSE) %>% 
	filter(unique != "27_4") %>% 
	mutate(babies_per_time = number_of_babies_sum/clutch3_age) %>% 
	do(tidy(lm(babies_per_time ~ mass, data = .), conf.int = TRUE)) %>% View