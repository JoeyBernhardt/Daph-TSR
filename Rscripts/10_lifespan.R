

library(tidyverse)
library(lubridate)
library(cowplot)
library(broom)


lifespan <- read_csv("data-raw/lifespan-data.csv") %>% 
	filter(!is.na(temperature))


ls2 <- lifespan %>% 
	gather(birth_date, death_date, 9:50, key = "clutch_no", value = "date") %>% 
	mutate(date = mdy(date)) %>% 
	spread(key = clutch_no, value = date)

ls3 <- ls2 %>% 
	# filter(temperature > 10) %>% 
	mutate(lifespan_calc = interval(birth_date, death_date)/ddays(1)) %>% 
	mutate(mass =  0.00402*((max_body_size/1000)^2.66)) %>% 
	mutate(inv_temp = (1/(.00008617*(temperature + 273.15))))


ls3 %>% 
	ggplot(aes(x = temperature, y = lifespan_calc)) + geom_point() +
	geom_smooth(method = "lm") +
	ylab("Lifespan (days)") + xlab("Temperature (°C)")

ls3 %>% 
	lm(lifespan_calc ~ temperature, data = .) %>% summary()


ls3 %>% 
	ggplot(aes(x = log(mass), y = log(lifespan_calc))) + geom_point() +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Lifespan) (days)") + xlab("ln(Mass)")


ls3 %>% 
	filter(temperature > 10) %>%
	ggplot(aes(x = log(mass), y = log(days_to_clutch1*(mass^0.25)))) + geom_point() +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Generation time) (days)") + xlab("ln(Mass)")

ls3 %>% 
	filter(temperature > 10) %>%
	ggplot(aes(x = log(mass), y = log(lifespan_calc*(mass^0.25)))) + geom_point() +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Generation time) (days)") + xlab("ln(Mass)")


ls3 %>% 
	filter(temperature > 10) %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	ggplot(aes(x = inv_temp, y = log(lifespan_calc*(mass^0.25)))) + geom_point() +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Lifespan x M^0.25) (days)") + xlab("Temperature") +
	scale_x_reverse()
ggsave("figures/mass_corr_lifespan_v_temp.pdf", width = 6, height = 4)


### generation time
ls3 %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	filter(temperature > 10) %>% 
	ggplot(aes(x = inv_temp, y = log(days_to_clutch1))) + geom_jitter(width = 0.01) +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Generation time) (days)") + xlab("Temperature (1/kT)") +
	scale_x_reverse()
ggsave("figures/generationtime_v_temp.pdf", width = 6, height = 4)


ls3 %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	# filter(temperature > 12) %>% 
	ggplot(aes(x = inv_temp, y = days_to_clutch1)) + geom_jitter(width = 0.01) +
	geom_smooth(color = "black") +
	ylab("Generation time (days)") + xlab("Temperature (1/kT)") +
	scale_x_reverse()


ls3 %>% 
	# filter(temperature > 10) %>% 
	lm(log(days_to_clutch1) ~ inv_temp, data = .) %>% tidy(conf.int = TRUE)

ls3 %>% 
	lm(log(lifespan_calc) ~ log(max_body_size), data = .) %>% summary()

ls3 %>% 
	filter(temperature > 10) %>% 
	lm(log(lifespan_calc) ~ log(mass), data = .) %>% tidy(conf.int = TRUE)

ls3 %>% 
	filter(temperature > 10) %>% 
	lm(log(lifespan_calc) ~ inv_temp, data = .) %>% tidy(conf.int = TRUE)

ls4 <- ls3 %>% 
	mutate(temp_kelvin = temperature + 273.15) %>%
	mutate(temp_corr_lifespan = lifespan_calc*(exp((0.51/0.00008617)*((1/temp_kelvin)-(1/293.15))))) %>% 
	mutate(temp_corr_gen = days_to_clutch1*(exp((0.51/0.00008617)*((1/temp_kelvin)-(1/293.15))))) %>% 
	mutate(clutch3_age = interval(birth_date, clutch1_bd)/ddays(1)) 

### somehow figure out how to get generation time corrected to 20C
ls4 %>% 
	filter(temperature > 10) %>% 
	lm(log(temp_corr_lifespan) ~ log(mass), data = .) %>% tidy(conf.int = TRUE)


ls4 %>% 
	filter(temperature > 10) %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	ggplot(aes(x = log(mass), y = log(temp_corr_lifespan))) + geom_jitter(width = 0.1) +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Lifespan at 20°C) (days)") + xlab("ln(Mass)") 
ggsave("figures/lifespan_v_mass.pdf", width = 6, height = 4)


ls4 %>% 
	filter(temperature > 10) %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	ggplot(aes(x = log(mass), y = log(temp_corr_gen))) + geom_jitter(width = 0.1) +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Generation time at 20°C) (days)") + xlab("ln(Mass)") +
	scale_x_reverse()
ggsave("figures/generationtime_v_mass.pdf", width = 6, height = 4)

ls3 %>% 
	filter(temperature > 10) %>% 
	lm(log(days_to_clutch1) ~ inv_temp, data = .) %>% summary()

ls3 %>% 
	filter(temperature > 10) %>% 
	lm(log(days_to_clutch1) ~ inv_temp, data = .) %>% tidy(conf.int = TRUE)

## get total clutches per lifetime

total_clutches <- ls3 %>% 
	filter(!is.na(days_to_clutch1)) %>% 
	gather(starts_with("clutch"), key = "clutch_number", value = "clutch_date") %>% 
	filter(!is.na(clutch_date)) %>% 
	group_by(temperature, replicate) %>% 
	distinct(clutch_number) %>% 
	tally()


## total clutches over lifetime
total_clutches %>% 
	filter(temperature > 10) %>% 
	ungroup() %>% 
	ggplot(aes(x  = temperature, y = n)) + geom_point() + 
	geom_smooth(method = "lm", color = "black") + xlab("Temperature (°C)") + ylab("Lifetime number of clutches")
ggsave("figures/lifetime_clutches.pdf", width = 6, height = 4)


ls5 <- left_join(ls4, total_clutches, by = c("temperature", "replicate"))


ls5 %>% 
	filter(temperature > 10) %>% 
	mutate(clutches_per_time = n/lifespan_calc) %>% 
	ggplot(aes(x = inv_temp, y = log(clutches_per_time))) + geom_point() + scale_x_reverse() +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Clutches per day)") + xlab("Temperature (1/kT)")
ggsave("figures/ln_clutches_per_time.pdf", width = 6, height = 4)

ls5 %>% 
	filter(temperature > 10) %>% 
	mutate(clutches_per_time = n/lifespan_calc) %>% 
	ggplot(aes(x = inv_temp, y = clutches_per_time)) + geom_point() + scale_x_reverse() +
	geom_smooth(method = "lm", color = "black") + ylab("Clutches per day") + xlab("Temperature (1/kT)")
ggsave("figures/clutches_per_time.pdf", width = 6, height = 4)

ls5 %>% 
	filter(temperature > 10) %>% 
	mutate(clutches_per_time = n/lifespan_calc) %>%
	lm(log(clutches_per_time) ~ inv_temp, data = .) %>% tidy(conf.int = TRUE)

ls5 %>% 
	filter(temperature > 10) %>% 
	mutate(clutches_per_time = n/lifespan_calc) %>%
	lm(n ~ temperature, data = .) %>% tidy(conf.int = TRUE)

ls5 %>% 
	filter(temperature > 10) %>% 
	mutate(clutches_per_time = n/lifespan_calc) %>%
	lm(log(lifespan_calc*mass^0.25) ~ inv_temp, data = .) %>% tidy(conf.int = TRUE)


ls5 %>% 
	filter(temperature > 10) %>% 
	ggplot(aes(x = temperature, y = mass)) + geom_point() +
	ylab("Body mass at death (mg)") + xlab("Temperature (°C)")
ggsave("figures/lifespan_size_at_death.png", width = 7, height = 5)
	

ls5 %>% 
	filter(temperature > 10) %>% 
	mutate(clutches_per_time = n/lifespan_calc) %>% 
	ggplot(aes(x = inv_temp, y = log(lifespan_calc))) + geom_point() + scale_x_reverse() +
	geom_smooth(method = "lm", color = "black") + ylab("ln(Lifespan) (days)") + xlab("Temperature (1/kT)")
ggsave("figures/lifespan_v_temp.pdf", width = 6, height = 4)



### now bring in the clutch sizes

ls_clutches <- read_csv("data-raw/lifespan-clutches.csv")


ls_clutch_sum <- ls_clutches %>% 
	filter(!is.na(baby_count)) %>% 
	group_by(temperature, replicate) %>% 
	summarise_each(funs(mean), baby_count)

ls_clutch_sum %>% 
	ungroup() %>% 
	ggplot(aes(x = temperature, y = baby_count)) + geom_point() +
	geom_smooth()


ls_clutch_sum %>% 
	ungroup() %>% 
	ggplot(aes(x = temperature, y = baby_count)) + geom_point() +
	geom_smooth()

ls_clutches %>% 
	ggplot(aes(x = temperature, y = baby_count, color = clutch_number)) + geom_point(size = 3) + geom_smooth(method = "lm", color = "black") +
	ylab("Offspring per clutch") + xlab("Temperature (°C)") + scale_color_viridis_c()
ggsave("figures/lifespan_offspring_per_clutch.pdf", width = 7, height = 5)

lm(baby_count ~ temperature, data = ls_clutches) %>% tidy(conf.int = TRUE)


ls_clutches %>% 
	gather(5:8, key = clutch, value = baby_size) %>% 
	ggplot(aes(x = temperature, y = baby_size, color = clutch)) + geom_jitter(width = 0.2, size = 2) + scale_color_viridis_d() +
	ylab("Offspring size (um)") + xlab("Temperature (°C)")
ggsave("figures/lifespan_offspring_size.pdf", width = 7, height = 5)

ls_clutches %>% 
	ggplot(aes(x = baby_count, y = avg_baby_size_um)) + geom_point(size = 3) + 
	# geom_smooth(method= "lm") +
	# scale_color_viridis_c(name = "Temperature (°C)") +
	ylab("Offspring size (um)") + xlab("Offspring number per clutch")
ggsave("figures/lifespan_offspring_size_fecundity_tradeoff_no_colour.pdf", width = 6, height = 5)


### now join the clutch sizes with the clutches

all_clutches <- left_join(total_clutches, ls_clutch_sum)

all_clutches %>% 
	mutate(R0 = n*baby_count) %>% 
	ungroup() %>% 
	ggplot(aes(x = temperature, y = R0)) + geom_point() +
	geom_smooth(color = "black") +
	xlab("Temperature (°C)") + ylab("R0 (total babies per lifetime)")
ggsave("figures/lifetime_reproductive_output.pdf", width = 6, height = 5)

all_clutches %>% 
	ggplot(aes(x = temperature, y = baby_count)) + geom_point()
all_clutches %>% 
	ggplot(aes(x = temperature, y = n)) + geom_point()

baby_sizes <- ls_clutches %>% 
	filter(!is.na(clutch_number)) %>% 
	select(temperature, replicate, clutch_number, baby1_size_um, baby2_size_um, baby3_size_um, baby4_size_um, baby_count) %>% 
	gather(key = clutch_no, value = size, 4:7) %>% 
	filter(!is.na(size)) %>% 
	group_by(temperature, replicate, clutch_number, baby_count) %>% 
	summarise(mean_baby_size = mean(size)) %>% 
	mutate(production = mean_baby_size*baby_count)


baby_sizes %>% 
	ungroup() %>% View
	ggplot(aes(x = clutch_number, y = production)) + geom_point() +
	facet_wrap( ~ temperature, scales = "free") + geom_smooth(method = "lm")

baby_sizes %>% 
	ungroup() %>% 
	ggplot(aes(x = clutch_number, y = mean_baby_size)) + geom_point() +
	geom_smooth(method = "lm") +
	facet_wrap( ~ temperature, scales = "free")

baby_size_summary <- baby_sizes %>% 
	group_by(temperature, replicate) %>% 
	summarise(mean_baby_size_all = mean(mean_baby_size))


b_size <- left_join(all_clutches, baby_size_summary) %>% 
	mutate(baby_mass =  0.00402*((mean_baby_size_all/1000)^2.66))


b_size %>% 
	mutate(lifetime_production = n*baby_mass) %>% 
	ungroup() %>% 
	ggplot(aes(x = temperature, y = baby_mass)) + geom_point() +
	geom_smooth(method = "lm", color = "black") +
	ylab("Mean offspring size (mg C)") + xlab("Temperature (°C)")
ggsave("figures/mean_offspring_size_lifespan.pdf", width = 6, height = 5)

b_size %>% 
	mutate(lifetime_production = n*baby_mass) %>% 
	ungroup() %>% 
	ggplot(aes(x = temperature, y = lifetime_production)) + geom_point() +
	geom_smooth(method = "lm", color = "black") + ylab("Lifetime production (mg C)") +
	xlab("Temperature (°C)")
ggsave("figures/lifetime_production.pdf", width = 6, height = 5)

b_size %>% 
	mutate(lifetime_production = n*baby_mass) %>% 
	ungroup() %>% 
	ggplot(aes(x = temperature, y = n)) + geom_point() +
	geom_smooth(method = "lm")

b_size2 <- b_size %>% 
	mutate(lifetime_production = n*baby_mass) %>% 
	ungroup()

lm(lifetime_production ~ temperature, data = b_size2) %>% summary()


ls_clutches %>% 
	ggplot(aes(x = clutch_number, y = baby_count)) + geom_point() + geom_smooth() +
	facet_wrap( ~ temperature, scales = "free")

str(lifespan)

lifespan %>% 
	mutate(clutch3_age = interval(mdy(birth_date), mdy(clutch1_bd))/ddays(1)) %>% View


ls6 <- left_join(ls5, ls_clutch_sum)

### plot of r vs temp for lifespan daphnia
plot1 <- ls6 %>% 
	# filter(temperature > 10) %>% 
	mutate(r = log(baby_count*3)/clutch3_age) %>% 
	ggplot(aes(x = temperature, y = r)) + geom_point(size = 3, alpha = 0.5) +
	geom_smooth(color = "black") + 
	xlim(10, 27) +
	xlab("Temperature (°C)") + ylab("Intrinsic rate of increase (r)")


## generation time
plot2 <- ls6 %>% 
	# filter(temperature > 10) %>% 
	mutate(r = log(baby_count*3)/clutch3_age) %>% 
	ggplot(aes(x = temperature, y = days_to_clutch1)) + geom_point(size = 3, alpha = 0.5) +
	geom_smooth(color = "black") +
	xlim(10, 27) +
	xlab("Temperature (°C)") + ylab("Generation time (days)")

### lifetime rep output (R0)
plot3 <- all_clutches %>%
	filter(temperature > 10) %>% 
	mutate(R0 = n*baby_count) %>% 
	ungroup() %>% 
	ggplot(aes(x = temperature, y = R0)) + geom_point(size = 3, alpha = 0.5) +
	geom_smooth(color = "black") +
	xlim(10, 27) +
	xlab("Temperature (°C)") + ylab("R0 (total babies per lifetime)")

## lifetime production
plot4 <- b_size %>% 
	filter(temperature > 10) %>% 
	mutate(lifetime_production = n*baby_mass) %>% 
	ungroup() %>% 
	ggplot(aes(x = temperature, y = lifetime_production)) + geom_point(size = 3, alpha = 0.5) +
	geom_smooth(color = "black", method = "lm") + ylab("Lifetime production (mg C)") +
	xlim(10, 27) +
	xlab("Temperature (°C)") 

plot5 <- ls6 %>% 
	filter(temperature > 10) %>% 
	ggplot(aes(x = temperature, y = lifespan_calc)) + geom_point(size = 3, alpha = 0.5) +
	geom_smooth(color = "black", method = "lm") +
	xlim(10, 27) +
	xlab("Temperature (°C)") + ylab("Lifespan (days)")

plot6 <- total_clutches %>% 
	filter(temperature > 10) %>% 
	ungroup() %>% 
	ggplot(aes(x  = temperature, y = n)) + geom_point(size = 3, alpha = 0.5) + 
	xlim(10, 27) +
	geom_smooth(method = "lm", color = "black") + xlab("Temperature (°C)") + ylab("Lifetime clutches")


all_plots_10b <- plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, align = "v", nrow = 3, ncol = 2, labels = c("A", "B", "C", "D", "E", "F"))
save_plot("figures/all_lifespan_plots_10c.pdf", all_plots_10b,
					ncol = 2, 
					nrow = 3, 
					base_aspect_ratio = 1.5,
					base_height = 3.3
  )
 
    ?save_plot
?plot_grid
