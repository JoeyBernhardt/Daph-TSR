

### figure with sizes over time



# acute daphnia -----------------------------------------------------------


size <- read_csv("data-raw/daph_tsr_body_size.csv")

size2 <- clean_names(size) %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	filter(actual_size_um > 0) %>% 
	mutate(size_um = actual_size_um) %>% 
	filter(!is.na(size_um))



size3 <- size2 %>% 
	filter(actual_size_um > 0) %>% 
	filter(stage != "neonate") %>% 
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	mutate(mass =  0.00402*((actual_size_um/1000)^2.66)) %>% 
	mutate(log_mass =  log(mass)) %>% 
	select(temperature, log_mass, stage, mass) %>% 
	mutate(experiment = "acute")
	
	
	
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


# acclimated daphnia ------------------------------------------------------


acc <- read_csv("data-raw/acclimated-daphnia-body-size.csv") %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	clean_names()


acc_size <- acc %>% 
	mutate(mass =  0.00402*((size_um/1000)^2.66)) %>% 
	filter(stage %in% c("clutch1","clutch2", "clutch3", "clutch4", "clutch5", "clutch6")) %>% 
	mutate(log_mass = log(mass)) %>% 
	select(temperature, stage, log_mass, mass) %>% 
	mutate(experiment = "acclimated")



all_sizes <- bind_rows(size3, acc_size)


all_sizes %>% 
	ggplot(aes(x = temperature, y = mass, color = experiment)) + geom_point() + 
	geom_smooth(method = "lm", aes(fill = experiment)) +
	facet_wrap( ~ stage) + ylab("Dry mass (mg)") + xlab("Temperature (°C)") +
	scale_color_viridis_d(begin = 0.2, end = 0.9) +
	scale_fill_viridis_d(begin = 0.2, end = 0.9) +
	geom_point(shape = 1, color = "black", size = 1.5)+
	theme(legend.position = c(0.1, 0.9))
ggsave("figures/size_clutches_acclimated_acute.png", width = 8, height = 5)
