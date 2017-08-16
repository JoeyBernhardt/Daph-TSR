

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
	filter(temperature == 27) 


all27_acc <- left_join(acc27, babies3_27, by = "replicate")


all27_acc %>% 
	ggplot(aes( x = size_um, y = final_baby_count, color = factor(replicate))) + geom_point() +
	geom_smooth(method = "lm", color = "black") + theme_bw()

all27_acc %>% 
	ungroup() %>% 
	do(tidy(lm(final_baby_count ~ size_um, data = .), conf.int = TRUE)) %>% View



# now to winter TSR babies ------------------------------------------------

winter_tsr_babies <- read_csv("data-raw/winter_tsr_babies.csv")

w_babies <- winter_tsr_babies %>% 
	filter(!is.na(number_of_babies)) %>% 
	mutate(stage = NA) %>% 
	mutate(stage = ifelse(clutch_number == 1, "clutch1", stage)) %>% 
	mutate(stage = ifelse(clutch_number == 2, "clutch2", stage)) %>% 
	mutate(stage = ifelse(clutch_number == 3, "clutch3", stage))

size <- read_csv("data-raw/daph_tsr_body_size.csv")

size2 <- clean_names(size) %>% 
	mutate(date_measured = mdy(date_measured)) %>% 
	filter(actual_size_um > 0) %>% 
	mutate(size_um = actual_size_um) %>% 
	filter(!is.na(size_um))


w_babies_size <- left_join(w_babies, size2, by = c("temperature", "replicate", "stage"))


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