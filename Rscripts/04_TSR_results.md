# Temperature size rule results







### Background

Something here about the TSR being the third 'universal' response to warming. However, the mechanisms responsible for this widespread pattern are unknown blah blah blah, particularly w/r/t whether being smaller at warmer temperatures is associated with higher fitness. Or else why would this pattern be so prevalent in nature?



```r
data3 <- read_csv("/Users/Joey/Documents/Daph-TSR/data-processed/data3.csv")
```


### Reproductive rate
How does reproductive rate vary with temperature?
![](04_TSR_results_files/figure-html/reproductive rate-1.png)<!-- -->


```r
data3 %>% 
	filter(unique_id != "K_16") %>% ## something weird is going on here!
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	do(tidy(lm(log(time_to_first_clutch) ~ inverse_temp, data = .), conf.int = TRUE)) %>%
	knitr::kable(.)
```



term               estimate   std.error   statistic   p.value      conf.low    conf.high
-------------  ------------  ----------  ----------  --------  ------------  -----------
(Intercept)     -13.5224258   2.3825210    -5.67568   7.6e-06   -18.4397074   -8.6051442
inverse_temp      0.4788195   0.0596525     8.02681   0.0e+00     0.3557027    0.6019362


Time between clutches

![](04_TSR_results_files/figure-html/time between clutches-1.png)<!-- -->


```r
## time between clutches
data3 %>% 
	filter(unique_id != "K_16") %>% ## something weird is going on here!
	mutate(inverse_temp = (1/(.00008617*(temperature+273.15)))) %>%
	do(tidy(lm(log(time_btw_1_2) ~ inverse_temp, data = .), conf.int = TRUE)) %>%
	knitr::kable(.)
```



term               estimate   std.error   statistic     p.value      conf.low    conf.high
-------------  ------------  ----------  ----------  ----------  ------------  -----------
(Intercept)     -19.7276177   5.1098596   -3.860697   0.0008466   -30.3248179   -9.1304174
inverse_temp      0.6086533   0.1281431    4.749792   0.0000967     0.3429007    0.8744058

### Body size

![](04_TSR_results_files/figure-html/body size-1.png)<!-- -->

### Somatic growth rates

![](04_TSR_results_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
data3 %>% 
	filter(unique_id != "K_16") %>% ## something weird is going on here!
	mutate(inverse_temp = (-1/(.00008617*(temperature+273.15)))) %>%
	mutate(somatic_growth_rate = ((length_at_1st_clutch - length_at_birth_um)/time_to_first_clutch)) %>%
do(tidy(lm(log(somatic_growth_rate) ~ inverse_temp, data = .), conf.int = TRUE)) %>%
	knitr::kable(.)
```



term              estimate   std.error   statistic   p.value     conf.low    conf.high
-------------  -----------  ----------  ----------  --------  -----------  -----------
(Intercept)     23.0958538   2.5278438    9.136583     1e-07   17.7625697   28.4291380
inverse_temp     0.5404576   0.0632702    8.542051     1e-07    0.4069691    0.6739461

### Size rate trade-off??

![](04_TSR_results_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Body size over time
![](04_TSR_results_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


Minimum adult body size
![](04_TSR_results_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


Maximum adult body size
![](04_TSR_results_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Mean adult body size
![](04_TSR_results_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Next step: bring in the fecundity data to merge with the size data


```r
lengths_clean <- lengths_all_adult %>% 
	mutate(unique_id = str_replace(unique_id, "2N1.2", "N")) %>% 
	mutate(unique_id = str_replace(unique_id, "2N1.3", "N")) %>%
	mutate(unique_id = str_replace(unique_id, "2N1.4", "N")) %>% 
	mutate(unique_id = str_replace(unique_id, "2N1.8", "N")) %>%
	mutate(unique_id = str_replace(unique_id, "2N1.5", "N")) %>% 
	mutate(unique_id = str_replace(unique_id, "2N1.1", "N")) %>% 
	separate(unique_id, into = c("letter", "temp"), remove = FALSE) %>% 
	mutate(life_stage = ifelse(life_stage == "1", "clutch_1", life_stage)) %>% 
	mutate(life_stage = ifelse(life_stage == "2", "clutch_2", life_stage)) %>%
	mutate(life_stage = ifelse(life_stage == "3", "clutch_3", life_stage)) %>%
	mutate(life_stage = ifelse(life_stage == "4", "clutch_4", life_stage)) %>% 
	rename(clutch_number = life_stage)

	
data_raw <- read_csv("/Users/Joey/Documents/Daph-TSR/data-raw/DAPH-TSR-clutches.csv")
```

```
## Parsed with column specification:
## cols(
##   ID = col_character(),
##   temperature = col_integer(),
##   clutch_number = col_character(),
##   individuals = col_integer(),
##   clutch_date = col_character(),
##   sample_date = col_character()
## )
```

```r
v2_babies <- data_raw %>% 
	filter(grepl("V2", ID)) %>% 
	separate(ID, into = c("V", "letter"), sep = 2) %>% 
	unite(unique_id, letter, temperature, sep = "_")


data_raw %>% 
	filter(str_detect(ID, "N")) %>% 
	arrange(temperature, clutch_number)### come back to filling out the clutch number business here, it's clearly not totally complete!
```

```
## # A tibble: 14 × 6
##       ID temperature clutch_number individuals   clutch_date   sample_date
##    <chr>       <int>         <chr>       <int>         <chr>         <chr>
## 1    V2N          16      clutch_1           2  July 19 2016 August 5 2016
## 2    V2N          16      clutch_2           4  July 23 2016 August 5 2016
## 3    V2N          16      clutch_3           0 August 1 2016 August 8 2016
## 4    V2N          16      clutch_4           9 August 4 2016 August 5 2016
## 5    V2N          20      clutch_1          14  July 13 2016  July 18 2016
## 6    V2N          20      clutch_1          17  July 16 2016  July 18 2016
## 7    V2N          20      clutch_2          15  July 19 2016 August 5 2016
## 8    V2N          20      clutch_3           5  July 21 2016 August 5 2016
## 9    V2N          24      clutch_1           9  July 17 2016          <NA>
## 10   V2N          24      clutch_2          18  July 18 2016          <NA>
## 11   V2N          24      clutch_3           5  July 20 2016 August 5 2016
## 12   V2N          24      clutch_3          NA  July 20 2016          <NA>
## 13   V2N          24      clutch_4           0  July 22 2016 August 5 2016
## 14   V2N          24      clutch_4          NA  July 22 2016          <NA>
```

```r
all <- left_join(lengths_clean, v2_babies, by = c("unique_id", "clutch_number"))

ggplot(data = all, aes(x = length, y = individuals, color = factor(temperature))) + geom_point(size = 4)
```

```
## Warning: Removed 258 rows containing missing values (geom_point).
```

![](04_TSR_results_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

For a given temperature, plot size vs. number of individuals


```r
all %>% 
	# filter(temperature == 24) %>% 
ggplot(data = ., aes(x = length, y = individuals, color = factor(temperature))) + geom_point(size = 4) + facet_wrap( ~ temperature)
```

```
## Warning: Removed 258 rows containing missing values (geom_point).
```

![](04_TSR_results_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
How do we answer the question: is being smaller at a given temperature maximizing your fitness?

```r
all %>% 
	filter(individuals != 0) %>% 
	# filter(temperature == 24) %>%
ggplot(data = ., aes(x = length, y = individuals, color = factor(temperature))) + geom_point(size = 4) + geom_smooth(method = "lm") +
	facet_wrap( ~ temperature)
```

![](04_TSR_results_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
	

```r
all %>% 
	filter(temperature == 12) %>%
	filter(individuals != 0) %>% 
ggplot(data = ., aes(x = length, y = individuals, color = factor(temperature))) + geom_point(size = 4) + geom_smooth(method = "lm") +
	facet_wrap( ~ temperature)
```

![](04_TSR_results_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

