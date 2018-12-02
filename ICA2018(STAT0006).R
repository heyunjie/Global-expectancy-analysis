full <- read_excel("~/Desktop/2018STAT0006ICA/icadata.csv")
View(full)
summary(full)
## remove rows lacking life expectancy
full <- full[!is.na(full$lifeexp2018),]
## consider useful columns(without too many NAs)
a <- is.na(full[,3:20])
View(a)
summary(a)
## likely useful covariates : agri_2016,  broadband_2016, mobile_2016,child_mort_2018,child_per_woman_2018,income_pp_2018 , inflation_2017
## internet_2016,self_employed_2018 sl_emp_2017, urban_pop_2017, four_regions,  worldbankregion,  pop_dens_2018
full <- full[,c("country","lifeexp2018","agri_2016","broadband_2016", "mobile_2016","child_mort_2018","child_per_woman_2018","income_pp_2018", "inflation_2017","internet_2016","self_employed_2018","sl_emp_2017", "urban_pop_2017", "four_regions", "worldbankregion", "pop_dens_2018")]
## replace NAs by mean values
full$agri_2016[is.na(full$agri_2016)] <- rep(mean(full$agri_2016[!is.na(full$agri_2016)]),length(full$agri_2016[is.na(full$agri_2016)]))
full$broadband_2016[is.na(full$broadband_2016)] <- rep(mean(full$broadband_2016[!is.na(full$broadband_2016)]),length(full$broadband_2016[is.na(full$broadband_2016)]))
full$child_mort_2018[is.na(full$child_mort_2018)] <- rep(mean(full$child_mort_2018[!is.na(full$child_mort_2018)]),length(full$child_mort_2018[is.na(full$child_mort_2018)]))
full$inflation_2017[is.na(full$inflation_2017)] <- rep(mean(full$inflation_2017[!is.na(full$inflation_2017)]),length(full$inflation_2017[is.na(full$inflation_2017)]))
full$inflation_2017[is.na(full$inflation_2017)] <- rep(mean(full$inflation_2017[!is.na(full$inflation_2017)]),length(full$inflation_2017[is.na(full$inflation_2017)]))
full$internet_2016[is.na(full$internet_2016)] <- rep(mean(full$internet_2016[!is.na(full$internet_2016)]),length(full$internet_2016[is.na(full$internet_2016)]))
full$self_employed_2018[is.na(full$self_employed_2018)] <- rep(mean(full$self_employed_2018[!is.na(full$self_employed_2018)]),length(full$self_employed_2018[is.na(full$self_employed_2018)]))
full$sl_emp_2017[is.na(full$sl_emp_2017)] <- rep(mean(full$sl_emp_2017[!is.na(full$sl_emp_2017)]),length(full$sl_emp_2017[is.na(full$sl_emp_2017)]))
full$urban_pop_2017[is.na(full$urban_pop_2017)] <- rep(mean(full$urban_pop_2017[!is.na(full$urban_pop_2017)]),length(full$urban_pop_2017[is.na(full$urban_pop_2017)]))

# As we have qualitative data(four_regions,worldbankregion), we need to convert them into category covatriates 
unique(full$four_regions)
unique(full$worldbankregion)
## We set asia as reference category, four_regions1 = 1,four_regions2 = 0, four_regions3 = 0: europe;
##                                    four_regions1 = 0,four_regions2 = 1, four_regions3 = 0: africa;
##                                    four_regions1 = 0,four_regions2 = 0, four_regions3 = 1: americas;
##                                    four_regions1 = 0,four_regions2 = 0, four_regions3 = 0: aisa

## We set South Asia as reference category, worldbankregion1 = 1, worldbankregion2 = 0, worldbankregion3 = 0, worldbankregion4 = 0, worldbankregion5 = 0: Europe & Central Asia;
##                                          worldbankregion1 = 0, worldbankregion2 = 1, worldbankregion3 = 0, worldbankregion4 = 0, worldbankregion5 = 0: Middle East & North Africa;
##                                          worldbankregion1 = 0, worldbankregion2 = 0, worldbankregion3 = 1, worldbankregion4 = 0, worldbankregion5 = 0: Sub-Saharan Africa;
##                                          worldbankregion1 = 0, worldbankregion2 = 0, worldbankregion3 = 0, worldbankregion4 = 1, worldbankregion5 = 0: Latin America & Caribbean;
##                                          worldbankregion1 = 0, worldbankregion2 = 0, worldbankregion3 = 0, worldbankregion4 = 0, worldbankregion5 = 1: East Asia & Pacific;
##                                          worldbankregion1 = 0, worldbankregion2 = 0, worldbankregion3 = 0, worldbankregion4 = 0, worldbankregion5 = 0: South Asia;          





