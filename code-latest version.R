full <- read_excel("icadata.xlsx")
## likely useful covariates : agri_2016,  broadband_2016, mobile_2016,child_mort_2018,child_per_woman_2018,income_pp_2018 , inflation_2017
## internet_2016,self_employed_2018 sl_emp_2017, urban_pop_2017, four_regions,  worldbankregion,  pop_dens_2018
full <- full[,c("country","lifeexp2018","agri_2016","agri_2017","broadband_2016", "mobile_2016","child_mort_2018","child_per_woman_2018","income_pp_2018", "inflation_2017","internet_2016","oil_per_capita_2016","self_employed_2018","sl_emp_2017","urban_pop_2017","four_regions", "worldbankregion","working_hours_2017","primary_completion_2016", "pop_dens_2018")]

# As we have qualitative data(four_regions,worldbankregion), we need to convert them into category covatriates 
unique(full$four_regions)
unique(full$worldbankregion)
## We set asia as reference category, four_regions1 = 1,four_regions2 = 0, four_regions3 = 0: europe;
##                                    four_regions1 = 0,four_regions2 = 1, four_regions3 = 0: africa;
##                                    four_regions1 = 0,four_regions2 = 0, four_regions3 = 1: americas;
##                                    four_regions1 = 0,four_regions2 = 0, four_regions3 = 0: aisa

## We set South Asia as reference category, worldbankregion1 = 1, worldbankregion2 = 0, worldbankregion3 = 0, worldbankregion4 = 0, worldbankregion5 = 0, worldbankregion5 = 0: Europe & Central Asia;
##                                          worldbankregion1 = 0, worldbankregion2 = 1, worldbankregion3 = 0, worldbankregion4 = 0, worldbankregion5 = 0, worldbankregion5 = 0: Middle East & North Africa;
##                                          worldbankregion1 = 0, worldbankregion2 = 0, worldbankregion3 = 1, worldbankregion4 = 0, worldbankregion5 = 0, worldbankregion5 = 0: Sub-Saharan Africa;
##                                          worldbankregion1 = 0, worldbankregion2 = 0, worldbankregion3 = 0, worldbankregion4 = 1, worldbankregion5 = 0, worldbankregion5 = 0: Latin America & Caribbean;
##                                          worldbankregion1 = 0, worldbankregion2 = 0, worldbankregion3 = 0, worldbankregion4 = 0, worldbankregion5 = 1, worldbankregion5 = 0: East Asia & Pacific;
##                                          worldbankregion1 = 0, worldbankregion2 = 0, worldbankregion3 = 0, worldbankregion4 = 0, worldbankregion5 = 0, worldbankregion5 = 1: North America;
##                                          worldbankregion1 = 0, worldbankregion2 = 0, worldbankregion3 = 0, worldbankregion4 = 0, worldbankregion5 = 0, worldbankregion5 = 0: South Asia;          

full$four_regions1 <- rep(NA,length(full$country))
full$four_regions2 <- rep(NA,length(full$country))
full$four_regions3 <- rep(NA,length(full$country))
full$worldbankregion1 <- rep(NA,length(full$country))
full$worldbankregion2 <- rep(NA,length(full$country))
full$worldbankregion3 <- rep(NA,length(full$country))
full$worldbankregion4 <- rep(NA,length(full$country))
full$worldbankregion5 <- rep(NA,length(full$country))
full$worldbankregion6 <- rep(NA,length(full$country))

for (i in 1:length(full$country)){
    if (full$four_regions[i]=="europe"){
        full$four_regions1[i] <- 1
    }
    else if(full$four_regions[i]=="africa"){
        full$four_regions2[i] <- 1
    }
    else if(full$four_regions[i]=="americas"){
        full$four_regions3[i] <- 1
    }
    else if(is.na(full$four_regions[i])){
        full$four_regions1[i] <- NA
        full$four_regions2[i] <- NA
        full$four_regions3[i] <- NA
    }
}

for (i in 1:length(full$country)){
    if (full$four_regions[i]=="europe"){
        full$four_regions1[i] <- 1
        full$four_regions2[i] <- 0
        full$four_regions3[i] <- 0
    }
    else if(full$four_regions[i]=="africa"){
        full$four_regions2[i] <- 1
        full$four_regions1[i] <- 0
        full$four_regions3[i] <- 0
    }
    else if(full$four_regions[i]=="americas"){
        full$four_regions3[i] <- 1
        full$four_regions1[i] <- 0
        full$four_regions2[i] <- 0
    }
    else if(full$four_regions[i]== "asia"){
        full$four_regions1[i] <- 0
        full$four_regions2[i] <- 0
        full$four_regions3[i] <- 0
    }
}

for (i in 1:length(full$country)){
    if (is.na(full$worldbankregion[i])){
        next
    }
    else if (full$worldbankregion[i]=="Europe & Central Asia"){
        full$worldbankregion1[i] <- 1
        full$worldbankregion2[i] <- 0
        full$worldbankregion3[i] <- 0
        full$worldbankregion4[i] <- 0
        full$worldbankregion5[i] <- 0
        full$worldbankregion6[i] <- 0
    }
    else if(full$worldbankregion[i]=="Middle East & North Africa"){
        full$worldbankregion2[i] <- 1
        full$worldbankregion1[i] <- 0
        full$worldbankregion3[i] <- 0
        full$worldbankregion4[i] <- 0
        full$worldbankregion5[i] <- 0
        full$worldbankregion6[i] <- 0
    }
    else if(full$worldbankregion[i]=="Sub-Saharan Africa"){
        full$worldbankregion3[i] <- 1
        full$worldbankregion2[i] <- 0
        full$worldbankregion1[i] <- 0
        full$worldbankregion4[i] <- 0
        full$worldbankregion5[i] <- 0
        full$worldbankregion6[i] <- 0
    }
    else if(full$worldbankregion[i]=="Latin America & Caribbean"){
        full$worldbankregion4[i] <- 1
        full$worldbankregion2[i] <- 0
        full$worldbankregion3[i] <- 0
        full$worldbankregion1[i] <- 0
        full$worldbankregion5[i] <- 0
        full$worldbankregion6[i] <- 0
    }
    else if(full$worldbankregion[i]=="East Asia & Pacific"){
        full$worldbankregion5[i] <- 1
        full$worldbankregion2[i] <- 0
        full$worldbankregion3[i] <- 0
        full$worldbankregion4[i] <- 0
        full$worldbankregion1[i] <- 0
        full$worldbankregion6[i] <- 0
    }
    else if(full$worldbankregion[i]=="North America"){
        full$worldbankregion6[i] <- 1
        full$worldbankregion2[i] <- 0
        full$worldbankregion3[i] <- 0
        full$worldbankregion4[i] <- 0
        full$worldbankregion5[i] <- 0
        full$worldbankregion1[i] <- 0
    }
    else if(full$worldbankregion[i]== "South Asia"){
        full$worldbankregion1[i] <- 0
        full$worldbankregion2[i] <- 0
        full$worldbankregion3[i] <- 0
        full$worldbankregion4[i] <- 0
        full$worldbankregion5[i] <- 0
        full$worldbankregion6[i] <- 0
    }
}

## check for collinearity
a <- c("agri_2016","broadband_2016","mobile_2016","child_mort_2018","child_per_woman_2018","income_pp_2018","inflation_2017","internet_2016","self_employed_2018","urban_pop_2017", "pop_dens_2018")
for (i in 1:length(a)){
    for (j in 1: length(a)){
    plot(full[,c(a[i],a[j])])
    }
}
plot(full[,c("agri_2016","broadband_2016","mobile_2016","child_mort_2018","child_per_woman_2018","income_pp_2018","inflation_2017","internet_2016","self_employed_2018","urban_pop_2017", "pop_dens_2018")])
cor(na.omit(full[,c( "agri_2016","broadband_2016","mobile_2016","child_mort_2018","child_per_woman_2018","income_pp_2018","inflation_2017","internet_2016","self_employed_2018","urban_pop_2017", "pop_dens_2018")]))
## we find that agr_2016&agri_2017; sel_emp_2017&self_employed_2018 are highly correlated with each other
plot(full[,c("broadband_2016","child_mort_2018","income_pp_2018","urban_pop_2017","agri_2016")])


## Let's build the model
# Model without categorical covariates:
test_1_1 <- lm(full$lifeexp2018 ~ full$agri_2016  + full$broadband_2016 + full$mobile_2016 + full$child_mort_2018 + full$child_per_woman_2018 + full$income_pp_2018 + full$inflation_2017 + full$internet_2016 + full$self_employed_2018 + full$urban_pop_2017 + full$pop_dens_2018, data = full) 
summary(test_1_1)
vif(test_1_1)

test_1_2 <- lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017)
summary(test_1_2)
vif(test_1_2)

# Model with categorical covariates: 
test_2_1 <-  lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017)
vif(test_2_1)

test_2_1_1<- lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017+full$four_regions1+full$four_regions2+full$four_regions3)
anova(test_2_1_1,test_2_1)
vif(test_2_1_1) 

test_2_1_2 <- lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017+full$worldbankregion1 + full$worldbankregion2 + full$worldbankregion3 + full$worldbankregion4 + full$worldbankregion5 + full$worldbankregion6)
anova(test_2_1,test_2_1_2)
vif(test_2_1_2)# It proves that there's no obvious collinearity bwtween covaraiates in test_2_1_2

test_2_1_3 <- lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017+full$four_regions1+full$four_regions2+full$four_regions3+full$agri_2016+full$worldbankregion1 + full$worldbankregion2 + full$worldbankregion3 + full$worldbankregion4 + full$worldbankregion5 + full$worldbankregion6)
anova(test_2_1_1,test_2_1_3)
vif(test_2_1_3) # " there are aliased coefficients in the model " it shows that there's perfect collinearity existing between worldbankregion and four_regions 

# Model with interaction term
test_3_1 <- lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017+full$worldbankregion1 + full$worldbankregion2 + full$worldbankregion3 + full$worldbankregion4 + full$worldbankregion5 + full$worldbankregion6)

test_3_1_1 <- lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017+full$worldbankregion1 + full$worldbankregion2 + full$worldbankregion3 + full$worldbankregion4 + full$worldbankregion5 + full$worldbankregion6+ full$worldbankregion1*full$broadband_2016 + full$worldbankregion2*full$broadband_2016 + full$worldbankregion3*full$broadband_2016 + full$worldbankregion4*full$broadband_2016 + full$worldbankregion5*full$broadband_2016 + full$worldbankregion6*full$broadband_2016)
anova(test_3_1,test_3_1_1) #significant

test_3_1_2 <- lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017+full$worldbankregion1 + full$worldbankregion2 + full$worldbankregion3 + full$worldbankregion4 + full$worldbankregion5 + full$worldbankregion6+ full$worldbankregion1*full$child_mort_2018 + full$worldbankregion2*full$child_mort_2018 + full$worldbankregion3*full$child_mort_2018 + full$worldbankregion4*full$child_mort_2018 + full$worldbankregion5*full$child_mort_2018 + full$worldbankregion6*full$child_mort_2018)
anova(test_3_1,test_3_1_2) #sinificant

test_3_1_3 <- lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017+full$worldbankregion1 + full$worldbankregion2 + full$worldbankregion3 + full$worldbankregion4 + full$worldbankregion5 + full$worldbankregion6+ full$worldbankregion1*full$income_pp_2018 + full$worldbankregion2*full$income_pp_2018 + full$worldbankregion3*full$income_pp_2018 + full$worldbankregion4*full$income_pp_2018 + full$worldbankregion5*full$income_pp_2018 + full$worldbankregion6*full$income_pp_2018)
anova(test_3_1,test_3_1_3) #not significant

test_3_1_4 <- lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017+full$worldbankregion1 + full$worldbankregion2 + full$worldbankregion3 + full$worldbankregion4 + full$worldbankregion5 + full$worldbankregion6+ full$worldbankregion1*full$urban_pop_2017 + full$worldbankregion2*full$urban_pop_2017 + full$worldbankregion3*full$urban_pop_2017 + full$worldbankregion4*full$urban_pop_2017 + full$worldbankregion5*full$urban_pop_2017 + full$worldbankregion6*full$urban_pop_2017)
anova(test_3_1,test_3_1_4) #not significant

#combine broadband_2016 and child_mort
test_3_1_5 <- lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017+full$worldbankregion1 + full$worldbankregion2 + full$worldbankregion3 + full$worldbankregion4 + full$worldbankregion5 + full$worldbankregion6+ full$worldbankregion1*full$broadband_2016 + full$worldbankregion2*full$broadband_2016 + full$worldbankregion3*full$broadband_2016 + full$worldbankregion4*full$broadband_2016 + full$worldbankregion5*full$broadband_2016 + full$worldbankregion6*full$broadband_2016+ full$worldbankregion1*full$child_mort_2018 + full$worldbankregion2*full$child_mort_2018 + full$worldbankregion3*full$child_mort_2018 + full$worldbankregion4*full$child_mort_2018 + full$worldbankregion5*full$child_mort_2018 + full$worldbankregion6*full$child_mort_2018)
anova(test_3_1,test_3_1_5)

## Now we have two choices
# choice1: model without interaction terms
m1 <- lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017+full$worldbankregion1 + full$worldbankregion2 + full$worldbankregion3 + full$worldbankregion4 + full$worldbankregion5 + full$worldbankregion6)
summary(m1)
# choice2: model with interaction terms
m2 <- lm(full$lifeexp2018 ~ full$broadband_2016+full$child_mort_2018+full$income_pp_2018+full$urban_pop_2017+full$worldbankregion1 + full$worldbankregion2 + full$worldbankregion3 + full$worldbankregion4 + full$worldbankregion5 + full$worldbankregion6+ full$worldbankregion1*full$broadband_2016 + full$worldbankregion2*full$broadband_2016 + full$worldbankregion3*full$broadband_2016 + full$worldbankregion4*full$broadband_2016 + full$worldbankregion5*full$broadband_2016 + full$worldbankregion6*full$broadband_2016+ full$worldbankregion1*full$child_mort_2018 + full$worldbankregion2*full$child_mort_2018 + full$worldbankregion3*full$child_mort_2018 + full$worldbankregion4*full$child_mort_2018 + full$worldbankregion5*full$child_mort_2018 + full$worldbankregion6*full$child_mort_2018)
summary(m2)

## Model checking
##  agri_2016 & self_employed_2018; broadband_2016 & internet_2016 ; child_mort_2018 & child_per_woman_2018 & internet_2016 ;



