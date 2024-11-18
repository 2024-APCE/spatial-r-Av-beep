#https://docs.google.com/spreadsheets/d/e/2PACX-1vQHXBBah_VLNcQIUaouCXzWnb2G47yWduBZfnXOx4S61ySn-rGlQqY0TTjGZSB_5AUDTFD2yttfOD5t/pub?gid=99131913&single=true&output=csv

#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of Anderson 2007 dataset
# Paper:
# browseURL("https://drive.google.com/file/d/1aasI7uIFj7zcjuu0A3QhPDU4X-oQtcnx/view?usp=sharing")

# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
install.packages("lavaan")
library(lavaan)

# dataset:
# browseURL("https://docs.google.com/spreadsheets/d/1YPMq10k8PUEdWJN9VbP04FEBwD1infb6DjOHxsTNZ7A/edit?gid=99131913#gid=99131913")

# read the data from the google docs link:
SEMdata<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQHXBBah_VLNcQIUaouCXzWnb2G47yWduBZfnXOx4S61ySn-rGlQqY0TTjGZSB_5AUDTFD2yttfOD5t/pub?gid=99131913&single=true&output=csv") 

names(SEMdata)

 # standardize all variables to mean 0 and standard deviation 1
SEMdatastd <- SEMdata |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEMdatastd
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEMdata %>% select(dist2river, elevation, CorProtAr, rainfall, cec, burnfreq, hills, woody),
                    stars = T, ellipses = F)
psych::pairs.panels(SEMdatastd %>% select(dist2river, elevation, CorProtAr, rainfall, cec, burnfreq, hills, woody),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
mulreg_std <- lm(woody ~ dist2river+elevation+CorProtAr+rainfall+cec+burnfreq+hills, data = SEMdatastd)

summary(mulreg_std)

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
leaf_N_model <- 'LF_N~BIOMASS+RES_LHU+FIRE_FRQ+NMS
                BIOMASS~FIRE_FRQ+RES_LHU
                NMS~FIRE_FRQ+RES_LHU'
leaf_N_model

Leaf_N_fit <- lavaan::sem(leaf_N_model, data = Anderson2007std)

woody_model <- 'woody~dist2river+cec+rainfall+burnfreq+elevation
                rainfall~hills+elevation
                burnfreq~elevation+CorProtAr+rainfall
                dist2river~hills+rainfall
                cec~dist2river+rainfall+burnfreq
                elevation~hills'
woody_model

woody_fit <- lavaan::sem(woody_model, data = SEMdatastd)
summary(woody_fit, standardized = T, fit.measures = T, rsquare = T)

# show the model results
summary(Leaf_N_fit, standardized = T, fit.measures = T, rsquare = T)

# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

<<<<<<< HEAD
# visualise the model
=======
  >>>>>>> 8a237fe2317acaad42b557f15ab08d729405ba65

# also explore the models as shown in fig 5b and 5c of the Anderson2007 paper
# so repeat the model for leaf P content
leaf_p_model <- 'LF_P~BIOMASS+RES_LHU+FIRE_FRQ+NMS
                BIOMASS~FIRE_FRQ+RES_LHU
                NMS~FIRE_FRQ+RES_LHU'
leaf_p_model

Leaf_p_fit <- lavaan::sem(leaf_p_model, data = Anderson2007std)

# show the model results
summary(Leaf_p_fit, standardized = T, fit.measures = T, rsquare = T)