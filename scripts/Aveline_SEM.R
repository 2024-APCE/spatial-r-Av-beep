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
names(SEMdatastd)
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEMdata %>% select(dist2river, elevation, CorProtAr, rainfall, cec, burnfreq, hills, woody),
                    stars = T, ellipses = F)
psych::pairs.panels(SEMdatastd %>% select(dist2river, elevation, CorProtAr, rainfall, cec, burnfreq, hills, woody),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
mulreg_std <- lm(woody ~ dist2river+elevation+CorProtAr+rainfall+cec+burnfreq+hills, data = SEMdatastd)

summary(mulreg_std)

#make a model
woody_model <- 'woody~dist2river+cec+rainfall+burnfreq+elevation
                rainfall~hills+elevation
                burnfreq~elevation+CorProtAr+rainfall
                dist2river~hills+rainfall
                cec~dist2river+rainfall+burnfreq
                elevation~hills'
woody_model

woody_fit <- lavaan::sem(woody_model, data = SEMdatastd)
summary(woody_fit, standardized = T, fit.measures = T, rsquare = T)

#another try
woody_model1 <- 'woody~burnfreq+cec+hills
                burnfreq~CorProtAr+rainfall
                cec~dist2river+rainfall
                rainfall~elevation
                dist2river~elevation'
woody_model1

#CorProtAr could have an impact on cec, because those areas area are fertile
#low rain means higher nutrients, more rain means it washes away nutrients

#burn --> poisson
#CorProtAr --> binomial


woody_fit1 <- lavaan::sem(woody_model1, data = SEMdatastd)
summary(woody_fit1, standardized = T, fit.measures = T, rsquare = T)

# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

###

# Install and load required package
if (!require(DiagrammeR)) install.packages("DiagrammeR")
library(DiagrammeR)

# Define the nodes and edges
graph <- create_graph() %>%
  add_node(label = "Distance to River", id = "dist2river") %>%
  add_node(label = "Elevation", id = "elevation") %>%
  add_node(label = "Protected Area", id = "CorProtAr") %>%
  add_node(label = "Rainfall", id = "rainfall") %>%
  add_node(label = "Soil CEC", id = "cec") %>%
  add_node(label = "Burn Frequency", id = "burnfreq") %>%
  add_node(label = "Hills", id = "hills") %>%
  add_node(label = "Woody Cover", id = "woody") %>%
  
  # Add edges based on relationships
  add_edge(from = "dist2river", to = "elevation") %>%
  add_edge(from = "elevation", to = "rainfall") %>%
  add_edge(from = "rainfall", to = "cec") %>%
  add_edge(from = "rainfall", to = "burnfreq") %>%
  add_edge(from = "burnfreq", to = "woody") %>%
  add_edge(from = "cec", to = "woody") %>%
  add_edge(from = "hills", to = "woody") %>%
  add_edge(from = "CorProtAr", to = "woody") %>%
  add_edge(from = "elevation", to = "woody")

# Visualize the causal web
render_graph(graph)



