#Piecewise SEM
rm(list = ls()) 

library(piecewiseSEM)

pointdata_init<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQHXBBah_VLNcQIUaouCXzWnb2G47yWduBZfnXOx4S61ySn-rGlQqY0TTjGZSB_5AUDTFD2yttfOD5t/pub?gid=99131913&single=true&output=csv")
pointdata <- pointdata_init |> # Remove rows with missing values
  na.omit() |>   # keep complete cases
  dplyr:: filter(woody>0, woody<20)

sum(is.na(pointdata))
colSums(is.na(pointdata))

psych::pairs.panels(pointdata,stars = T, ellipses = F)

# Model 1: woody predicted by burnfreq and rainfall
model_woody <- lm(woody ~  dist2river+cec +burnfreq+elevation, 
                  data = pointdata)
summary(model_woody)

#burning
ggplot(data=pointdata,aes(x=burnfreq,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)

#CEC
ggplot(data=pointdata,aes(x=cec,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              #              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T) 

#distance to river
ggplot(data=pointdata,aes(x=dist2river,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              #              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T) 

#elevation
ggplot(data=pointdata,aes(x=elevation,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              #              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T) 

# Model_burnfreq: burning frequency predicted by Core Protected Areas and Rainfall
model_burnfreq_init <- glm(burnfreq ~ CorProtAr + rainfall, 
                           family=poisson, 
                           data = pointdata)
# Calculate dispersion statistic
dispersion_stat <- summary(model_burnfreq_init)$deviance / summary(model_burnfreq_init)$df.residual
dispersion_stat
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
library(MASS)
model_burnfreq <- MASS::glm.nb(burnfreq ~ CorProtAr + rainfall, 
                               data = pointdata)
summary(model_burnfreq)

#burn/CorProtAr
ggplot(data=pointdata,aes(y=burnfreq,x=CorProtAr))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)

#burn/rainfall
ggplot(data=pointdata,aes(y=burnfreq,x=rainfall))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)

# model_cec: predicted by rainfall
model_cec <- lm(cec ~ rainfall + CorProtAr, 
                data = pointdata)
summary(model_cec)

#cec/rainfall
ggplot(data=pointdata,aes(y=cec,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
#cec/CorProtAr
ggplot(data=pointdata,aes(y=cec,x=CorProtAr))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)

# model_CorProtAra:  predicted by elevation
model_CorProtAr <-glm(CorProtAr~elevation,
                      family=binomial,
                      data=pointdata)
summary(model_CorProtAr)
#CorProtAr/elevation
ggplot(data=pointdata,aes(y=CorProtAr,x=elevation))+
  geom_jitter(height = 0.02) +
  geom_smooth(method="glm",
              method.args=list(family=binomial),
              formula= y~x,
              se=T)

# model_rainfall: rainfall predicted by elevation
model_rainfall <- lm(rainfall ~ elevation, 
                     data = pointdata)
summary(model_rainfall)

ggplot(data=pointdata,aes(y=rainfall,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)

# model_dist2river: distance to river predicted by elevation
model_dist2river <- lm(dist2river ~ elevation, 
                        data = pointdata)
summary(model_dist2river)

ggplot(data=pointdata,aes(y=dist2river,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)

# combine the figures
library(patchwork)
allplots<-p1+p2+p3+p4+p5+p6+p7+
  patchwork::plot_layout(ncol=3) +
  patchwork::plot_annotation(title="Relations in model 1")
allplots

####### Combine all models into a single piecewise SEM
psem_model <- piecewiseSEM::psem(model_woody,
                                 model_burnfreq,
                                 model_cec,
                                 model_CorProtAr,
                                 model_rainfall,
                                 model_dist2river)

# Summarize the SEM results
summary(psem_model, conserve = TRUE)

plot(psem_model)
