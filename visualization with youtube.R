view(pol_data)

library(tidyverse)
library(openxlsx)
library(reshape2)
library(corrplot)
library(vegan)
library(openintro)
library(dplyr)
library(ggplot2)
library(e1071)
library(ggpubr)
library(moments)
library(WRS2)
library(ggthemes)
library(scales)
library(lme4)
library(lmerTest)
library(MuMIn)
library(emdbook)
library(lattice)
pol_data <- read.xlsx("pollinator observation table.xlsx", sheet = 2)
pol_data%>%
ggplot(aes(x=rich_flo_2,
y=pol_density))+
  geom_point(aes(color=pla_spe))+
  geom_smooth()
view(pol_data)
pol_data%>%
ggplot(aes(x=rich_flow_10,
           y=pol_density))+
  geom_point(aes(color=pla_spe))+
  geom_smooth()


pol_data%>%
  ggplot(aes(x=temp,
             y=pol_density))+
  geom_point(aes(color=pla_spe))+
  geom_smooth()

pol_data%>%
  ggplot(aes(x=rh,
             y=pol_density))+
  geom_point(aes(color=pla_spe))+
  geom_smooth()

pol_data%>%
  ggplot(aes(x=mean.height,
             y=pol_density))+
  geom_point(aes(color=pla_spe))+
  geom_smooth()

pol_data%>%
  ggplot(aes(x=doy,
             y=pol_density))+
  geom_point(aes(color=pla_spe))+
  geom_smooth()
####################################

hist(pol_data$pol_density, breaks = 10)
qqnorm(pol_data$pol_density)
qqline(pol_data$pol_density)
library(openintro)
library(dplyr)
library(ggplot2)
library(e1071)
hist(pol_data$pol_density, breaks = 10)
skewness(pol_data$pol_density)
#highly skewed data
pol_density2 <- log(pol_data$pol_density)
hist(pol_density2)
shapiro.test(pol_data$pol_density)
#p-value is so small= 3.777e-12
#therefore H0 rejected. data is not normally disturbed.

library(ggpubr)
ggqqplot(pol_data$pol_density)
#not normal disturbution at all
hist(pol_data$temp)]
ggqqplot(pol_data$temp)
#temprature is disturbutes normally
hist(pol_data$rich_flo_2)
ggqqplot(pol_data$rich_flo_2)
#flower richness 2 meter: normal disturbiution
hist(pol_data$rich_flow_10, breaks=5)
ggqqplot(pol_data$rich_flow_10)
#########
#NECTAR:

nectar_data <- read.xlsx("pollinator observation table.xlsx", sheet = 4)
view(nectar_data)
nectar_data%>%
  
  ggplot(aes(x=Flo_dur,
             y=nectar_field))+
  geom_point(aes(color=species))+
  geom_smooth()
  

nectar_data%>%
ggplot(aes(x=Flo_dur,
           y=sucrose_nectar_c1))+
  geom_point(aes(color=species))+
  geom_smooth()

nectar_data%>%
  ggplot(aes(x=Flo_dur,
             y=`sucrose_mg/Probe`))+
  geom_point(aes(color=species))+
  geom_smooth()

nectar_data%>%
  drop_na(nectar_field)  %>%
  ggplot(aes(x=doy,
             y=nectar_field))+
  geom_point(aes(color=species))+
  geom_smooth()


nectar_data%>%
  drop_na(sucrose_nectar_c1)  %>%
  drop_na(Flo_dur)%>%
  ggplot(aes(x=doy,
             y=sucrose_nectar_c1))+
  geom_point(aes(color=species))+
  geom_smooth()

nectar_data%>%
  drop_na(`sucrose_mg/Probe`)  %>%
  drop_na(Flo_dur)%>%
  ggplot(aes(x=doy,
             y=`sucrose_mg/Probe`))+
  geom_point(aes(color=species))+
  geom_smooth()


hist(nectar_data$nectar_field)
hist(nectar_data$sucrose_nectar_c1)
hist(nectar_data$`sucrose_mg/Probe`)

###All of the the nectar data that I have, Their distribution is not normal
#highly skewed, positive skewed
#I want to transform them to the normal distribution
#first I would like to follow every exact line of the guide so check the 
#distribution once again
library(ggpubr)
library(moments)
nectar_data <- read.xlsx("pollinator observation table.xlsx", sheet = 4)
ggdensity(nectar_data, x = "nectar_field", fill = "lightgray", title = "nectar in the field as quantity") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(nectar_data, x = "sucrose_nectar_c1", fill = "lightgray", title = "sucose concentration") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(nectar_data, x = "sucrose_mg/Probe", fill = "lightgray", title = "sucrose Mass") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
summary(nectar_data)


ggdensity(nectar_data, x = "Flo_dur", fill = "lightgray", title = "sucrose Mass") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")










summary(nectar_data)
skewness(nectar_data$nectar_field, na.rm = TRUE)
#3.541773
skewness(nectar_data$sucrose_nectar_c1, na.rm = TRUE)
#2.649432
skewness(nectar_data$`sucrose_mg/Probe`, na.rm = TRUE)
#2.537043 


#1/x transformation of the skewed data:

#nectar_data$nectar_field <- 1/(nectar_data$nectar_field)
#nectar_data$sucrose_nectar_c1 <- 1/(nectar_data$sucrose_nectar_c1)
#nectar_data$`sucrose_mg/Probe` <- log10(nectar_data$`sucrose_mg/Probe`)

#log transformation of data
nectar_data$nectar_field <- log10(nectar_data$nectar_field)
nectar_data$sucrose_nectar_c1 <- log10(nectar_data$sucrose_nectar_c1)
nectar_data$`sucrose_mg/Probe` <- log10(nectar_data$`sucrose_mg/Probe`)

# Distribution of Nectar in the field
ggdensity(nectar_data, x = "nectar_field", fill = "lightgray", title = "nectar in the field") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

# Distribution of nectar concentration
ggdensity(nectar_data, x = "sucrose_nectar_c1", fill = "lightgray", title = "nectar concentration") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

# Distribution of Nectar in the field
ggdensity(nectar_data, x = "sucrose_mg/Probe", fill = "lightgray", title = "Sucrose mass") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(nectar_data$nectar_field, na.rm = TRUE)
#NaN
skewness(nectar_data$sucrose_nectar_c1, na.rm = TRUE)
#NaN
skewness(nectar_data$`sucrose_mg/Probe`, na.rm = TRUE)
#0.6148799


drop_na(nectar_data, sucrose_nectar_c1)  %>%
drop_na(Flo_dur)%>%
  
ggplot(aes(x=Flo_dur, sucrose_nectar_c1, fill= species))+
  geom_boxplot()+
  
  theme_bw()
###################################
nectar_data%>%
drop_na(nectar_field)  %>%
  drop_na(Flo_dur)%>%
ggplot(aes(x=Flo_dur, nectar_field, fill= species))+
  geom_boxplot()+
  
  theme_bw()
########################
nectar_data%>%
  drop_na(`sucrose_mg/Probe`)  %>%
  drop_na(Flo_dur)%>%
  ggplot(aes(x=Flo_dur, `sucrose_mg/Probe`, fill= species))+
  geom_boxplot()+
  
  theme_bw()
#############################################

  plot(nectar_data$Flo_dur, nectar_data$nectar_field)
cor.test(nectar_data$Flo_dur, nectar_data$nectar_field, method = "pearson", alternative = "two.sided")
cor.test(nectar_data$Flo_dur, nectar_data$nectar_field, method = "spearman", alternative = "two.sided")
cor.test(nectar_data$Flo_dur, nectar_data$nectar_field, method = "kendall", alternative = "two.sided")
#for pearson and spearman the p-value is two big we have to accept the H0
#and it means there is no correlation between nectar in the field(quantity) and 
#flowering duration

plot(nectar_data$Flo_dur, nectar_data$sucrose_nectar_c1)
cor.test(nectar_data$Flo_dur, nectar_data$sucrose_nectar_c, method = "pearson", alternative = "two.sided")
cor.test(nectar_data$Flo_dur, nectar_data$sucrose_nectar_c, method = "spearman", alternative = "two.sided")
cor.test(nectar_data$Flo_dur, nectar_data$sucrose_nectar_c, method = "kendall", alternative = "two.sided")
# same as above, no corelation between suger concentration and flowring duration

plot(nectar_data$Flo_dur, nectar_data$`sucrose_mg/Probe`)
cor.test(nectar_data$Flo_dur, nectar_data$`sucrose_mg/Probe`, method = "pearson", alternative = "two.sided")
cor.test(nectar_data$Flo_dur, nectar_data$`sucrose_mg/Probe`, method = "spearman", alternative = "two.sided")
cor.test(nectar_data$Flo_dur, nectar_data$`sucrose_mg/Probe`, method = "kendall", alternative = "two.sided")

#p-value is 0.05914 for pearson, no corelation between sucrose mass and flowring duration
 library(WRS2)
attach(nectar_data)
pbcor(nectar_data$nectar_field,nectar_data$Flo_dur)
detach(nectar_data)
#p-value=0.64432. no correlation


attach(nectar_data)
pbcor(nectar_data$sucrose_nectar_c1,nectar_data$Flo_dur)
detach(nectar_data)
#p-value; 0.44257


attach(nectar_data)
pbcor(nectar_data$`sucrose_mg/Probe`,nectar_data$Flo_dur)
detach(nectar_data)
#same as above


library(lme4)
###lm modeling

model1 <- lm(nectar_data$nectar_field~ nectar_data$Flo_dur, data = nectar_data)
summary(model1)

model2 <- lmer (nectar_data$nectar_field~ nectar_data$Flo_dur +(1|species),data= nectar_data)
summary(model2)


model3 <- lmer (nectar_data$nectar_field~ nectar_data$Flo_dur +(nectar_data$Flo_dur|species),data= nectar_data)
summary(model3)
#####no model fitted
model4<- glm(nectar_data$nectar_field~ nectar_data$Flo_dur+ (1|nectar_data$species),family=poisson)
summary(model4)
###########################################
#Insect analyze#
pol_data <- read.xlsx("pollinator observation table.xlsx", sheet = 2)
insect_data <- read.xlsx("pollinator observation table.xlsx", sheet = 3)
view(insect_data)
view(pol_data)
head(insect_data)
drop_na(insect_data$pol_group)

ggplot(data = insect_data, aes(x= doy, fill= pol_group)) +
  geom_bar(stat = "count", position = "fill")+
  scale_y_continuous(labels = percent)+
  ggthemes::theme_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))+
  labs(title = "Flower visitors composition during flowering season")+
  labs(x= "Day of year", y= "Flower visitors perportion")
# +facet_wrap(~pla_spe)


##################################################
#flower visitors density
ggplot(data = insect_data, aes(x= doy, fill= pol_group)) +
  geom_bar(stat = "count", position = "fill")+
  scale_y_continuous(labels = percent)+
  ggthemes::theme_economist()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))+
  labs(title = "Flower visitors composition during flowering season")+
  labs(x= "Day of year", y= "Flower visitors perportion")
# +facet_wrap(~pla_spe)

pol_data%>%
  ggplot(aes(x=doy,
             y=pol_density))+
  geom_point(aes(color=pla_spe))+
  geom_smooth()+
  facet_wrap(~pla_spe)

pol_data%>%
  ggplot(aes(x=temp,
             y=pol_density))+
  geom_point(aes(color=pla_spe))+
  geom_smooth()+
  facet_wrap(~pla_spe)

pol_data%>%
  ggplot(aes(x=rich_flow_10,
             y=pol_density))+
  geom_point(aes(color=pla_spe))+
  geom_smooth()+
  facet_wrap(~pla_spe)

#######################################################################
#Normality of pol density]
ggdensity(pol_data, x = "pol_density", fill = "lightgray", title = "flower visitors") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
#normalization:
pol_data$pol_density

pol_data$pol_density_norm <- log10(pol_data$pol_density+1)

pol_data$pol_density_norm

ggdensity(pol_data, x = "pol_density", fill = "lightgray", title = "flower visitors") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
# Temprature:

ggdensity(pol_data, x = "temp", fill = "lightgray", title = "temprature") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
#correalation test for Density & Temp:

cor.test(pol_data$pol_density_norm, pol_data$temp, method = "pearson", alternative = "two.sided")

#for pearson the p-value is two big (0.29) we have to accept the H0
#and it means there is no correlation between number of flower visitors and Temprature

#correalation test for Density & flower rich -2:
pol_data$rich_flo_2
#normality of flower richness around 2m:
ggdensity(pol_data, x = "rich_flo_2", fill = "lightgray", title = "richness of flowers-2m") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
cor.test(pol_data$pol_density_norm, pol_data$rich_flo_2, method = "pearson", alternative = "two.sided")
#for pearson the p-value ispretty small (0.0009) we have to reject the H0
#and it means there is a correlation between number of flower visitors and flower richness-2m

#normality of flower richness around 10m:
pol_data$rich_flow_10


ggdensity(pol_data, x = "rich_flow_10", fill = "lightgray", title = "richness of flowers-10m") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

cor.test(pol_data$pol_density_norm, pol_data$rich_flow_10, method = "pearson", alternative = "two.sided")
#for pearson the p-value ispretty small (0.0004) we have to reject the H0
#and it means there is a correlation between number of flower visitors and flower richness-10m
##########################################
#RH:
ggdensity(pol_data, x = "rh", fill = "lightgray", title = "relative humidity") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
cor.test(pol_data$pol_density_norm, pol_data$rh, method = "pearson", alternative = "two.sided")
#for pearson the p-value is high (0.36) we have to accept the H0
#and it means there is a  no correlation between number of flower visitors and flower rh

#mean height:
ggdensity(pol_data, x = "mean.height", fill = "lightgray", title = "mean height") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
cor.test(pol_data$pol_density_norm, pol_data$mean.height, method = "pearson", alternative = "two.sided")
#for pearson the p-value is small (0.01) we have to reject the H0
#and it means there is a correlation between number of flower visitors and mean height

#coverage:
ggdensity(pol_data, x = "cover", fill = "lightgray", title = "cover of a patch") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
cor.test(pol_data$pol_density_norm, pol_data$cover, method = "pearson", alternative = "two.sided")
#for pearson the p-value is small (0.01) we have to reject the H0
#and it means there is a correlation between number of flower visitors and mean height





###############################################################
insect_data <- read.xlsx("pollinator observation table.xlsx", sheet = 3)
insect_data%>%
  #filter(pla_spe=="Rud_hir"|
           ##pla_spe=="Ech_sph"| 
    # pla_spe=="Eup_can"|
  #pla_spe=="Ach_mil"| 
  #pla_spe=="Sal_nem"| 
  #pla_spe=="Lav_ang"|
  #pla_spe=="Sec_var"| 
  # pla_spe=="Vic_cra")%>%
  ggplot(aes(x= doy, fill= pol_group)) +
  geom_bar(stat = "count", position = "fill")+
  scale_y_continuous(labels = percent)+

  theme(axis.text.x = element_text(angle = 90, hjust = 0))+
  labs(title = "Flower visitors composition during flowering season")+
  labs(x= "Day of year", y= "Flower visitors perportion")+
  facet_wrap(~pla_spe)
 

###############################################################################
pol_data <- read.xlsx("pollinator observation table.xlsx", sheet = 2)
pol_data%>%
#filter(pla_spe=="Rud_hir"|
         # pla_spe=="Ech_sph"| 
          #pla_spe=="Eup_can"|
          #pla_spe=="Ach_mil"| 
          #pla_spe=="Sal_nem"| 
          #pla_spe=="Lav_ang"|
          #pla_spe=="Sec_var"| 
         # pla_spe=="Vic_cra")%>%


  ggplot(aes(x=doy,
             y=pol_density))+
  geom_point(aes(color= pla_spe))+
  geom_line(aes(color= pla_spe))+
facet_wrap(~pla_spe)
################################################################
#Modeling for number of pollinators
names(pol_data)
summary(pol_data)
#pla_numeric <- as.numeric(pol_data$plant_species)
#pla_numeric
#pol_data$plant_id <- as.numeric(pol_data$plant_species)
 glm1 <- glmer(pol_density~ rich_flo_2* rich_flow_10, family = poisson, data = pol_data)
summary(glm1)
glm2 <- glmer(pol_density~ rich_flo_2* rich_flow_10 + (1|plant_species), family = poisson, data = pol_data)
summary(glm2)
