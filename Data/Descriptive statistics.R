#Required packages

library(dplyr)
library(haven)
library(xtable)
library(plm)
library(pglm)
library(broom)
library(knitr) 
library(tidyverse)
library(tidyr)
library(haven)
library(stargazer)
library(kableExtra)
library(knitr)
library(expss)

#Get the raw datasets

data2010_raw <- read_dta("peopleu.dta") #Sample size 22,179 observations
data2013_raw <- read_dta("peopleu2013.dta") #Sample size 20,574 observations
data2016_raw <- read_dta("peopleu2016.dta") #Sample size 19,298

##Data prep

data2010 <- data2010_raw %>%
  rename(id = llave_ID_lb, 
         wave = ola, 
         age = edad, 
         inedu = estudia, 
         educ_dad = educ_padre, 
         educ_mom = educ_madre, 
         gender = sexo, 
         income = vr_salario, 
         ocupation = ocupacion,
         dad_house = padre_vive,
         mom_house = madre_vive)

data2013 <- data2013_raw %>% 
  rename(id = llave_ID_lb, 
         wave = ola, 
         age = edad, 
         inedu = estudia, 
         educ_dad = educ_padre, 
         educ_mom = educ_madre, 
         gender = sexo, 
         income = vr_salario, 
         ocupation = ocupacion,
         dad_house = padre_vive,
         mom_house = madre_vive)

data2016 <- data2016_raw %>%
  rename(id = llave_ID_lb, 
         wave = ola, 
         age = edad, 
         inedu = estudia, 
         educ_dad = educ_padre, 
         educ_mom = educ_madre, 
         gender = sexo, 
         income = vr_salario, 
         ocupation = ocupacion,
         dad_house = padre_vive,
         mom_house = madre_vive)

#Recoding variable wave

data2010$wave <- 2010 
data2013$wave <- 2013 
data2016$wave <- 2016 

#Recoding variable inedu

data2010$inedu[data2010$inedu == 1] <- 0
data2013$inedu[data2013$inedu == 1] <- 0
data2016$inedu[data2016$inedu == 1] <- 0

data2010$inedu[data2010$inedu == 2] <- 1
data2013$inedu[data2013$inedu == 2] <- 1
data2016$inedu[data2016$inedu == 2] <- 1

#Recoding variable edu_mom and edu_dad

#Recoding variable edu_mom and edu_dad

data2010$educ_dad <- factor(data2010$educ_dad,
                            levels = c(1,2,3,4,5,6,7,8,9,98,99),
                            labels = c("Some years of primary school", 
                                       "All primary school",
                                       "Some years of secundary school",
                                       "All secondary school",
                                       "One or more years of technical educ",
                                       "University without degree",
                                       "Univeristy with degree",
                                       "Posgrad",
                                       "None",
                                       "Does not know",
                                       "No info"))

data2013$educ_dad <- factor(data2013$educ_dad,
                            levels = c(1,2,3,4,5,6,7,8,9,98,99),
                            labels = c("Some years of primary school", 
                                       "All primary school",
                                       "Some years of secundary school",
                                       "All secondary school",
                                       "One or more years of technical educ",
                                       "University without title",
                                       "Univeristy with title",
                                       "Posgrad",
                                       "None",
                                       "Does not know",
                                       "No info"))
data2016$educ_dad <- factor(data2016$educ_dad,
                            levels = c(1,2,3,4,5,6,7,8,9,98,99),
                            labels = c("Some years of primary school", 
                                       "All primary school",
                                       "Some years of secundary school",
                                       "All secondary school",
                                       "One or more years of technical educ",
                                       "University without title",
                                       "Univeristy with title",
                                       "Posgrad",
                                       "None",
                                       "Does not know",
                                       "No info"))

data2010$educ_mom <- factor(data2010$educ_mom,
                            levels = c(1,2,3,4,5,6,7,8,9,98,99),
                            labels = c("Some years of primary school", 
                                       "All primary school",
                                       "Some years of secundary school",
                                       "All secondary school",
                                       "One or more years of technical educ",
                                       "University without title",
                                       "Univeristy with title",
                                       "Posgrad",
                                       "None",
                                       "Does not know",
                                       "No info"))
data2013$educ_mom <- factor(data2013$educ_mom,
                            levels = c(1,2,3,4,5,6,7,8,9,98,99),
                            labels = c("Some years of primary school", 
                                       "All primary school",
                                       "Some years of secundary school",
                                       "All secondary school",
                                       "One or more years of technical educ",
                                       "University without title",
                                       "Univeristy with title",
                                       "Posgrad",
                                       "None",
                                       "Does not know",
                                       "No info"))

data2016$educ_mom <- factor(data2016$educ_mom,
                            levels = c(1,2,3,4,5,6,7,8,9,98,99),
                            labels = c("Some years of primary school", 
                                       "All primary school",
                                       "Some years of secundary school",
                                       "All secondary school",
                                       "One or more years of technical educ",
                                       "University without title",
                                       "Univeristy with title",
                                       "Posgrad",
                                       "None",
                                       "Does not know",
                                       "No info"))
#Recoding variable income

data2010$income[data2010$income < 877803] <- 1
data2010$income[data2010$income >= 877803] <- 2
data2010$income[data2010$income < 877803] <- 2
data2010$income[data2010$income >= 1755606] <- 3
data2010$income[data2010$income < 2633409]  <- 3            
data2010$income[data2010$income > 2633409]  <- 4                

data2010$income<- factor(data2010$income,
                         levels = c(1,2,3,4),
                         labels = c("Less than 1 minimum wage", 
                                    "From 1 to less than 2 minimum wages",
                                    "From 2 to less than 3 minimum wages",
                                    "More than 3 minimum wages"))

#Recoding variable ocupation

data2010$ocupation <- factor(data2010$ocupation,
                             levels = c(1,2,3,4,5,6,7,8,9,10,11),
                             labels = c("Salaried employee of a private company with indefinite-term contract",
                                        "Salaried employee of a private company with fixed-term contract",
                                        "Government employee with indefinite-term contract",
                                        "Government employee with fixed-term contract",
                                        "Laborer or day laborer",
                                        "Domestic worker",
                                        "Independent worker",
                                        "Employer",
                                        "Worker on own farm",
                                        "Unpaid family worker without",
                                        "Other"))  

#Converting variables double as factors

data2010$income <- as.factor(data2010$income)
data2013$income <- as.factor(data2013$income)
data2016$income <- as.factor(data2016$income)

data2010$dad_house <- as.factor(data2010$dad_house)
data2013$dad_house <- as.factor(data2013$dad_house)
data2016$dad_house <- as.factor(data2016$dad_house)

data2010$mom_house <- as.factor(data2010$mom_house)
data2013$mom_house <- as.factor(data2013$mom_house)
data2016$mom_house <- as.factor(data2016$mom_house)

##Getting the variables in each wave
#Dataset wave 1

#Select the variables of interest 
subset1_w1 <- subset(data2010, select = c("id", 
                                          "wave", 
                                          "age",
                                          "inedu",
                                          "gender",
                                          "educ_dad",
                                          "educ_mom",
                                          "dad_house",
                                          "mom_house"))

#Sample size 22,179

#Drop all NA´s from ID variable
subset2_w1 <- subset(subset1_w1, !is.na(id))
#Sample size 22,179

wave1 <- subset2_w1 #Sample size wave1 22.179

summary(wave1$age)

#Dataset wave 2

#Select variables of interest
subset1_w2 <- subset(data2013, select = c("id","wave",
                                          "age",
                                          "inedu",
                                          "gender",
                                          "educ_dad",
                                          "educ_mom",
                                          "dad_house",
                                          "mom_house"))

#Sample size 20,547

#Drop all NA´s from ID variable
subset2_w2 <- subset(subset1_w2, !is.na(id))
#Sample size 17,064

wave2 <- subset2_w2 #Sample size wave2 17,064

#Dataset wave 3

#Select variables of interest
subset1_w3 <- subset(data2016, select = c("id", 
                                          "wave", 
                                          "age", 
                                          "inedu",
                                          "gender",
                                          "educ_dad",
                                          "educ_mom",
                                          "dad_house",
                                          "mom_house"))


#Sample size 19,298

subset2_w3 <- subset(subset1_w3, !is.na(id))
#Sample size 14,944

wave3 <- subset2_w3 #Sample size wave3 14,944

##Getting the dataset with all variables

#Append the datasets

data2010_2016 <- rbind(wave1, wave2, wave3) #Sample size 54,187

#Drop all NA´s from inedu variable
subset1 <- subset(data2010_2016, !is.na(inedu))
#Sample size 41,198

#Subset the data to ages between 6 and 17(compulsory education in Colombia)
subset2 <- subset(subset1, age >= 6 & age <= 17)
#Sample size 10,358

data01 <- subset2
#Sample size data01 10,313


