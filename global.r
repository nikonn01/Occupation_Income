library(shiny)
library(shinyjs)
library(vcd)
library(MASS)
library(RColorBrewer)
library(datasets)
library(corrgram)
library(visdat)
library(forecast)
library(tidytext)
library(tidyverse)
library(janeaustenr)
library(stringr)
library(wordcloud2)
library(reshape2)
library(pls)
library(ggplot2)
library(devtools)
library(car)
library(lubridate)
library(DT)
library(eeptools)
# devtools::install_github("mtennekes/tabplot")
library(tabplot)  # WARNING:  This may not install as CRAN is withdrawing tabplot due to maintenance "issues." Uncomment line above.

library(visNetwork)
library(leaflet)
# http://leaflet-extras.github.io/leaflet-providers/preview/    <<<--- worth a look
# https://rstudio.github.io/leaflet/shiny.html
library(shinycssloaders)
library(corrplot)
library(dplyr)
library(labourR)
library(magrittr)
library(plumber)


lexicon <- "afinn"

#loading data for user control visualisation
WIP_Income <- read.delim("Income.txt", header = TRUE)
WIP_Occupation<-read.delim("Occupations.txt", header = TRUE)

#loading data for report
dat_Income <- read.delim("Income.txt", header = TRUE)
#dat_Income <- read.delim("C:\\Users\\user\\Desktop\\Stast NZ interview task\\Income.txt", header = TRUE)
dat_Occupation<-read.delim("Occupations.txt", header = TRUE)
#dat_Occupation <- read.delim("C:\\Users\\user\\Desktop\\Stast NZ interview task\\Occupations.txt", header = TRUE)

#Converting date columns to date
dat_Income<-dat_Income%>%mutate(Transaction_Date=as.Date(ymd(Transaction_Date)),Birth_Date=as.Date(ymd(Birth_Date)))
dat_Occupation<-dat_Occupation%>%mutate(Transaction_Date=as.Date(ymd(Transaction_Date)),Birth_Date=as.Date(ymd(Birth_Date)))

#1.Clean 'Occupations.txt' 
#The sex column contains '11' for males and '12' for females. Change this to '1' for males and '2' for females
dat_Occupation$Sex[dat_Occupation$Sex==11] <- 1
dat_Occupation$Sex[dat_Occupation$Sex==12] <- 2

#Remove the invalid occupations and replace with a null value
unique_occupation<-unique(dat_Occupation$Occupation) #define variable for unique occupation
Occcupation_exceptions<-dat_Occupation$Occupation[c(1:2)] #define variable for exceptions
dat_Occupation$Occupation[dat_Occupation$Occupation == Occcupation_exceptions] <- NA #replace occupation with NA if it is in the exception list
unique_occupation <- unique_occupation[c(-1,-2)] #remove exceptions from unique occupations


#2.Clean'Income.txt' 
#For Date of Transaction equal to 01/01/2015 remove any income values equal to 2,000,000
dat_Income$Income <- ifelse(dat_Income$Income==2000000 & dat_Income$Transaction_Date==as.Date('2015-01-01'), NA, dat_Income$Income)

#For Date of Transaction equal to 1/1/2017 remove any income values over 2,500,000
dat_Income$Income <- ifelse(dat_Income$Income>2500000 & dat_Income$Transaction_Date==as.Date('2017-01-01'), NA, dat_Income$Income)

#Create a new column called "Rounded Incomes" and populate it with income values rounded to the nearest 10,000
dat_Income$Rounded_Incomes<-round(dat_Income$Income, -4)

#As we changed the gender column in Occupation table we have to do the same for income table
dat_Income$Sex[dat_Income$Sex==11] <- 1
dat_Income$Sex[dat_Income$Sex==12] <- 2

#Create a new table that joins the two tables supplied 
#Create a new table that joins "Occupations" and "Income"

Occupation_Income<-merge(x = dat_Income, y = dat_Occupation, by = "ID", all = TRUE)


#Occupation_Income= dat_Income %>% full_join(dat_Occupation,by="ID")

Occupation_Income_Exception<-Occupation_Income[duplicated(Occupation_Income$ID), ] #creating exception table for values where date of birth is different
Occupation_Income<-Occupation_Income[!duplicated(Occupation_Income$ID), ]#creating a data set where date of birth is the same for both tables


#create Transaction_Date column
Occupation_Income$Transaction_Date <- ifelse(!is.na(Occupation_Income$Transaction_Date.x), 
                                             as.Date(ymd(Occupation_Income$Transaction_Date.x)), 
                                             as.Date(ymd(Occupation_Income$Transaction_Date.y)))

Occupation_Income$Transaction_Date <- as.Date(Occupation_Income$Transaction_Date, origin = "1970-01-01")

#create First_Name column
Occupation_Income$First_Name <- ifelse(!is.na(Occupation_Income$First_Name.x), 
                                             (Occupation_Income$First_Name.x), 
                                             (Occupation_Income$First_Name.y))
#create Last_Name column
Occupation_Income$Last_Name <- ifelse(!is.na(Occupation_Income$Last_Name.x), 
                                       (Occupation_Income$Last_Name.x), 
                                       (Occupation_Income$Last_Name.y))

#create Birth_Date column
Occupation_Income$Birth_Date <- ifelse(!is.na(Occupation_Income$Birth_Date.x), 
                                      (Occupation_Income$Birth_Date.x), 
                                      (Occupation_Income$Birth_Date.y))

Occupation_Income$Birth_Date <- as.Date(Occupation_Income$Birth_Date, origin = "1970-01-01")

#create gender column
Occupation_Income$Sex <- ifelse(!is.na(Occupation_Income$Sex.x), 
                                       (Occupation_Income$Sex.x), 
                                       (Occupation_Income$Sex.y))

master_data = subset(Occupation_Income, select = -c(Transaction_Date.x,
                                                    Transaction_Date.y,
                                                    First_Name.x,
                                                    First_Name.y,
                                                    Last_Name.x,
                                                    Last_Name.y,
                                                    Birth_Date.x,
                                                    Birth_Date.y,
                                                    Sex.x,
                                                    Sex.y) )
master_data$Age <- floor(age_calc(master_data$Birth_Date, units = "years")) #adding years column

#check mismatching
#Occupation_Income$First_Name.x[!(Occupation_Income$First_Name.x %in% Occupation_Income$First_Name.y)]

#declare variable master_discrete_col which contains qualitative columns
master_discrete_col<-master_data%>%select(4,6,7)
master_categor <- colnames(master_discrete_col)

#declare variable master_quant_cal which contains quantitative columns
master_quant_cal<-master_data%>%select(1:3,9, 10)
master_numer <- colnames(master_quant_cal)  

master_all<-colnames(master_data)




  
 