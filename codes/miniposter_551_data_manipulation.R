# DATA 551 Project Milestone 2 ----

# Name: Bindu Joopally
# 25 Feb 2019



# This code contains: ----
# Data Preparation (to be fed into Tableau) for miniposter
# Some plots used in miniposter using ggplot




# Set working directory ----
# setwd("/Users/bindu/551_project")



# Load libraries ----
library(data.table)
library(ggplot2)
library(varhandle)
library(tidyverse)




# Load data ----
crime_dt <- data.table(read.csv("data/Crime_Data_from_2010_to_Present.csv"
                                , na.strings = c(""," ","NA")))

crime_dt_sample <- crime_dt[1:1000, ] # just in case for quick checks as data is somewhat large




# Data manipulation and some cleaning ----

# Check unique key
uniqueN(crime_dt$DR.Number) == nrow(crime_dt)

# Remove unneccesary variables
crime_dt[, `:=` (Area.ID = NULL)]
crime_dt[, `:=` (Crime.Code.2 = NULL
                 , Crime.Code.1 = NULL
                 , Crime.Code.3 = NULL
                 , Crime.Code.4 = NULL
                 , Address = NULL
                 , Cross.Street = NULL
                 # , Location = NULL
                 , Status.Code = NULL
                 , Status.Description = NULL
                 , Area.Name = NULL
                 , Reporting.District = NULL)]

crime_dt[, `:=` (MO.Codes = NULL
                 , Time.Occurred = NULL
                 , Date.Reported = NULL
                 , Premise.Code = NULL
                 , Premise.Description = NULL)]

names(crime_dt)





###################################################################
# Plot 1 ----
# What are the types of weapons frequently used for these crimes?

# How many
uniqueN(crime_dt$Weapon.Used.Code) #81
uniqueN(crime_dt$Weapon.Description) 

sum(is.na(crime_dt$Weapon.Description)) #1272646

# what are they
# unique(crime_dt$Weapon.Description)

# See top weapons
sort(table(crime_dt$Weapon.Description), decreasing = T)[1:5]

crime_dt[, Weapon.Description := as.character(Weapon.Description)]

# Remove the crimes that occured more than 1000 times and label them as others
t <- sort(table(crime_dt$Weapon.Description), decreasing = T) > 1000
length(sort(table(crime_dt$Weapon.Description), decreasing = T)[t]) # 10 weapons
top_weapons <- as.vector(names(sort(table(crime_dt$Weapon.Description), decreasing = T)[t]))

# create crime type variable
crime_dt[, weapon_type := ifelse(Weapon.Description %in% top_weapons
                                 , Weapon.Description
                                 , "OTHERS")]
# check
table(crime_dt$weapon_type)

# put all kinds of 'knife' in one category
crime_dt[grepl("KNIFE", weapon_type), weapon_type := "KNIFE"]
crime_dt[grepl("OTHER", weapon_type), weapon_type := NA]

df2 <- data.table(table(crime_dt$weapon_type))
df2 <- df2[order(N, decreasing = T),]

df2[, V1 := factor(V1, levels = V1[order(N)])]
df2 <- df2[!is.na(V1), ]

# Lets take only top 20  
p1<-ggplot(data=df2[1:19], aes(x=V1, y=N)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(x = "Weapon Type", y = "Number of Crimes")+
  coord_flip() + ggtitle("Weapons used in Crimes")
p1



###################################################################

# Who is most vulnerable?


# Plot 2 -----
### Gender ----
class(crime_dt$Victim.Sex)
crime_dt[, sex := as.character(Victim.Sex)]
sum(is.na(crime_dt$sex)) #lots

table(crime_dt$sex)

# Treat sex other than X, male and female as NA
crime_dt[!sex %in% c("F", "M")
         , sex := NA]

crime_dt[sex == "F", sex := "Female"]
crime_dt[sex == "M", sex := "Male"]


df <- data.table(table(crime_dt$sex))
# library(plotrix)
slices <- df$N
lbls <- df$V1
# pie3D(slices,labels=lbls,explode=0.1,
#       main="Gender of victims")

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls 
    , main="Gender of Victims"
    )





#######################################
# Plot 3 -----
### Age

table(crime_dt$Victim.Age)
summary(crime_dt$Victim.Age)

crime_dt[, age := ifelse(Victim.Age>90 | Victim.Age <1
                         , NA
                         , Victim.Age)]

summary(crime_dt$age)



crime_dt[, age := as.integer(age)]

df3 <- data.table(table(crime_dt$age))
df3[, V1:=as.numeric(V1)]

ggplot(data=df3, aes(x=V1, y=N)) + 
  geom_point(color = "violet") +
  # ggtitle("Age of Victims") +
  labs(x = "Age", y = "Number of crimes") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method = "loess")




#######################################
# Plot 4 ----
#### Descent 

class(crime_dt$Victim.Descent)
crime_dt[, descent := as.character(Victim.Descent)]
sum(is.na(crime_dt$Victim.Descent)) #lots

table(crime_dt$descent)

crime_dt[descent == "-", descent := NA]
crime_dt[descent == "X", descent := NA]

# check percentage of first three races
sum(sort(table(crime_dt$descent), decreasing = T)[1:3])/nrow(crime_dt[!is.na(descent),]) #86%

# From https://www.opendatanetwork.com/dataset/data.lacity.org/y8tr-7khq

crime_dt[descent == "A", descent := "Other Asian"]
crime_dt[descent == "B", descent := "Black"]
crime_dt[descent == "C", descent := "Chinese"]
crime_dt[descent == "D", descent := "Cambodian"]
crime_dt[descent == "F", descent := "Filipino"]
crime_dt[descent == "G", descent := "Guamanian"]
crime_dt[descent == "H", descent := "Hispanic/Latin/Mexican "]
crime_dt[descent == "I", descent := "American Indian/Alaskan Native "]
crime_dt[descent == "J", descent := "Japanese"]
crime_dt[descent == "K", descent := "Korean"]
crime_dt[descent == "L", descent := "Laotian"]
crime_dt[descent == "O", descent := "Other"]
crime_dt[descent == "P", descent := "Pacific Islander"]
crime_dt[descent == "S", descent := "Samoan"]
crime_dt[descent == "U", descent := "Hawaiian"]
crime_dt[descent == "V", descent := "Vietnamese"]
crime_dt[descent == "W", descent := "White"]

crime_dt[descent == "Z", descent := "Asian Indian"]


crime_dt[descent == "Other", descent := NA] # don't want 'other' to come up as main category

df4 <- data.table(table(crime_dt$descent))
df4 <- df4[order(N, decreasing = T),]

df4[, V1 := factor(V1, levels = V1[order(N, decreasing = T)])]


# Lets take only top 20  
p4<-ggplot(data=df4[1:8], aes(x=V1, y=N)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.7)+
  theme_minimal() +
  labs(x = "Victim Descent", y = "Number of crimes")+
  # ggtitle("Descent of Victims") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5), plot.title = element_text(hjust = 0.5))
p4





##############################################################
# Location - latitude and longitudes for tableau maps ----


location <- crime_dt$Location %>% # take coord as string
  str_replace_all("[()]", "") %>% # replace parantheses
  str_split_fixed(", ", n=2) %>% # split up based on comma and space after
  as.data.frame %>% # turn this to a data frame
  transmute(lat=V1, long=V2) # rename the variables 
crime_dt <- cbind(crime_dt, location)

names(crime_dt)


# Write data to CSV file - This will be input data in Tableau ----
write.csv(crime_dt, file = "crime_data.csv") # dropbox link provided in README file.


# Clean environment ----
rm(list = ls())
gc()
# END ----


