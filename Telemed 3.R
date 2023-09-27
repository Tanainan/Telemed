library(readxl)
library(tidyverse)
library(anytime)
library(eeptools)
library(glue)
library(viridis)
library(lubridate)

# import data
person <- data.frame(read_excel("~/Downloads/Telemed/43 แฟ้ม Telemed/PERSON.xlsx"))
service <- data.frame(read_excel("~/Downloads/Telemed/43 แฟ้ม Telemed/SERVICE.xlsx"))
ipd <- data.frame(read_excel("~/Downloads/Telemed/43 แฟ้ม Telemed/DIAGNOSIS_IPD.xlsx"))
opd <- data.frame(read_excel("~/Downloads/Telemed/43 แฟ้ม Telemed/DIAGNOSIS_OPD.xlsx"))
adm <- data.frame(read_excel("~/Downloads/Telemed/43 แฟ้ม Telemed/ADMISSION.xlsx"))

# remove duplicates
opd <- unique(opd)
ipd <- unique(ipd)

# change date format
opd$DATE_SERV <- anydate(opd$DATE_SERV)
service$DATE_SERV <- anydate(service$DATE_SERV)
ipd$DATETIME_ADMIT <- substr(ipd$DATETIME_ADMIT,1,8)
ipd$DATETIME_ADMIT <- anydate(ipd$DATETIME_ADMIT)

# select only columns being used for each data set
person <- person[, c("PID", "SEX", "BIRTH")]
opd <- opd[, c("PID", "SEQ", "DIAGCODE", "DIAGTYPE", "DATE_SERV")]
service <- service[, c("PID", "SEQ", "TYPEIN", "DATE_SERV", "INSTYPE")]
ipd <- ipd[, c("PID", "AN", "DATETIME_ADMIT", "DIAGCODE", "DIAGTYPE")]
adm <- adm[, c("AN", "SEQ", "TYPEIN")]

# change column name
colnames(ipd)[colnames(ipd) == "DATETIME_ADMIT"] <- "DATE_SERV"

# identify ipd or opd
ipd$type <- "IPD"
opd$type <- "OPD"

# import missingvalue_UPDATE file
df <- read.csv("~/Downloads/Telemed/43 แฟ้ม Telemed/Missingvalue_UPDATE.csv")

# change date format
df$DATE_SERV <- as.Date(df$DATE_SERV, format = "%d/%m/%Y")

# df is opd
df$type <- "OPD"

# get SEQ from admission file
ipd$AN <- as.character(ipd$AN)
temp1 <- full_join(ipd, adm, by = c("AN"))

# remove AN column
temp1$AN <- NULL

# find NAs
temp1[rowSums(is.na(temp1)) > 0,]

# fill out missing value
temp1[721:723, c("SEQ")] <- "3405600037"

# combine opd and ipd
temp1$PID <- as.character(temp1$PID)
temp1$DIAGTYPE <- as.character(temp1$DIAGTYPE)

df$PID <- as.character(df$PID)
df$SEQ <- as.character(df$SEQ)
df$DIAGTYPE <- as.character(df$DIAGTYPE)
df$TYPEIN <- as.character(df$TYPEIN)
temp2 <- full_join(opd, df, by = c("PID", "SEQ", "DIAGCODE", "DIAGTYPE", "DATE_SERV", "type"))

temp3 <- full_join(temp2, temp1, by = c("PID", "SEQ", "DIAGCODE", "DIAGTYPE",
                                        "type", "DATE_SERV", "TYPEIN"))

temp4 <- full_join(temp3, service, by = c("PID", "SEQ", "DATE_SERV"))

# remove TYPEIN.x
temp4$TYPEIN.x <- NULL

# change colname
colnames(temp4)[which(names(temp4) == "TYPEIN.y")] <- "TYPEIN"

temp4[rowSums(is.na(temp4)) > 0,]

# reset row names
rownames(temp4) <- NULL

# combine person to temp4
person$PID <- as.character(person$PID)
temp5 <- full_join(temp4, person, by = c("PID"))

# find NAs
temp5[rowSums(is.na(temp5)) > 0,]

data <- temp5

