library(readxl)
library(tidyverse)
library(anytime)
library(eeptools)
library(glue)
library(viridis)

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
adm <- adm[, c("AN", "SEQ")]

# change column name
colnames(ipd)[colnames(ipd) == "DATETIME_ADMIT"] <- "DATE_SERV"

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
temp2 <- full_join(temp1, opd, by = c("PID", "SEQ", "DIAGCODE", "DIAGTYPE", "DATE_SERV"))

# import missingvalue_UPDATE file
df <- read.csv("~/Downloads/Telemed/43 แฟ้ม Telemed/Missingvalue_UPDATE.csv")

# change date format
df$DATE_SERV <- as.Date(df$DATE_SERV, format = "%d/%m/%Y")

df$PID <- as.character(df$PID)
df$SEQ <- as.character(df$SEQ)
df$DIAGTYPE <- as.character(df$DIAGTYPE)
# combine temp2 with Missingvalues_UPDATE
temp3 <- full_join(temp2, df, by = c("PID", "SEQ", "DIAGCODE", "DIAGTYPE", "DATE_SERV"))

# combine temp3 with service
temp4 <- full_join(temp3, service, by = c("PID", "SEQ", "DATE_SERV"))

which(is.na(temp4$TYPEIN.y) == T)
which(is.na(temp4$INSTYPE) == T)

# remove TYPEIN.x column
temp4$TYPEIN.x <- NULL

# change name of TYPEIN.y
colnames(temp4)[which(names(temp4) == "TYPEIN.y")] <- "TYPEIN"

# reset row names
rownames(temp4) <- NULL

# combine person to temp4
person$PID <- as.character(person$PID)
temp5 <- full_join(temp4, person, by = c("PID"))

# find NAs
temp5[rowSums(is.na(temp5)) > 0,]

# remove NAs -------------------------- will delete this line
temp5 <- na.omit(temp5)

data <- temp5

