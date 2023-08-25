library(readxl)
library(tidyverse)
library(anytime)
library(eeptools)
library(glue)

# import data
person <- data.frame(read_excel("~/Downloads/Telemed/43 แฟ้ม Telemed/PERSON.xlsx"))
service <- data.frame(read_excel("~/Downloads/Telemed/43 แฟ้ม Telemed/SERVICE.xlsx"))
#ipd <- data.frame(read_excel("~/Downloads/Telemed/43 แฟ้ม Telemed/DIAGNOSIS_IPD.xlsx"))
opd <- data.frame(read_excel("~/Downloads/Telemed/43 แฟ้ม Telemed/DIAGNOSIS_OPD.xlsx"))

# change date format
opd$DATE_SERV <- anydate(opd$DATE_SERV)
service$DATE_SERV <- anydate(service$DATE_SERV)

# select only columns being used for each data set
person <- person[, c("PID", "HN", "SEX", "BIRTH")]
opd <- opd[, c("PID", "SEQ", "DIAGCODE", "DIAGTYPE", "CLINIC", "DATE_SERV")]
service <- service[, c("PID", "SEQ", "TYPEIN", "DATE_SERV")]

# find duplicate
duplicated(person) %>% sum # none is duplicated
duplicated(opd) %>% sum() # 4 is duplicated
duplicated(service) %>% sum() # none is duplicated

which(duplicated(opd) == T)

# remove duplicates
opd <- unique(opd)

# merge service and opd
data <- full_join(opd, service, by = c("SEQ" = "SEQ", "PID" = "PID", "DATE_SERV" = "DATE_SERV"))

data$SEX <- NA
data$BIRTH <- NA
# merge data and person
for (i in 1:nrow(data)){
  for (j in 1:nrow(person)){
    if (person[j, c("PID")] == data[i, c("PID")]){
     data[i, c("SEX", "BIRTH")] <-  person[j, c("SEX", "BIRTH")]
    }
  }
}

# change date format for BIRTH
data$BIRTH <- anydate(data$BIRTH)

# typein
table(data$TYPEIN)

# 1     3      5 = telemed
# 16781    19  1889
# total visits = 1889

# select only telemed (typein = 5)
data_tele <- data[data$TYPEIN == 5,]

# unique ID
n_distinct(data_tele$PID) # 351

# on average, patients visits
nrow(data_tele)/n_distinct(data_tele$PID) # 5.381766


# reset rownames
rownames(data_tele) <- NULL

# gender
table(data_tele$SEX)

# male = 509
# female = 1380

prop.table(table(data_tele$SEX))

# male = 0.2694547
# female = 0.7305453

# freq of visits
max(table(data_tele$PID))
# the most freq = 21

# one visit
sum(table(data_tele$PID) == 1) # 45
# more than one visit but less than 4
sum(table(data_tele$PID) > 1 & table(data_tele$PID) < 4) # 91
# more than 4
sum(table(data_tele$PID) < 4) # 136

# calculate age from BIRTH
date_today <- Sys.Date()
date_today

data_tele$age <- NA
data_tele$age <- age_calc(data_tele$BIRTH, date_today, units = "years")

mean(data_tele$age) # 59.81989

# categorize ages into groups
data_tele <- data_tele %>%
  mutate(age_group = case_when(
    age >=0 & age <4 ~ "1",
    age >=4 & age <8 ~ "2",
    age >=8 & age <12 ~ "3",
    age >=12 & age <16~ "4",
    age >=16 & age <20~ "5",
    age >=20 & age <24~ "6",
    age >=24 & age <28~ "7",
    age >=28 & age <32~ "8",
    age >=32 & age <36~ "9",
    age >=36 & age <40~ "10",
    age >=40 & age <44~ "11",
    age >=44 & age <48~ "12",
    age >=48 & age <52~ "13",
    age >=52 & age <56~ "14",
    age >=56 & age <60~ "15",
    age >=60 & age <64~ "16",
    age >=64 & age <68~ "17",
    age >=68 & age <72~ "18",
    age >=72 & age <76~ "19",
    age >=76 & age <80~ "20",
    age >=80 & age <84~ "21",
    age >=84 & age <88~ "22",
    age >=88 & age <92~ "23",
    age >=92 & age <96~ "24",
    age >=96 & age <100~ "25"
  ))

# age group of visit
table(data_tele$age_group)
table(subset(data_tele, SEX == 1)$age_group) # male
# 10 11 12 13 14 15 16 17 18 19 20 21 22 23  3  7  8  9
# 21 58 27 40 15 53 52 55 39 36  3  9 22  9 19 25  1 25

table(subset(data_tele, SEX == 2)$age_group) # female
# 10  11  12  13  14  15  16  17  18  19  20  21  22  23  24   3   6   7   8   9
# 38  43  78 102 137 175 210 173  82 137  66  86   7  14   5   9   1   1   2  14

# use data_tele1 for diseases identification
# remove NAs
data_tele1 <- na.omit(data_tele) # 1802
data_tele1$code <- NA

# rank the diseases
data.frame(count=sort(table(data_tele1$DIAGCODE), decreasing=TRUE))

# 1        E789        499 Other disorders of purine and pyrimidine metabolism
# 2         I10        498 Essential (primary) hypertension
# 3        E119        255 Non-insulin-dependent diabetes mellitus type 2 at without complications
# 4        Z719         82 Counselling\, unspecified
# 5        Z133         69 Special screening examination for mental and behavioural disorders

# sort DIAGCODE
# create a new column for DIAGCODE
data_tele1$code <- substr(data_tele1$DIAGCODE, 1, 3)

# rank the diseases
data.frame(count=sort(table(data_tele1$code), decreasing=TRUE))
