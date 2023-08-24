library(readxl)
library(tidyverse)
library(anytime)

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

# unique ID
n_distinct(data_tele$PID) # 351

# on average, patients visits
nrow(data_tele)/n_distinct(data_tele$PID) # 5.381766

# select only telemed (typein = 5)
data_tele <- data[data$TYPEIN == 5,]

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

# remove NAs
data_tele1 <- na.omit(data_tele) # 1802

# sort DIAGCODE
# create a new column for DIAGCODE
data_tele1$code <- NA

# rank the diseases
data.frame(count=sort(table(data_tele$DIAGCODE), decreasing=TRUE))

# 1        E789        499 Other disorders of purine and pyrimidine metabolism
# 2         I10        498 Essential (primary) hypertension
# 3        E119        255 Non-insulin-dependent diabetes mellitus type 2 at without complications
# 4        Z719         82 Counselling\, unspecified
# 5        Z133         69 Special screening examination for mental and behavioural disorders


