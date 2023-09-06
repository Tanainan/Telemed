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

# find duplicate
duplicated(person) %>% sum # none is duplicated
duplicated(opd) %>% sum() # 4 is duplicated
duplicated(service) %>% sum() # none is duplicated
duplicated(ipd) %>% sum() # 1 is duplicated
duplicated(adm) %>% sum() # none is duplicated

which(duplicated(opd) == T)
which(duplicated(ipd) == T)

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
service <- service[, c("PID", "SEQ", "TYPEIN", "DATE_SERV")]
ipd <- ipd[, c("PID", "AN", "DATETIME_ADMIT", "DIAGCODE", "DIAGTYPE")]

# change column name
colnames(ipd)[colnames(ipd) == "DATETIME_ADMIT"] <- "DATE_SERV"

ipd$SEQ <- NA
ipd$TYPEIN <- NA
# merge admission and ipd
for (i in 1:nrow(ipd)){
  for (j in 1:nrow(adm)){
    if (adm[j, c("AN")] == ipd[i, c("AN")]){
      ipd[i, c("SEQ", "TYPEIN")] <- adm[j, c("SEQ", "TYPEIN")]
    }
  }
}


# fill out the missing value
rownames(ipd) <- NULL
which(is.na(ipd$SEQ))
ipd[721:723, c("SEQ")] <- 3405600037
ipd[721:723, c("TYPEIN")] <- 1

# remove column AN from ipd
ipd <- ipd[, !names(ipd) %in% c("AN")]

# ipd$SEQ <- as.character(ipd$SEQ)
ipd$PID <- as.character(ipd$PID)
ipd$DIAGTYPE <- as.character(ipd$DIAGTYPE)
# ipd$TYPEIN <- as.character(ipd$TYPEIN)

# merge service and opd
data <- full_join(opd, service, by = c("SEQ" = "SEQ", "PID" = "PID", "DATE_SERV" = "DATE_SERV"))

na_opd_service <- data[rowSums(is.na(data)) > 0,]
rownames(na_opd_service) <- NULL

# remove missing values from the data first
data <- na.omit(data)

# import missingvalue_UPDATE file
df <- read.csv("~/Downloads/Telemed/43 แฟ้ม Telemed/Missingvalue_UPDATE.csv")

# change date format
df$DATE_SERV <- as.Date(df$DATE_SERV, format = "%d/%m/%Y")

# combine ipd and missingvalue files
ipd <- rbind(ipd, df)

# search for duplicates
duplicated(ipd) %>% sum

# remove duplicates
ipd <- unique(ipd)

# merge ipd and na_opd_service
for (i in 1:nrow(ipd)){
  for (j in 1:nrow(na_opd_service)){
    if (na_opd_service[j, c("PID")] == ipd[i, c("PID")] &
        na_opd_service[j, c("SEQ")] == ipd[i, c("SEQ")] &
        na_opd_service[j, c("DATE_SERV")] == ipd[i, c("DATE_SERV")] &
        na_opd_service[j, c("TYPEIN")] == ipd[i, c("TYPEIN")]){
      na_opd_service[j, c("DIAGCODE", "DIAGTYPE")] <- ipd[i, c("DIAGCODE", "DIAGTYPE")]
    }
  }
}

# combine ipd with na_opd_service
ipd <- rbind(ipd, na_opd_service)

rownames(ipd) <- NULL

# search for duplicates
duplicated(ipd) %>% sum # 395

# remove duplicates
ipd <- unique(ipd)

# merge data and ipd
data <- full_join(data, ipd, by = c("SEQ" = "SEQ", "PID" = "PID", "DATE_SERV" = "DATE_SERV",
                                    "DIAGCODE" = "DIAGCODE", "DIAGTYPE" = "DIAGTYPE", "TYPEIN" = "TYPEIN"))

# check for NAs
data[rowSums(is.na(data)) > 0,]

which(is.na(data$DIAGCODE))
data[19608, c("DIAGCODE", "DIAGTYPE")] <- c("Z539", "1")
data[19610, c("DIAGCODE", "DIAGTYPE")] <- c("Z539", "1")

# copy row 19609
data <- data %>% slice(rep(19609,4)) %>%
  bind_rows(data)

data[1, c("DIAGCODE", "DIAGTYPE")] <- c("L089", "1")
data[2, c("DIAGCODE", "DIAGTYPE")] <- c("E119", "2")
data[3, c("DIAGCODE", "DIAGTYPE")] <- c("E789", "2")
data[4, c("DIAGCODE", "DIAGTYPE")] <- c("I10", "2")
data[19613, c("DIAGCODE", "DIAGTYPE")] <- c("N183", "2")



# df$SEQ <- as.character(df$SEQ)
# df$PID <- as.character(df$PID)
# df$DIAGTYPE <- as.character(df$DIAGTYPE)
# df$TYPEIN <- as.character(df$TYPEIN)
#
# # merge data and df
# data <- full_join(data, df, by = c("SEQ" = "SEQ", "PID" = "PID", "DATE_SERV" = "DATE_SERV",
#                                    "DIAGCODE" = "DIAGCODE", "DIAGTYPE" = "DIAGTYPE", "TYPEIN" = "TYPEIN"))
rownames(data) <- NULL



# which(complete.cases(data) == FALSE)


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

# check for NAs
data[rowSums(is.na(data)) > 0,]

# change date format for BIRTH
data$BIRTH <- anydate(data$BIRTH)

# calculate age from BIRTH
data$age <- NA
data$age <- age_calc(data$BIRTH, data$DATE_SERV, units = "years")

# month visit
data$year_month <- format(data$DATE_SERV, "%Y-%m")

# categorize ages into groups
data <- data %>%
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

data <- data %>%
  mutate(age_des = case_when(
    age_group == 1 ~ "0-4 y/o",
    age_group == 2 ~ "4-8 y/o",
    age_group == 3 ~ "8-12 y/o",
    age_group == 4 ~ "12-16 y/o",
    age_group == 5 ~ "16-20 y/o",
    age_group == 6 ~ "20-24 y/o",
    age_group == 7 ~ "24-28 y/o",
    age_group == 8 ~ "28-32 y/o",
    age_group == 9 ~ "32-36 y/o",
    age_group == 10 ~ "36-40 y/o",
    age_group == 11 ~ "40-44 y/o",
    age_group == 12 ~ "44-48 y/o",
    age_group == 13 ~ "48-52 y/o",
    age_group == 14 ~ "52-56 y/o",
    age_group == 15 ~ "56-60 y/o",
    age_group == 16 ~ "60-64 y/o",
    age_group == 17 ~ "64-68 y/o",
    age_group == 18 ~ "68-72 y/o",
    age_group == 19 ~ "72-76 y/o",
    age_group == 20 ~ "76-80 y/o",
    age_group == 21 ~ "80-84 y/o",
    age_group == 22 ~ "84-88 y/o",
    age_group == 23 ~ "88-92 y/o",
    age_group == 24 ~ "92-96 y/o",
    age_group == 25 ~ "96-100 y/o"))

data <- data %>%
  mutate(age_des = factor(age_des,
                          levels = c("0-4 y/o","4-8 y/o",
                                     "8-12 y/o","12-16 y/o",
                                     "16-20 y/o","20-24 y/o",
                                     "24-28 y/o","28-32 y/o",
                                     "32-36 y/o","36-40 y/o",
                                     "40-44 y/o","44-48 y/o",
                                     "48-52 y/o","52-56 y/o",
                                     "56-60 y/o","60-64 y/o",
                                     "64-68 y/o","68-72 y/o",
                                     "72-76 y/o","76-80 y/o",
                                     "80-84 y/o","84-88 y/o",
                                     "88-92 y/o","92-96 y/o",
                                     "96-100 y/o"),
                          ordered = TRUE))

# sort DIAGCODE
# create a new column for DIAGCODE - select only the first 3 letters
data <- data %>%
  mutate(block_char = substr(data$DIAGCODE,1,1),
         block_num = substr(data$DIAGCODE,2,3))

data$block_num <- as.numeric(data$block_num)
data <- data %>%
  mutate(chapter = case_when(
    block_char == "A" | block_char == "B" & block_num >=00 & block_num <= 99 ~ "1",
    block_char == "C" | block_char == "D" & block_num >=00 & block_num <= 48 ~ "2",
    block_char == "D" & block_num >=50 & block_num <= 89 ~ "3",
    block_char == "E" & block_num >=00 & block_num <= 90 ~ "4",
    block_char == "F" & block_num >=00 & block_num <= 99 ~ "5",
    block_char == "G" & block_num >=00 & block_num <= 99 ~ "6",
    block_char == "H" & block_num >=00 & block_num <= 59 ~ "7",
    block_char == "H" & block_num >=60 & block_num <= 95 ~ "8",
    block_char == "I" & block_num >=00 & block_num <= 99 ~ "9",
    block_char == "J" & block_num >=00 & block_num <= 99 ~ "10",
    block_char == "K" & block_num >=00 & block_num <= 93 ~ "11",
    block_char == "L" & block_num >=00 & block_num <= 99 ~ "12",
    block_char == "M" & block_num >=00 & block_num <= 99 ~ "13",
    block_char == "N" & block_num >=00 & block_num <= 99 ~ "14",
    block_char == "O" & block_num >=00 & block_num <= 99 ~ "15",
    block_char == "P" & block_num >=00 & block_num <= 96 ~ "16",
    block_char == "Q" & block_num >=00 & block_num <= 99 ~ "17",
    block_char == "R" & block_num >=00 & block_num <= 99 ~ "18",
    block_char == "S" | block_char == "T" & block_num >=00 & block_num <= 98 ~ "19",
    block_char == "V" | block_char == "W" | block_char == "X"| block_char == "Y" & block_num >=01 & block_num <= 98 ~ "20",
    block_char == "Z" & block_num >=00 & block_num <= 99 ~ "21",
    block_char == "U" | block_char == "T" & block_num >=00 & block_num <= 99 ~ "22",
  ))

data$chapter <- as.numeric(data$chapter)

check <- data %>%
  group_by(chapter)%>%
  count()

# check for NAs
data[rowSums(is.na(data)) > 0,]

data <- data %>%
  mutate(title = case_when(
    chapter == 1 ~ "Certain infectious and parasitic diseases",
    chapter == 2 ~ "Neoplasms",
    chapter == 3 ~ "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
    chapter == 4 ~ "Endocrine, nutritional and metabolic diseases",
    chapter == 5 ~ "Mental and behavioural disorders",
    chapter == 6 ~ "Diseases of the nervous system",
    chapter == 7 ~ "Diseases of the eye and adnexa",
    chapter == 8 ~ "Diseases of the ear and mastoid process",
    chapter == 9 ~ "Diseases of the circulatory system",
    chapter == 10 ~ "Diseases of the respiratory system",
    chapter == 11 ~ "Diseases of the digestive system",
    chapter == 12 ~ "Diseases of the skin and subcutaneous tissue",
    chapter == 13 ~ "Diseases of the musculoskeletal system and connective tissue",
    chapter == 14 ~ "Diseases of the genitourinary system",
    chapter == 15 ~ "Pregnancy, childbirth and the puerperium",
    chapter == 16 ~ "Certain conditions originating in the perinatal period",
    chapter == 17 ~ "Congenital malformations, deformations and chromosomal abnormalities",
    chapter == 18 ~ "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
    chapter == 19 ~ "Injury, poisoning and certain other consequences of external causes",
    chapter == 20 ~ "External causes of morbidity and mortality",
    chapter == 21 ~ "Factors influencing health status and contact with health services",
    chapter == 22 ~ "Codes for special purposes",
  ))

check <- data %>%
  group_by(title)%>%
  count() %>%
  arrange(desc(n))

# 22 Disease groups (NHSO policy for telemedicine)
#1. Hypertension	Disease gr. For burden of disease??? -> I10,I11,I119,I12,I129,I13,I131,I132,I139,I15,I151,I152,I158,I159
#2. DM	Disease gr. For burden of disease???		-> E10,E11,E12,E13,E14
#3. Mental health	 (Mental and behavioural disorders)	->	F00–F99
#4. Asthma	"Disease gr. For burden of disease??? -> J40,J41,J411,J418,J42,J43,J431,J432,J438,J439,J44,J441,J448,J449,J45,J451,J458,J459,J46,J47
#5. Cancers	 (Neoplasms)		-> C00–D48
#6. Other diseases			-> Other

data <- data %>%
  mutate(NHSO_policy= case_when(
    DIAGCODE %in% c("I10","I111","I112","I113","I114","I115","I116","I117","I118","I119","I121","I122","I123","I124","I125","I126","I127","I128","I129","I133","I134","I135","I136","I137","I138","I139","I14","I15","I150","I151","I152","I158","I159") ~ 1,
    DIAGCODE %in% c("E109","E119","E129","E139","E149","E100","E110","E120","E130","E140","E101","E111","E121","E131","E141","E102","E112","E122","E132","E142","E103","E113","E123","E133","E143","E104","E114","E124","E134","E144","E105","E115","E125","E135","E145","E106","E116","E126","E136","E146","E107","E117","E127","E137","E147","E118","E128","E138","E148")~ 2,
    chapter == 5 ~ 3,
    DIAGCODE %in% c("J45","J46","J450","J451","J452","J458","J459") ~ 4,
    chapter == 2 ~ 5 ,
    TRUE ~ 6
  ))

data %>%
  group_by(NHSO_policy) %>%
  count()

data <- data %>%
  mutate(NHSO_policy_des = case_when(
    NHSO_policy == 1 ~ "Hypertension",
    NHSO_policy == 2 ~ "Diabetes",
    NHSO_policy == 3 ~ "Mental health",
    NHSO_policy == 4 ~ "Asthma",
    NHSO_policy == 5 ~ "Cancers",
    NHSO_policy == 6 ~ "Other",
  ))

data %>%
  group_by(NHSO_policy,NHSO_policy_des) %>%
  count()

################################################
# typein
table(data$TYPEIN)

# 1     3     5
# 17554    23  2037
# total visits = 2037

# select only telemed (typein = 5)
data_tele <- data[data$TYPEIN == 5,]

# unique ID
n_distinct(data_tele$PID) # 351

# since the data is supposed to record patients who used telemedicine (not the whole patients in the hospital)
# we see which PID from the PERSON file do not have TYPEIN = 5 at all
data_tele_person <- data.frame(PID = unique(data_tele$PID))
missing_TYPEIN_5 <- data.frame(PID = setdiff(person$PID, data_tele_person$PID))

# we remove patients who never use telemedicine from the data file
data <- data[data$PID %in% intersect(person$PID, data_tele_person$PID), ]
# data11 <- data[data$PID %in% setdiff(person$PID, data_tele_person$PID), ]
# table(data11$PID)

# on average, patients visits
nrow(data_tele)/n_distinct(data_tele$PID) # 5.803419

# reset rownames
rownames(data_tele) <- NULL

# gender based on visits
table(data_tele$SEX)

# male = 538
# female = 1499

# pie(table(data_tele$SEX), c("Male", "Female"))

prop.table(table(data_tele$SEX))

# male = 0.26
# female = 0.74

# gender based on person
person <- person[person$PID %in% intersect(person$PID, data_tele_person$PID), ]

table(person$SEX)
#
# # male = 104
# # female = 247
#
prop.table(table(person$SEX))
# # male = 0.30
# # female = 0.70

# freq of visits
max(table(data_tele$PID))
# the most freq = 21

# which one?
names(which.max(table(data_tele$PID))) # 95623

# one visit
sum(table(data_tele$PID) == 1) # 45
# more than one visit but less than 2
sum(table(data_tele$PID) > 1 & table(data_tele$PID) <= 2) # 34
# more than 2 visit but less than 3
sum(table(data_tele$PID) > 2 & table(data_tele$PID) <= 3) # 55
# more than 3 visit but less than 4
sum(table(data_tele$PID) > 3 & table(data_tele$PID) <= 4) # 26
# more than 4 visit but less than 5
sum(table(data_tele$PID) > 4 & table(data_tele$PID) <= 5) # 15
# more than 5
sum(table(data_tele$PID) > 5) # 176

45/351*100 # 12.82%
34/351*100 # 9.69%
55/351*100 # 15.67%
26/351*100 # 7.41%
15/351*100 # 4.27%
176/351*100 # 50.14%

mean(data_tele$age) # 59.84993
sd(data_tele$age) # 14.30572
min(data_tele$age) # 9.18
max(data_tele$age) # 92.93

# age group of visit
table(data_tele$age_group)
# 10  11  12  13  14  15  16  17  18  19  20  21  22  23  24   3   6   7   8   9
# 61 101 138 124 197 258 269 225 143 179 110  66  43  20   2  28  10  17   6  40

table(subset(data_tele, SEX == 1)$age_group) # male
# 10 11 12 13 14 15 16 17 18 19 20 21 22 23  3  6  7  8  9
# 25 55 42 25 21 59 57 58 45 32  4  8 34  3 19  9 16  4 22

table(subset(data_tele, SEX == 2)$age_group) # female
# 10  11  12  13  14  15  16  17  18  19  20  21  22  23  24   3   6   7   8   9
# 36  46  96  99 176 199 212 167  98 147 106  58   9  17   2   9   1   1   2  18

data$age_group <- as.factor(data$age_group)
# histogram age
ggplot(data = data_tele, aes(x = as.factor(age_des), fill = as.factor(SEX))) +
  geom_bar(position = position_dodge(), width = 0.7) +
  theme_minimal() +
  xlab("Age Group") +
  scale_fill_discrete(name = "Gender", label = c("Male", "Female")) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# use data_tele1 for diseases identification
data_tele[rowSums(is.na(data_tele)) > 0,]

# rank the diseases
data.frame(count=sort(table(data_tele$DIAGCODE), decreasing=TRUE))

# 1        E789        564 Other disorders of purine and pyrimidine metabolism
# 2         I10        564 Essential (primary) hypertension
# 3        E119        280 Non-insulin-dependent diabetes mellitus type 2 at without complications
# 4        Z719         82 Counselling\, unspecified
# 5        Z133         69 Special screening examination for mental and behavioural disorders


table(data_tele$chapter)
table(data_tele$title)

# look closely at Endocrine
end <- data[data$title == "Endocrine, nutritional and metabolic diseases",]

# remove TYPEIN = 3
end <- end[end$TYPEIN != 3, ]

end %>%
  ggplot(aes(x = year_month, fill = TYPEIN)) +
  geom_bar(position = position_dodge(), width = 0.5) +
  theme_minimal() +
  ggtitle("Endocrine, nutritional and metabolic diseases") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Telemed"))

# look closely at Diseases of the circulatory system
cir <- data[data$title == "Diseases of the circulatory system",]

# remove TYPEIN = 3
cir <- cir[cir$TYPEIN != 3, ]

cir %>%
  ggplot(aes(x = year_month, fill = TYPEIN)) +
  geom_bar(position = position_dodge(), width = 0.5) +
  theme_minimal() +
  ggtitle("Diseases of the circulatory system ") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Telemed"))

# look closely at Factors influencing health status and contact with health services
fac <- data[data$title == "Factors influencing health status and contact with health services",]

# remove TYPEIN = 3
fac <- fac[fac$TYPEIN != 3, ]

fac %>%
  ggplot(aes(x = year_month, fill = TYPEIN)) +
  geom_bar(position = position_dodge(), width = 0.5) +
  theme_minimal() +
  ggtitle("Factors influencing health status and contact with health services") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Telemed"))

# look closely at Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified
symp <- data[data$title == "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",]

# remove TYPEIN = 3
symp <- symp[symp$TYPEIN != 3, ]

symp %>%
  ggplot(aes(x = year_month, fill = TYPEIN)) +
  geom_bar(position = position_dodge(), width = 0.5) +
  theme_minimal() +
  ggtitle("Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Telemed"))

# look closely at Mental and behavioural disorders
men <- data[data$title == "Mental and behavioural disorders",]

# remove TYPEIN = 3
men <- men[men$TYPEIN != 3, ]

men %>%
  ggplot(aes(x = year_month, fill = TYPEIN)) +
  geom_bar(position = position_dodge(), width = 0.5) +
  theme_minimal() +
  ggtitle("Mental and behavioural disorders") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Telemed"))

# separated by genders
table(subset(data_tele, SEX == 1)$chapter) # male
# 2   3   4   5   9  10  11  13  14  18  21
# 1   4 186  51 134   1   4   8  16  23 110

table(subset(data_tele, SEX == 2)$chapter) # female
# 1   3   4   5   6   7   9  10  11  13  14  18  19  20  21
# 1  13 710  53   1   3 439   6   1  11  27  79   1   1 153

# histogram SEX
ggplot(data = data_tele, aes(x = as.factor(chapter), fill = as.factor(SEX))) +
  geom_bar(position = position_dodge()) +
  theme_minimal() +
  xlab("ICD-10 Chapter") +
  scale_fill_discrete(name = "Gender", label = c("Male", "Female")) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1)

# find out about the patient that has the most visits
subset(data_tele, PID == "95623")$title %>% table()

# find out about the patient that has the most visits
subset(data_tele, PID == "95623")$NHSO_policy_des %>% table()

# 6 diseases by genders
data_tele %>%
  group_by(NHSO_policy,NHSO_policy_des,SEX) %>%
  count()

# frequency of visits for the 6 diseases
table(data_tele$NHSO_policy)

# visits by month
data_tele %>%
  group_by(year_month) %>%
  count()

policy <- data_tele %>%
  group_by(NHSO_policy,NHSO_policy_des,year_month) %>%
  count()

# visit freq based on month
table(data_tele$year_month)


policy %>%
  ggplot( aes(x=year_month, y=n, group=NHSO_policy_des, color=NHSO_policy_des,fill = NHSO_policy_des)) +
  geom_line(size = 1.5) +
  scale_color_viridis(discrete = TRUE) +
  geom_point(size = 3) +
  scale_color_manual(values=c("darkred", "darkblue", "darkgreen", "orange","purple","gold")) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 1) +
  theme(legend.position="none", plot.title = element_text(size=30)) +
  ggtitle("Disease Treands") +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1))

# look closely at Mental health
ment <- data[data$NHSO_policy_des == "Mental health" & data$year_month > "2022-04",]

# remove TYPEIN = 3
ment <- ment[ment$TYPEIN != 3, ]

ment %>%
  ggplot(aes(x = year_month, fill = TYPEIN)) +
  geom_bar(position = position_dodge()) +
  theme_minimal() +
  ggtitle("Mental Health") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Telemed"))

# look closely at Diabetes
dia <- data[data$NHSO_policy_des == "Diabetes" & data$year_month > "2022-04",]

# remove TYPEIN = 3
dia <- dia[dia$TYPEIN != 3, ]

dia %>%
  ggplot(aes(x = year_month, fill = TYPEIN)) +
  geom_bar(position = position_dodge()) +
  theme_minimal() +
  ggtitle("Diabetes") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Telemed"))

# look closely at Hypertension
hyper <- data[data$NHSO_policy_des == "Hypertension" & data$year_month > "2022-04",]

# remove TYPEIN = 3
hyper <- hyper[hyper$TYPEIN != 3, ]

hyper %>%
  ggplot(aes(x = year_month, fill = TYPEIN)) +
  geom_bar(position = position_dodge()) +
  theme_minimal() +
  ggtitle("Hypertension") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Telemed"))

# look closely at Other
Other <- data[data$NHSO_policy_des == "Other" & data$year_month > "2022-04",]

# remove TYPEIN = 3
Other <- Other[Other$TYPEIN != 3, ]

Other %>%
  ggplot(aes(x = year_month, fill = TYPEIN)) +
  geom_bar(position = position_dodge()) +
  theme_minimal() +
  ggtitle("Others") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Telemed"))


# age group - 6 disease groups
table(data_tele$NHSO_policy_des,data_tele$age_group)

month_visit<- data_tele %>%
  group_by(year_month) %>%
  count()

ggplot(data = month_visit, mapping = aes(x = year_month,y = n ,fill = n)) +
  geom_col() +
  theme_minimal() +
  xlab("Timeline") +
  scale_fill_gradient(low = "yellow", high = "red") +
  ylab("Visit") +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 3) +
  # scale_x_date(date_labels = "%b-%y", breaks = '1 month', expand = c(0.001, 5)) +
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  # geom_smooth(aes(group = year, color = year), method = "lm", se = FALSE) +
  scale_color_manual(values = c("#69b3a2", "#F47695", "#959359"))

###### compare with physical visits ######
# based on date
tele_vs_phy_date <- ggplot(data = data, aes(x = year_month, fill = TYPEIN)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Referral", "Telemed"))

tele_vs_phy_date

# based on diseases ICD10
data[rowSums(is.na(data)) > 0,]
# data1 <- na.omit(data)

data$chapter <- as.numeric(data$chapter)

tele_vs_phy_disease <- ggplot(data = data, aes(x = as.factor(chapter), fill = TYPEIN)) +
  geom_bar() +
  theme_minimal() +
  xlab("Chapter") +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Referral", "Telemed"))
# geom_text(stat= "count",aes(label = ..count..), vjust = -1)
tele_vs_phy_disease

# based on sex and age
data1_male <- subset(data, SEX == 1)
ggplot(data = data1_male, aes(x = age_des, fill = TYPEIN)) +
  geom_bar() +
  theme_minimal() +
  xlab("Age Group") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Referral", "Telemed"))

data1_female <- subset(data, SEX == 2)
ggplot(data = data1_female, aes(x = age_des, fill = TYPEIN)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  xlab("Age Group") +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Referral", "Telemed"))

# based on NHSO diseases
ggplot(data = data, aes(x = as.factor(NHSO_policy_des), fill = TYPEIN)) +
  geom_bar() +
  theme_minimal() +
  xlab("NHSO Policy") +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Referral", "Telemed")) +
  theme(axis.text = element_text(angle = 90, hjust = 1))




