library(readxl)
library(tidyverse)
library(anytime)
library(eeptools)
library(glue)
library(viridis)

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

# calculate age from BIRTH
date_today <- Sys.Date()
date_today

data$age <- NA
data$age <- age_calc(data$BIRTH, date_today, units = "years")

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

# which one?
names(which.max(table(data_tele$PID))) # 95623

# one visit
sum(table(data_tele$PID) == 1) # 45
# more than one visit but less than 4
sum(table(data_tele$PID) > 1 & table(data_tele$PID) < 4) # 91
# more than 4
sum(table(data_tele$PID) < 4) # 136

45/351*100 # 12.82%
91/351*100 # 25.93%
136/351*100 # 38.75%

mean(data_tele$age) # 59.81989
sd(data_tele$age) #14.44631

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

# rank the diseases
data.frame(count=sort(table(data_tele1$DIAGCODE), decreasing=TRUE))

# 1        E789        499 Other disorders of purine and pyrimidine metabolism
# 2         I10        498 Essential (primary) hypertension
# 3        E119        255 Non-insulin-dependent diabetes mellitus type 2 at without complications
# 4        Z719         82 Counselling\, unspecified
# 5        Z133         69 Special screening examination for mental and behavioural disorders


# separated by genders
table(subset(data_tele1, SEX == 1)$chapter) # male

# 3   4   5   9  10  11  13  14  18  21
# 3 167  51 120   1   4   7  13  20 107

table(subset(data_tele1, SEX == 2)$chapter) # female

# 1   3   4   5   9  10  11  13  14  18  19  20  21
# 1  10 620  51 384   6   1  10  16  68   1   1 140

# find out about the patient that has the most visits
subset(data_tele1, PID == "95623")$title %>% table()

# find out about the patient that has the most visits
subset(data_tele1, PID == "95623")$NHSO_policy_des %>% table()

# 6 diseases by genders
data_tele1 %>%
  group_by(NHSO_policy,NHSO_policy_des,SEX) %>%
  count()

# frequency of visits for the 6 diseases
table(data_tele1$NHSO_policy)

# visits by month
data_tele1 %>%
  group_by(year_month) %>%
  count()

policy <- data_tele1 %>%
  group_by(NHSO_policy,NHSO_policy_des,year_month) %>%
  count()

# visit freq based on month
table(data_tele1$year_month)


figure_policy1 <- policy %>%
  ggplot( aes(x=year_month, y=n, group=NHSO_policy_des, color=NHSO_policy_des,fill = NHSO_policy_des)) +
  geom_line(size = 1.5) +
  scale_color_viridis(discrete = TRUE) +
  geom_point(size = 3) +
  scale_color_manual(values=c("darkred", "darkblue", "darkgreen", "orange","purple","gold")) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 1)+
  theme(legend.position="none", plot.title = element_text(size=30)) +
  ggtitle("disease treand") +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1))

policy %>%
  ggplot( aes(x=year_month, y=n, group=NHSO_policy_des, color=NHSO_policy_des,fill = NHSO_policy_des)) +
  geom_col() +
  scale_color_viridis(discrete = TRUE) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 3)+
  theme(legend.position="none", plot.title = element_text(size=30)) +
  ggtitle("disease treand") +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1))

policy2  <- policy  %>%
  mutate(NHSO_policy_des_2= NHSO_policy_des)

figure_policy2  <-  policy2 %>%
  ggplot( aes(x=year_month, y=n)) +
  geom_line(data = policy2 , aes(group = NHSO_policy_des_2), color="darkblue", size=1.5, alpha=0.5) +
  geom_line(aes(color= NHSO_policy_des_2), color="#69b3a2", size=1.2 ) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  ggtitle("disease trend") +
  facet_wrap(~ NHSO_policy_des_2)

figure_policy1/figure_policy2

# age group - 6 disease groups
table(data_tele1$NHSO_policy_des,data_tele1$age_group)

month_visit<- data_tele1 %>%
  group_by(year_month) %>%
  count()

histogram <- ggplot(data = month_visit, mapping = aes(x = year_month,y = n ,fill = n)) +
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

histogram

###### compare with physical visits ######
# based on date
tele_vs_phy_date <- ggplot(data = data, aes(x = year_month, fill = TYPEIN)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Referral", "Telemed"))

tele_vs_phy_date

# based on diseases ICD10
data1 <- na.omit(data)

data1$chapter <- as.numeric(data1$chapter)

tele_vs_phy_disease <- ggplot(data = data1, aes(x = as.factor(chapter), fill = TYPEIN)) +
  geom_bar() +
  theme_minimal() +
  xlab("Chapter") +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Referral", "Telemed"))
# geom_text(stat= "count",aes(label = ..count..), vjust = -1)
tele_vs_phy_disease

# based on sex and age
data1_male <- subset(data1, SEX == 1)
ggplot(data = data1_male, aes(x = age_des, fill = TYPEIN)) +
  geom_bar() +
  theme_minimal() +
  xlab("Age Group") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Referral", "Telemed"))

data1_female <- subset(data1, SEX == 2)
ggplot(data = data1_female, aes(x = age_des, fill = TYPEIN)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  xlab("Age Group") +
  scale_fill_discrete(name = "Type", label = c("Physical Visit", "Referral", "Telemed"))

# based on NHSO diseases



