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
    age >=0 & age <5 ~ "1",
    age >=5 & age <10 ~ "2",
    age >=10 & age <15 ~ "3",
    age >=15 & age <20~ "4",
    age >=20 & age <25~ "5",
    age >=25 & age <30~ "6",
    age >=30 & age <35~ "7",
    age >=35 & age <40~ "8",
    age >=40 & age <45~ "9",
    age >=45 & age <50~ "10",
    age >=50 & age <55~ "11",
    age >=55 & age <60~ "12",
    age >=60 & age <65~ "13",
    age >=65 & age <70~ "14",
    age >=70 & age <75~ "15",
    age >=75 & age <80~ "16",
    age >=80 & age <85~ "17",
    age >=85 & age <90~ "18",
    age >=90 & age <95~ "19",
    age >=95 & age <100~ "20"
  ))

data <- data %>%
  mutate(age_des = case_when(
    age_group == 1 ~ "0-4 y/o",
    age_group == 2 ~ "5-9 y/o",
    age_group == 3 ~ "10-14 y/o",
    age_group == 4 ~ "15-19 y/o",
    age_group == 5 ~ "20-24 y/o",
    age_group == 6 ~ "25-29 y/o",
    age_group == 7 ~ "30-34 y/o",
    age_group == 8 ~ "35-39 y/o",
    age_group == 9 ~ "40-44 y/o",
    age_group == 10 ~ "45-49 y/o",
    age_group == 11 ~ "50-54 y/o",
    age_group == 12 ~ "55-59 y/o",
    age_group == 13 ~ "60-64 y/o",
    age_group == 14 ~ "65-69 y/o",
    age_group == 15 ~ "70-74 y/o",
    age_group == 16 ~ "75-79 y/o",
    age_group == 17 ~ "80-84 y/o",
    age_group == 18 ~ "85-89 y/o",
    age_group == 19 ~ "90-94 y/o",
    age_group == 20 ~ "95-99 y/o"))

data <- data %>%
  mutate(age_des = factor(age_des,
                          levels = c("0-4 y/o","5-9 y/o",
                                     "10-14 y/o","15-19 y/o",
                                     "20-24 y/o","25-29 y/o",
                                     "30-34 y/o","35-39 y/o",
                                     "40-44 y/o","45-49 y/o",
                                     "50-54 y/o","55-59 y/o",
                                     "60-64 y/o","65-69 y/o",
                                     "70-74 y/o","75-79 y/o",
                                     "80-84 y/o","85-89 y/o",
                                     "90-94 y/o","95-99 y/o"),
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
# select only telemed (typein = 5)
data_tele <- data[data$TYPEIN == 5,]
data_tele2 <- data[data$TYPEIN == 5 & data$DIAGTYPE == 1 & data$DIAGCODE != "Z539",]

# total telemed = 758


# unique ID
n_distinct(data_tele2$PID) # 338

# since the data is supposed to record patients who used telemedicine (not the whole patients in the hospital)
# we see which PID from the PERSON file do not have TYPEIN = 5 at all
data_tele_person <- data.frame(PID = unique(data_tele2$PID))
missing_TYPEIN_5 <- data.frame(PID = setdiff(person$PID, data_tele_person$PID))

# we remove patients who never use telemedicine from the data file
data <- data[data$PID %in% intersect(person$PID, data_tele_person$PID), ]
# data11 <- data[data$PID %in% setdiff(person$PID, data_tele_person$PID), ]
# table(data11$PID)

# on average, patients visits
nrow(data_tele2)/n_distinct(data_tele2$PID) # 2.24

# SD
sd_tele <- data_tele2 %>% group_by(PID) %>%
  summarise(n = n())

sd(sd_tele$n) # 1.25

min(sd_tele$n)
max(sd_tele$n)

# reset rownames
rownames(data_tele2) <- NULL

# gender based on visits
table(data_tele2$SEX)

# male 192
# female 566

prop.table(table(data_tele2$SEX))*100


# gender based on person
person <- person[person$PID %in% intersect(person$PID, data_tele_person$PID), ]
person$BIRTH <- anydate(person$BIRTH)

table(person$SEX)
#
# # male = 99
# # female = 239
#
prop.table(table(person$SEX))*100

# age based on person (for the first telemed visit)
dt_age <- aggregate(age ~ PID, data_tele2, function(x) min(x)) # find minimum age for each PID
mean(dt_age$age)
sd(dt_age$age)
min(dt_age$age)
max(dt_age$age)

dt_sex <- data_tele2[, c("PID", "SEX")]
dt_age <- left_join(dt_age, dt_sex, by = c("PID"))

# remove duplicates
dt_age <- unique(dt_age)

dt_age <- dt_age %>%
  mutate(age_group = case_when(
    age >=0 & age <5 ~ "1",
    age >=5 & age <10 ~ "2",
    age >=10 & age <15 ~ "3",
    age >=15 & age <20~ "4",
    age >=20 & age <25~ "5",
    age >=25 & age <30~ "6",
    age >=30 & age <35~ "7",
    age >=35 & age <40~ "8",
    age >=40 & age <45~ "9",
    age >=45 & age <50~ "10",
    age >=50 & age <55~ "11",
    age >=55 & age <60~ "12",
    age >=60 & age <65~ "13",
    age >=65 & age <70~ "14",
    age >=70 & age <75~ "15",
    age >=75 & age <80~ "16",
    age >=80 & age <85~ "17",
    age >=85 & age <90~ "18",
    age >=90 & age <95~ "19",
    age >=95 & age <100~ "20"
  ))

dt_age <- dt_age %>%
  mutate(age_des = case_when(
    age_group == 1 ~ "0-4 y/o",
    age_group == 2 ~ "5-9 y/o",
    age_group == 3 ~ "10-14 y/o",
    age_group == 4 ~ "15-19 y/o",
    age_group == 5 ~ "20-24 y/o",
    age_group == 6 ~ "25-29 y/o",
    age_group == 7 ~ "30-34 y/o",
    age_group == 8 ~ "35-39 y/o",
    age_group == 9 ~ "40-44 y/o",
    age_group == 10 ~ "45-49 y/o",
    age_group == 11 ~ "50-54 y/o",
    age_group == 12 ~ "55-59 y/o",
    age_group == 13 ~ "60-64 y/o",
    age_group == 14 ~ "65-69 y/o",
    age_group == 15 ~ "70-74 y/o",
    age_group == 16 ~ "75-79 y/o",
    age_group == 17 ~ "80-84 y/o",
    age_group == 18 ~ "85-89 y/o",
    age_group == 19 ~ "90-94 y/o",
    age_group == 20 ~ "95-99 y/o"))

dt_age <- dt_age %>%
  mutate(age_des = factor(age_des,
                          levels = c("0-4 y/o","5-9 y/o",
                                     "10-14 y/o","15-19 y/o",
                                     "20-24 y/o","25-29 y/o",
                                     "30-34 y/o","35-39 y/o",
                                     "40-44 y/o","45-49 y/o",
                                     "50-54 y/o","55-59 y/o",
                                     "60-64 y/o","65-69 y/o",
                                     "70-74 y/o","75-79 y/o",
                                     "80-84 y/o","85-89 y/o",
                                     "90-94 y/o","95-99 y/o"),
                          ordered = TRUE))

table(subset(dt_age, SEX == 1)$age_group) # male
round(prop.table(table(subset(dt_age, SEX == 1)$age_group))*100, 2)

table(subset(dt_age, SEX == 2)$age_group) # female
round(prop.table(table(subset(dt_age, SEX == 2)$age_group))*100, 2)

# NHSO age group
dt_age <- dt_age %>%
  mutate(age_group_NHSO = case_when(
    age >=0 & age <=6 ~ "1",
    age >6 & age<= 25 ~ "2",
    age >25 & age<= 60 ~ "3",
    age >= 60 ~ "4"
  ))

table(dt_age$age_group_NHSO)
round(prop.table(table(dt_age$age_group_NHSO))*100,2)


# freq of visits
max(table(data_tele2$PID))
# the most freq = 8

# which one?
names(which.max(table(data_tele2$PID)))

# one visit
sum(table(data_tele2$PID) == 1)
# more than one visit but less than 2
sum(table(data_tele2$PID) > 1 & table(data_tele2$PID) <= 2)
# more than 2 visit but less than 3
sum(table(data_tele2$PID) > 2 & table(data_tele2$PID) <= 3)
# more than 3 visit but less than 4
sum(table(data_tele2$PID) > 3 & table(data_tele2$PID) <= 4)
# more than 4 visit but less than 5
sum(table(data_tele2$PID) > 4 & table(data_tele2$PID) <= 5)
# more than 5
sum(table(data_tele2$PID) > 5)

123/338*100
78/338*100
94/338*100
28/338*100
9/338*100
6/338*100

15/338*100


# age group by visit (not unique ID)
table(data_tele2$age_group)

table(subset(data_tele2, SEX == 1)$age_group) # male

table(subset(data_tele2, SEX == 2)$age_group) # female

data$age_group <- as.factor(data$age_group)
# histogram age by visit
ggplot(data = data_tele2, aes(x = as.factor(age_des), fill = as.factor(SEX))) +
  geom_bar(position = position_dodge(), width = 0.7) +
  theme_minimal() +
  xlab("Age Group") +
  scale_fill_discrete(name = "Gender", label = c("Male", "Female")) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# unique ID age by gender
ggplot(data = dt_age, aes(x = as.factor(age_des), fill = as.factor(SEX))) +
  geom_bar(position = position_dodge(), width = 0.7) +
  theme_minimal() +
  xlab("Age Group") +
  scale_fill_discrete(name = "Gender", label = c("Male", "Female")) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# use data_tele1 for diseases identification
data_tele2[rowSums(is.na(data_tele2)) > 0,]

# rank the diseases
data.frame(count=sort(table(data_tele2$DIAGCODE), decreasing=TRUE))

# 1         I10        310  Essential (primary) hypertension
# 2        E119        251  Non-insulin-dependent diabetes mellitus type 2 at without complications
# 3        F322         31  Severe depressive episode without psychotic symptoms
# 4        E789         27  Disorder of lipoprotein metabolism\,unspecified
# 5        Z518         15  Other specified medical care

data.frame(count=sort(prop.table(table(data_tele2$DIAGCODE)), decreasing=TRUE))

table(data_tele2$chapter)
table(data_tele2$title)

# separated by genders
table(subset(data_tele2, SEX == 1)$chapter) # male


table(subset(data_tele2, SEX == 2)$chapter) # female


# histogram SEX
ggplot(data = data_tele2, aes(x = as.factor(chapter), fill = as.factor(SEX))) +
  geom_bar(position = position_dodge()) +
  theme_minimal() +
  xlab("ICD-10 Chapter") +
  scale_fill_discrete(name = "Gender", label = c("Male", "Female")) +
  geom_text(aes(label = stat(count)), stat = "count", position = position_dodge(w = 0.75), vjust = -1)


# 6 diseases by genders
data_tele2 %>%
  group_by(NHSO_policy,NHSO_policy_des,SEX) %>%
  count()

# frequency of visits for the 6 diseases
table(data_tele2$NHSO_policy)

# visits by month
data_tele2 %>%
  group_by(year_month) %>%
  count()

# percent visit each month
prop.table(table(data_tele2$year_month))*100

policy <- data_tele2 %>%
  group_by(NHSO_policy,NHSO_policy_des,year_month) %>%
  count()

# visit freq based on month
table(data_tele2$year_month)
round(prop.table(table(data_tele2$year_month))*100,2)

policy %>%
  ggplot(aes(x=year_month, y=n, group=NHSO_policy_des, color=NHSO_policy_des,fill = NHSO_policy_des)) +
  geom_line(size = 1.5) +
  scale_color_viridis(discrete = TRUE) +
  geom_point(size = 3) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 1) +
  theme(legend.position="none", plot.title = element_text(size=30)) +
  ggtitle("Disease Treands") +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1))

# age group - 6 disease groups
table(data_tele2$NHSO_policy_des,data_tele2$age_group)

month_visit <- data_tele2 %>%
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

######## Payment ########################################
table(data_tele2$INSTYPE)

############# average cost ##################

price_opd <- data.frame(read_excel("~/Downloads/Telemed/43 แฟ้ม Telemed/CHARGE_OPD.xlsx"))
price_ipd <- data.frame(read_excel("~/Downloads/Telemed/43 แฟ้ม Telemed/CHARGE_IPD.xlsx"))
service2 <- service

# remove the first 0 in price_opd$INSTYPE
# service2$INSTYPE <- sub("^0+", "", service2$INSTYPE)
# price_ipd$INSTYPE <- sub("^0+", "", price_ipd$INSTYPE)
# table(p_opd$INSTYPE)
# table(p_ipd$INSTYPE)

# change column name
colnames(price_ipd)[colnames(price_ipd) == "DATETIME_ADMIT"] <- "DATE_SERV"

# change date format
price_ipd$DATE_SERV <- substr(price_ipd$DATE_SERV,1,8)
price_ipd$DATE_SERV <- anydate(price_ipd$DATE_SERV)
price_opd$DATE_SERV <- anydate(price_opd$DATE_SERV)

# get SEQ of IPD from admission
p_ipd <- full_join(price_ipd, adm, by = c("AN"))

# check for NAs
p_ipd[rowSums(is.na(p_ipd)) > 0,] %>% nrow()
# write.csv(a, "Missing in CHARGE_IPD.csv")

# get TYPEIN of opd from service
price_opd$SEQ <- as.character(price_opd$SEQ)
price_opd$PID <- as.character(price_opd$PID)
price_opd$DATE_SERV <- anydate(price_opd$DATE_SERV)

p_opd <- full_join(price_opd, service2, by = c("SEQ", "PID", "DATE_SERV"))

p_opd[rowSums(is.na(p_opd)) > 0,] %>% nrow()
#
# write.csv(b, "Missing in CHARGE_OPD.csv")

# select only columns needed
p_ipd <- p_ipd[, c("PID", "PRICE", "SEQ", "TYPEIN", "DATE_SERV", "INSTYPE", "PAYPRICE")]
colnames(p_opd)
p_opd <- p_opd[, c("PID", "PRICE", "SEQ", "TYPEIN", "DATE_SERV", "INSTYPE.y", "PAYPRICE")]

# rename
colnames(p_opd)[colnames(p_opd) == "INSTYPE.y"] <- "INSTYPE"

p_opd[rowSums(is.na(p_opd)) > 0,] %>% nrow()

# add type column
p_ipd$type <- "IPD"
p_opd$type <- "OPD"

# remove NAs ------------------- will delete this line
p_ipd <- na.omit(p_ipd)
p_opd <- na.omit(p_opd)

# combine both datasets
price <- rbind(p_ipd, p_opd)

# choose only telemed -- only OPD will appear
price_tele <- price[price$TYPEIN == 5,]

rownames(price_tele) <- NULL

mean(as.numeric(price_tele$PRICE)) # 295.4417
sd(as.numeric(price_tele$PRICE))
max(as.numeric(price_tele$PRICE))
min(as.numeric(price_tele$PRICE))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(as.numeric(price_tele$PRICE))


mean(as.numeric(price_tele$PAYPRICE)) # 0.02295285
sd(as.numeric(price_tele$PAYPRICE))
max(as.numeric(price_tele$PAYPRICE))
min(as.numeric(price_tele$PAYPRICE))
getmode(as.numeric(price_tele$PAYPRICE))


###### before and after opd and ipd ##################
# find the first date of telemed
pid <- data_tele2 %>% group_by(PID)
first_tele <- pid %>% summarise(min(DATE_SERV))

# data %>% summarize(max(DATE_SERV)) # 2023-07-31
# data %>% summarize(min(DATE_SERV)) # 2019-12-29
first_tele$last_day <- anytime("2023-07-31")

# change column name
colnames(first_tele)[colnames(first_tele) == "min(DATE_SERV)"] <- "tele_date"

# calculate numbers of days
first_tele$after_range <- age_calc(anydate(first_tele$tele_date), anydate(first_tele$last_day), units = "days")

first_tele$after_range <- parse_number(as.character(first_tele$after_range))

first_tele$first_day <- ymd(first_tele$tele_date) - days(first_tele$after_range)

# first_tele %>% summarize(min(before)) # 2021-03-05

first_tele$int_before <- interval(first_tele$first_day, first_tele$tele_date - 1)
first_tele$int_after <- interval(first_tele$tele_date + 1, first_tele$last_day)

# select only physical visit from the data
data_phy <- subset(data, TYPEIN == 1 & DIAGTYPE == 1)

# add back to the data
data_date <- full_join(data_phy, first_tele, by = c("PID"))

# separate opd and ipd
data_opd <- subset(data_date, type == "OPD")
data_ipd <- subset(data_date, type == "IPD")

# count how many unique patients
data_opd$PID %>% n_distinct
data_ipd$PID %>% n_distinct

# count how many physical visits for before and after telemed
a_opd <- data_opd %>%
  group_by(PID) %>%
  mutate(count_before = sum(as_date(DATE_SERV) %within% int_before)) %>%
  mutate(count_after = sum(as_date(DATE_SERV) %within% int_after))

a_ipd <- data_ipd %>%
  group_by(PID) %>%
  mutate(count_before = sum(as_date(DATE_SERV) %within% int_before)) %>%
  mutate(count_after = sum(as_date(DATE_SERV) %within% int_after))

b_opd <- a_opd %>%
  group_by(PID) %>%
  distinct(count_before, count_after)

b_ipd <- a_ipd %>%
  group_by(PID) %>%
  distinct(count_before, count_after)

# opd
mean(b_opd$count_before, na.rm = T)
sd(b_opd$count_before, na.rm = T)
max(b_opd$count_before, na.rm = T)
min(b_opd$count_before, na.rm = T)


mean(b_opd$count_after, na.rm = T)
sd(b_opd$count_after, na.rm = T)
max(b_opd$count_after, na.rm = T)
min(b_opd$count_after, na.rm = T)

# ipd
mean(b_ipd$count_before, na.rm = T)
sd(b_ipd$count_before, na.rm = T)
max(b_ipd$count_before, na.rm = T)
min(b_ipd$count_before, na.rm = T)

mean(b_ipd$count_after, na.rm = T)
sd(b_ipd$count_after, na.rm = T)
max(b_ipd$count_after, na.rm = T)
min(b_ipd$count_after, na.rm = T)

######## before and after opd and ipd all ########
all_visit <- data_phy %>% group_by(PID) %>%
  summarize(min(DATE_SERV))

all_visit$last_day <- anytime("2023-07-31")

# change column name
colnames(all_visit)[colnames(all_visit) == "min(DATE_SERV)"] <- "first_day"

first_tele2 <- first_tele[, c("PID", "tele_date")]
all_visit <- full_join(all_visit, first_tele2, by = c("PID"))

all_visit$int_before_all <- interval(all_visit$first_day, all_visit$tele_date - 1)
all_visit$int_after_all <- interval(all_visit$tele_date + 1, all_visit$last_day)

# add back to the data
data_date_all <- full_join(data_phy, all_visit, by = c("PID"))

# separate opd and ipd
data_opd_all <- subset(data_date_all, type == "OPD")
data_ipd_all <- subset(data_date_all, type == "IPD")

# count how many unique patients
data_opd_all$PID %>% n_distinct
data_ipd_all$PID %>% n_distinct

# count how many physical visits for before and after telemed
a_opd_all <- data_opd_all %>%
  group_by(PID) %>%
  mutate(count_before = sum(as_date(DATE_SERV) %within% int_before_all)) %>%
  mutate(count_after = sum(as_date(DATE_SERV) %within% int_after_all))

a_ipd_all <- data_ipd_all %>%
  group_by(PID) %>%
  mutate(count_before = sum(as_date(DATE_SERV) %within% int_before_all)) %>%
  mutate(count_after = sum(as_date(DATE_SERV) %within% int_after_all))

b_opd_all <- a_opd_all %>%
  group_by(PID) %>%
  distinct(count_before, count_after)

b_ipd_all <- a_ipd_all %>%
  group_by(PID) %>%
  distinct(count_before, count_after)

# opd
mean(b_opd_all$count_before, na.rm = T)
sd(b_opd_all$count_before, na.rm = T)
max(b_opd_all$count_before, na.rm = T)
min(b_opd_all$count_before, na.rm = T)


mean(b_opd_all$count_after, na.rm = T)
sd(b_opd_all$count_after, na.rm = T)
max(b_opd_all$count_after, na.rm = T)
min(b_opd_all$count_after, na.rm = T)

# ipd
mean(b_ipd_all$count_before, na.rm = T)
sd(b_ipd_all$count_before, na.rm = T)
max(b_ipd_all$count_before, na.rm = T)
min(b_ipd_all$count_before, na.rm = T)

mean(b_ipd_all$count_after, na.rm = T)
sd(b_ipd_all$count_after, na.rm = T)
max(b_ipd_all$count_after, na.rm = T)
min(b_ipd_all$count_after, na.rm = T)

##### one visit ######
dt <- as.data.frame(table(unique(data_tele2)$PID))
colnames(dt)[1] <- "PID"
# choose only PID one time
dt1 <- dt[dt$Freq == 1,]
rownames(dt1) <- NULL
dt2 <- left_join(dt1, data_tele2[, c("PID", "year_month", "DIAGCODE")], by = c("PID"))
table(dt2$year_month)
round(prop.table(table(dt2$year_month))*100,2)

table(dt2$DIAGCODE)

# rank the diseases
data.frame(count=sort(table(dt2$DIAGCODE), decreasing=TRUE))

####### follow-up 3 months #######
a_opd <- a_opd[, c("PID", "DATE_SERV", "tele_date", "last_day", "after_range", "first_day", "count_before", "count_after")]

b_opd_3 <- a_opd %>%
  group_by(PID) %>%
  distinct(count_before, count_after, .keep_all = T)

b_opd_3$trimester <- round(b_opd_3$after_range/90, 2)

# remove less than 90 days
c_opd_3 <- subset(b_opd_3, trimester >= 1)

mean(c_opd_3$count_before)
sd(c_opd_3$count_before)
min(c_opd_3$count_before)
max(c_opd_3$count_before)

mean(c_opd_3$count_after)
sd(c_opd_3$count_after)
min(c_opd_3$count_after)
max(c_opd_3$count_after)

# calculate pre and post 3 months
c_opd_3$pre <- round(c_opd_3$count_before / c_opd_3$trimester, 2)

c_opd_3$post <- round(c_opd_3$count_after / c_opd_3$trimester, 2)

# opd
mean(c_opd_3$pre)
sd(c_opd_3$pre)
min(c_opd_3$pre)
max(c_opd_3$pre)

mean(c_opd_3$post)
sd(c_opd_3$post)
min(c_opd_3$post)
max(c_opd_3$post)


table(data_tele2$year_month)

