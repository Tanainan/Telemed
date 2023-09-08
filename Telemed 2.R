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

ipd$PID <- as.character(ipd$PID)
ipd$DIAGTYPE <- as.character(ipd$DIAGTYPE)

# check for NAs
ipd[rowSums(is.na(ipd)) > 0,] # no NA

colnames(ipd)
# "PID"       "DATE_SERV" "DIAGCODE"  "DIAGTYPE"  "SEQ"       "TYPEIN"

colnames(opd)
# "PID"       "SEQ"       "DIAGCODE"  "DIAGTYPE"  "DATE_SERV"

# merge ipd and opd
dt <- full_join(opd, ipd, by = c("SEQ" = "SEQ", "PID" = "PID", "DATE_SERV" = "DATE_SERV",
                                 "DIAGCODE" = "DIAGCODE", "DIAGTYPE" = "DIAGTYPE"))

which(duplicated(dt) == T)

colnames(df)
# "PID"       "SEQ"       "DIAGCODE"  "DIAGTYPE"  "DATE_SERV" "TYPEIN"

df$SEQ <- as.character(df$SEQ)
df$PID <- as.character(df$PID)
df$DIAGTYPE <- as.character(df$DIAGTYPE)
df$TYPEIN <- as.character(df$TYPEIN)
# merge with missing value update
dt2 <- full_join(dt, df, by = c("SEQ" = "SEQ", "PID" = "PID", "DATE_SERV" = "DATE_SERV",
                                "DIAGCODE" = "DIAGCODE", "DIAGTYPE" = "DIAGTYPE", "TYPEIN" = "TYPEIN"))

# search for duplicates
which(duplicated(dt2) == T)

colnames(service)
# "PID"       "SEQ"       "TYPEIN"    "DATE_SERV" "INSTYPE"

dt2$INSTYPE <- NA
# merge with service
# for (i in 1:nrow(dt2)){
#   for (j in 1:nrow(service)){
#     if (service[j, c("SEQ")] == dt2[i, c("SEQ")]){
#       dt2[i, c("TYPEIN", "INSTYPE")] <- service[j, c("TYPEIN", "INSTYPE")]
#     }
#   }
# }

# find NAs
dt2[rowSums(is.na(dt2)) > 0,]

which(duplicated(dt2) == T)

# save data
# write.csv(dt2, "Complete_data.csv")
##############################################
dt0 <- full_join(dt, df, by = c("SEQ" = "SEQ", "PID" = "PID", "DATE_SERV" = "DATE_SERV",
                         "DIAGCODE" = "DIAGCODE", "DIAGTYPE" = "DIAGTYPE", "TYPEIN" = "TYPEIN"))
dt3 <- full_join(dt0, service, by = c("PID", "SEQ", "DATE_SERV"))

# remove TYPEIN.x
dt3[, c("TYPEIN.x")] <- NULL

# change the name of TYPEIN.y
colnames(dt3)[which(names(dt3) == "TYPEIN.y")] <- "TYPEIN"

# find NAs
dt3[rowSums(is.na(dt3)) > 0,]

# see if dt2 and dt3 are different
setdiff(dt2, dt3)
setdiff(dt3, dt2)
