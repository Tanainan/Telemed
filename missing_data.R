

ipd1 <- ipd$SEQ %>% list()
new_DF1 <- new_DF$SEQ %>% list()

which(do.call(paste0, new_DF1) %in% do.call(paste0, ipd1) == F)

d <- new_DF[which(do.call(paste0, new_DF1) %in% do.call(paste0, ipd1) == F),]

rownames(d) <- NULL
write.csv(d, "Missingvalue.csv")
