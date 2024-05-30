#observations:

#Fields always present: ICULOS, Age, Gender, HospAdmTime, SepsisLabel
#Fields with < 15% missing values: HR, O2Sat, SBP, MAP, DBP
#Fields with 20% to 90% missing: Temp, Resp, Glucose, Unit1, Unit2
#All other fields have > 90% missing values.
#The median patient percent missing is 100% for 16 of the quantities.

#Demographics:

#Only about 1.4% of raw records indicate sepsis. [2623 / 188,453]
#Only about 5.6% of the patients have sepsis. [279 / 5000]
#Sepsis reported in first hour for 20.1% of sepsis patients. [56 / 279]

#original data
data_clean<-read.csv("C:\\Users\\viswa\\Downloads\\Dataset.csv")
onlycolumns<-c("ICULOS", "Age", "Gender", "HospAdmTime","HR", "O2Sat", "SBP", "MAP", "DBP", "SepsisLabel")

df_clean<-data_clean[,onlycolumns]
sum(is.na(df_clean$ICULOS))
sum(is.na(df_clean$Age))
sum(is.na(df_clean$Gender))
sum(is.na(df_clean$HospAdmTime))
sum(is.na(df_clean$SepsisLabel))
sum(is.na(df_clean$HR))
sum(is.na(df_clean$O2Sat))
sum(is.na(df_clean$SBP))
sum(is.na(df_clean$MAP))
sum(is.na(df_clean$MAP))

final_df<-na.omit(df_clean)
final_df$Salary = ifelse(is.na(final_df$O2Sat),ave(final_df$O2Sat,FUN = function(x)mean(x,na.rm = TRUE)),final_df$O2Sat)
final_df$SBP = ifelse(is.na(final_df$SBP),ave(final_df$SBP,FUN = function(x)mean(x,na.rm = TRUE)),final_df$SBP)
final_df$Salary = ifelse(is.na(final_df$MAP),ave(final_df$MAP,FUN = function(x)mean(x,na.rm = TRUE)),final_df$MAP)
final_df$Salary = ifelse(is.na(final_df$DBP),ave(final_df$DBP,FUN = function(x)mean(x,na.rm = TRUE)),final_df$DBP)
final_df$Salary = ifelse(is.na(final_df$HR),ave(final_df$HR,FUN = function(x)mean(x,na.rm = TRUE)),final_df$HR)

str(final_df)
dt<-final_df[1:9]

sum(is.na(final_df))
nrow(final_df)
str(final_df)

max(final_df$Age)
min(final_df$Age)

set.seed(123)

final_df$Patient_ID <- paste0("P", seq_len(nrow(final_df)))
str(final_df)


#handling Imbalance dataset
datafinal<-final_df[, c("Patient_ID", "Gender", "Age", "ICULOS", "HospAdmTime", "HR", "O2Sat", "SBP", "MAP", "DBP", "SepsisLabel")]
str(datafinal)
table(datafinal$SepsisLabel)
library(sqldf)
tab<-sqldf("SELECT *
FROM datafinal
WHERE SepsisLabel = 0
LIMIT 5000")
tab1<-sqldf("SELECT * FROM datafinal WHERE SepsisLabel = 1 LIMIT 5000;")

last<-rbind(tab1,tab)
names(last)[names(last) == "SepsisLabel"] <- "Sepsis_Result"
last <- last[, -which(names(last) == "Patient_ID")]

str(last)

write.csv(last, file = "data.csv", row.names = FALSE)

orgdata <- read.csv("Dataset.csv")
summary(is.na(orgdata))
sum(is.na(orgdata))

install.packages(c("ggplot2", "reshape2"))

# Load libraries
library(ggplot2)
library(reshape2)

orgdatamatrix <- as.matrix(orgdata)
orgdatamatrix1 <- as.matrix(data)
data <- read.csv("data.csv")


heatmap(orgdatamatrix, scale = "none", Rowv = NA, Colv = NA, cexRow = 0.5, cexCol = 0.5)
heatmap(orgdatamatrix1, scale = "row", Rowv = NA, Colv = NA, cexRow = 0.7, cexCol = 1)

na_counts <- list()

for (col in names(orgdata)) {
  na_count <- sum(is.na(orgdata[[col]]))
  
  na_counts[[col]] <- na_count
}
print(na_counts)