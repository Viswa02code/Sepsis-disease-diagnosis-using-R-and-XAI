library(caTools)

#ploting data

data <- read.csv("finaldata.csv")

data$Sepsis <- factor(data$Sepsis, levels = c("Sepsis", "Non-Sepsis"),
        labels = c(0, 1))

set.seed(123)

split <- sample.split(data, SplitRatio = 0.75)
split
Train_data <- subset(data, split == "TRUE")
Test_data <- subset(data, split == "FALSE")


summary(Test_data)
summary(Train_data)

data

