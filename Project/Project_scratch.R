library(tidyr)

f <- file.choose("Users/anneschwenker/Documents/606/Project/Gender_Data/Gender_StatsData.csv")
gender_data <-read.csv(file="Users/anneschwenker/Documents/606/Project/Gender_Data/Gender_StatsData.csv", header=TRUE, sep=",")

head(gender_data)
gathered_gender_data <- tidyr::gather(gender_data,"year",starts_with("X"))
head(gathered_gender_data)
summary(gathered_gender_data)

