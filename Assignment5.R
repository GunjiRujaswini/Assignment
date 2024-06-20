stats <- read.csv(file.choose())#Demographics
stats

stats[stats$Country.Name =="France",]
stats

data<- stats[stats$Birth.rate > 20 & stats$Income.Group == "High income", ]
print(data)

max_internet_user_country <- stats[stats$Internet.users == max(stats$Internet.users,na.rm = TRUE), ]
print(max_internet_user_country)

Avg_birthrate <- mean(stats$Birth.rate, na.rm = TRUE)
print(Avg_birthrate)

SD_birthrate <- sd(stats$Birth.rate, na.rm = TRUE)
print(SD_birthrate)

countries_above_2SDs <- stats[stats$Birth.rate > (Avg_birthrate + 2*SD_birthrate), ]
print(countries_above_2SDs)


Iqr_birthrate <- IQR(stats$Birth.rate, na.rm = TRUE)
print(Iqr_birthrate)

