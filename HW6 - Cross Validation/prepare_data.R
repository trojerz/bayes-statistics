first <- read.csv("2015.csv")
second <- read.csv("2016.csv")
third <- read.csv("2017.csv")
fourth <- read.csv("2018.csv")
fifth <- read.csv("2019.csv")

countries <- first[c("Country", "Region")]


happiness <- read_csv("happiness.csv")



colnames(fourth) <- c("Rank", "Country", "Hapiness.Score", "Economy..GDP.per.Capita.", "Social", "Health..Life.Expectancy.", "Freedom", "Generosity", "Trust..Government.Corruption.")
colnames(fifth) <- c("Rank", "Country", "Hapiness.Score", "Economy..GDP.per.Capita.", "Social", "Health..Life.Expectancy.", "Freedom", "Generosity", "Trust..Government.Corruption.")

first["Year"] <- 2015
second["Year"] <- 2016
third["Year"] <- 2017
fourth["Year"] <- 2018
fifth["Year"] <- 2019

first_three_cols <- intersect(intersect(colnames(first), colnames(second)), colnames(third))
take_columns <- intersect(intersect(colnames(fourth), first_three_cols), colnames(fifth))


first <- first[take_columns]
second <- second[take_columns]
third <- third[take_columns]
fourth <- fourth[take_columns]
fifth <- fifth[take_columns]

all_data <- rbind(first, second, third, fourth, fifth)


colnames(all_data) <- c("Country", "Economy", "Health", "Freedom", "Generosity", "Trust", "Year")

happiness <- happiness[c("country", "score", "year")]
colnames(happiness) <- c("Country", "Score", "Year")


merged_data <- merge(all_data, happiness, by=c("Country","Year"))
merged_data <- merge(merged_data, countries, by=c("Country"))


write.csv(merged_data, file = "hapiness_kaggle.csv", row.names = FALSE)

