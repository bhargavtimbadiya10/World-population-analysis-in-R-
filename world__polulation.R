library(readr)
print(getwd())
setwd("D:/R programming")
print(getwd())
data = read.csv("world_population_data.csv")
print(data)

#1.Find the total population of the world in 1980 and 2023
print("Total population of 1980:")
a = sum(data$X1980.population)
print(a)
print("Total population of 2023:")
b = sum(data$X2023.population)
print(b)

cat("\n**************************************************************\n\n")

#2.Calculate average population of top 10 countries in the year 2015
top10 <- head(data[ , c("country", "X2015.population")], 10)
print(top10)
avg <- mean(data$X2015.population[1:10])
print("average population of top 10 countries in the year 2015:")
print(avg)

cat("\n**************************************************************\n\n")

#3.List all countries where the land area is greater than 10,00,000 sq.km.
a <- subset(data, area > 1000000,c("country", "area"))
print("countries where the land area is greater than 1,00,000 sq.km.: ")
print(a)

cat("\n**************************************************************\n\n")

#4.Find the country with maximum and minimum population in 2000
a <- max(data$X2000.population)
country <- data$country[data$X2000.population == a]
print("maximum population of 2000:")
cat(country, "with population",a,"\n\n")

b <- min(data$X2000.population)
country <- data$country[data$X2000.population == b]
print("minimum population of 2000:")
cat(country, "with population",b)

cat("\n**************************************************************\n\n")

#5.Find top 5 fastest populated country in the world
top5 <- head(data[ , c("country", "worldper")], 5)
print("top 5 fastest populated country in the world:")
print(top5)

cat("\n**************************************************************\n\n")

#6. Find 5 countries with the lowest population in 1980 
bottom5 <- tail(data[ , c("country", "X1980.population")], 5)
print("top 5 lowestest populated country in the world:")
print(bottom5)

cat("\n**************************************************************\n\n")

#7.Find countries with a population growth rate higher than 3%
a <- subset(data, growthrate > 3,c("country", "growthrate"),)
print("countries with a population growth rate higher than 3%: ")
print(a)

cat("\n**************************************************************\n\n")

#8.List all countries where the population growth rate is negative.

growth_rate <- subset(data, growthrate < 0,c("country", "growthrate"),)
print("countries with the population growth rate is negative")
print(growth_rate)

cat("\n**************************************************************\n\n")

#9.Analyze how country populations changed every 10 years.
d <- data[1:5,]
d$X1990to2000 <- d$X2000.population - d$X1990.population
d$X2000to2010 <- d$X2010.population - d$X2000.population
d$X2010to2020 <- d$X2020.population - d$X2010.population

result <- d[, c("country", "X1990to2000", "X2000to2010", "X2010to2020")]
print(result)

cat("\n**************************************************************\n\n")

#10. Display 10 countries with population density less than 100.
a <- head(subset(data, density < 100,c("country", "density")),10)
print("countries where the density is less than 100 ")
print(a)

cat("\n**************************************************************\n\n")

#11.Show the total number of countries in each continent. 
a <- table(data$continent)
print("Total number of countries in each continent:")
print(a)

cat("\n**************************************************************\n\n")

#12.List countries where population in 2020 is more than 500 million.
a <- subset(data, X2020.population > 500000000,c("country", "X2020.population"),)
print("countries with a 500 million population in year 2020 ")
print(a)

cat("\n**************************************************************\n\n")

#13.Display top 5 countries in 2010 using bar chart 
a <- data$X2010.population[1:5] 
bp <- barplot(a/1000000,names.arg = data$country[1:5],
              main = "Top 5 Most Populated Countries (2010)",xlab = "Country",
              ylab = "Population (in Millions)",
              col = c("red","orange","brown","blue","green"),
              ylim = c(0, max(a/1000000) * 1.1))
text(x = bp, y = a/1000000,
     labels = format(a, big.mark = ","),pos = 3,cex = 1)

cat("\n**************************************************************\n\n")

#14.Display Total population by continents in 2023 using pie chart
asia <- sum(data$X2023.population[data$continent == "Asia"])
europe <- sum(data$X2023.population[data$continent == "Europe"])
africa <- sum(data$X2023.population[data$continent == "Africa"])
na <- sum(data$X2023.population[data$continent == "North America"])
sa <- sum(data$X2023.population[data$continent == "South America"])
oceania <- sum(data$X2023.population[data$continent == "Oceania"])

x <- rbind(asia, europe, africa, na, sa, oceania)

pie(x,labels = paste(c("Asia", "Europe", "Africa", "North America", "South America", "Oceania"),
                     format(x, big.mark = ",")),
    col = rainbow(6),border = "black",cex = 1,
    main = "Total Population by Continent (2023)")

cat("\n**************************************************************\n\n")

#15.Display India’s population trend from 1970 to 2023 in a Line Chart.
years <- rbind(1970, 1980, 1990, 2000, 2010, 2015, 2020, 2023)

pop <- rbind(
  data$X1970.population[data$country == "India"],
  data$X1980.population[data$country == "India"],
  data$X1990.population[data$country == "India"],
  data$X2000.population[data$country == "India"],
  data$X2010.population[data$country == "India"],
  data$X2015.population[data$country == "India"],
  data$X2020.population[data$country == "India"],
  data$X2023.population[data$country == "India"]
)
plot(years,pop / 1000000,type = "o", col = "red",
     main = "India's Population Trend (1970–2023)",
     xlab = "Year", ylab = "Population (in Millions)",
     ylim = c(400, max(pop/1000000) *1.1))
axis(1 ,at = years)
text(years,pop/1000000,labels = format(a, big.mark = ","),pos = 3,cex = 0.8)
grid()

cat("\n**************************************************************\n\n")

#16.Display 3 years population comparison by group bar chart.
a <- data$X2000.population[1:10]
b <- data$X2010.population[1:10]
d <- data$X2015.population[1:10]
m <- rbind(a, b, d)

barplot(m / 1000000,beside = TRUE,names.arg = data$country[1:10],
        xlab = "Country",ylab = "Population (in millions)",
        main = "Population Comparison (2000,2010,2015)",
        ,col = c("yellow", "orange", "red"),
        ylim = c(0, max(m/1000000) * 1.1))
legend("topright",legend = c("2000 population", "2010 population", "2015 population"),
       fill = c("yellow", "orange", "red"))

cat("\n**************************************************************\n\n")

#17.Display the 30 countries land area analysis by Histogram.
e <- data$area[1:30]
hist(e /1000000,main = "Histogram: Land Area (Top 30 Countries)",
     xlab = "Land Area (in Millions km²)",
     ylab = "Number of Countries",
     col = "lightgreen", border = "black",
     ylim = c(0, max(e/1000000) * 2),
     xlim = c(0, max(e/1000000) * 1.5))

cat("\n**************************************************************\n\n")

#18.Show a bar chart of population density for 5 smallest countries by area.
a <- data$density[data$area %in% head(sort(data$area), 5)]
bp <- barplot(a,
              names.arg = data$country[data$area %in% head(sort(data$area),5)],
              main = "Population Density of 5 Smallest Countries by Area",
              xlab = "Country", ylab = "Population Density",
              col = rainbow(5),
              ylim = c(0, max(a) * 1.3))
text(x = bp, y = a,labels = format(a, big.mark = ","),pos = 3,cex = 1)
