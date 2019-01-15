setwd("/Users/miabajic/Downloads")

x = read.csv("cl_apartments.csv")
County = readRDS("shp_county.rds")
Place = readRDS("shp_place.rds")
library("maps")
library("viridis")
library("lattice")

install.packages("Hmisc")
library("Hmisc")

## Problem 1
#get rid of duplicates
summary(x$title)
x_n = x[order(x$date_posted, decreasing = TRUE),]
x_n = x_n[!duplicated(x$title),]

#subset price so there are no price outliers
x_n$price_n = x_n$price
x_n$price_n[x_n$price < 300] = NA

#look for mover advertisements and get rid of them
x_n$moving = grepl("moving", x_n$text, fixed = TRUE)
x_n$moving[x_n$moving] = NA

## Problem 2

summary(x_n$sqft)
summary(x_n$price)
#subset out square footage and price outliers more
x_n$sqft_n = x_n$sqft
x_n$sqft_n[x_n$sqft > 2000] = NA
x_n$price_n[x_n$price > 10400] = NA
#smooth scatter plot of square footage versus price
smoothScatter(x_n$sqft_n, x_n$price_n, main = "Square Footage vs Price", xlab = "Square Footage", ylab = "Price")
abline(a = -0, b = 3.25, col = "red")

#Choose counties and plot price vs square footage by county
counties = c("Sonoma", "Yolo", "San Mateo", "Santa Clara", "Placer", "Sacramento", "San Diego", "Los Angeles", "Marin", "Napa", "Alameda", "Contra Costa", "El Dorado", "San Francisco", "Solano")
xyplot(price_n ~ sqft_n|counties, data = x_n, main = "Square Footage vs Price by County", xlab = "Square Footage", ylab = "Price", panel = panel.smoothScatter)


## Problem 3
#create new bathroom variable that is subsetted
x_n$bathrooms_n = x_n$bathrooms
x_n$bathrooms_n[x_n$bathrooms_n > 5] = NA
#plot bedroom price medians 
barplot(bedpricemedian, main = "Bedrooms vs Price", xlab = "Number of Bedrooms", ylab = "Price")

#plot bathroom frequency by bedrooms
histogram(~ factor(bathrooms) | factor(bedrooms), x, main = "Total Bathrooms Frequency per Bedroom Total", xlab = "Number of Bathrooms", ylab = "Frequency of Number of Bathrooms")

#plot price veruss bedrooms by bathrooms
bwplot(price ~ factor(bedrooms) | factor(bathrooms), x, xlab = "Number of Bedrooms", ylab = "Price", main = "Number of Bedrooms vs Price \n by Number of Bathrooms")
#set margins
par(mar = c(4, 4, 4, 4))
#cross tabulate bathrooms and bedrooms
tab = table(x$bathrooms, x$bedrooms)
#find median prices for both 
medprices = tapply(x$price, list(x$bedrooms, x$bathrooms), median, na.rm=T)
#plot prices, each line is a bathroom option, x axis is number of bedrooms
matplot(medprices, type = "l", lwd = 1, main = "Median Prices by Number of Bedrooms", ylim = c(0, 10000), ylab = "Median Prices", xlab = "Number of Bedrooms", col = magma(4))
#add legend
legend("topright", lty = 1:5, col = 1:5, legend = colnames(medprices))


## Problem 4 Time Difference
#set margins
par(mar = c(5.1, 4.1, 4.1, 2.1))
#calculate time difference
x$timediff = as.Date(x$date_posted) - as.Date(x$date)
x$timediff = as.numeric(x$timediff)
#plot histogram of time differences 
hist(x$timediff, main = "Histogram of Time Difference", xlim = c(-300, 200), ylab = "Frequency", xlab = "Time Difference", breaks = 50)
#subset smaller portion of time differences
forh = subset(x, timediff > -50 & timediff < 0)
#plot this portion
hist(forh$timediff, main = "Histogram of Time Difference", xlim = c(-50, 0), ylab = "Frequency", xlab = "Time Difference")

dev.off()
library("lattice")

#Bay Area
#set margins
par(mar = c(3, 3, 3, 3))
setwd("/Users/miabajic/Desktop")
#choose counties
sfbay = c("San Francisco", "San Mateo", "Santa Clara", "Contra Costa", "Alameda", "Napa", "Solano", "Sonoma", "Marin")

#use shape files
map_sf_place = subset(Place, county %in% sfbay)
map_sf_county = subset(County, NAME %in% sfbay) 
#subset counties in sfbay
x_sfbay = subset(x, shp_county %in% sfbay)
x_sfbay$shp_place = droplevels(x_sfbay$shp_place)
x_sfbay$shp_county = droplevels(x_sfbay$shp_county)
#calculate median time differences for each place
medtimediffs = tapply(x_sfbay$timediff, x_sfbay$shp_place, median, na.rm=T)
medtimediffs = medtimediffs[as.character(map_sf_place$NAME)]
#cut into levels for color palette
sfcolor = cut2(medtimediffs, g= 5, digits = 5)
#set names of colors
names(sfcolor) = names(medtimediffs)
#create palette
palette = viridis(4)
#relate the two 
col = palette[sfcolor]
#plot county map with background color
plot(map_sf_county,bg = "gray50")
#plot place map over
plot(map_sf_place, col = col, add = T)
#add title
title("Median Time Difference of Postings \n in San Francisco Bay Area by City", line = 1)
#choose regions
sf_regions=c(sfbay[1:5],sfbay[7:8],sfbay[13:14])
#relabel them 
sf_labels=c(sfbay[1:4],"Marin",sfbay[7],"San Francisco",sfbay[13:14])
#add labels to the map 
map.text(County, regions= sf_regions, labels=sf_labels, add = TRUE, col = "white")
#add legend
legend("bottomleft", legend = levels(sfcolor), levels(col), fill = palette, text.width = 0.7)

#LA
#same process as before
la = c("Los Angeles", "Orange", "Riverside", "San Bernardino", "Ventura")

map_la_place = subset(Place, county %in% la)
map_la_county = subset(County, NAME %in% la)
x_la = subset(x, shp_county %in% la)
x_la$shp_place = droplevels(x_la$shp_place)
x_la$shp_county = droplevels(x_la$shp_county)
medtimediffs = tapply(x_la$timediff, x_la$shp_place, median, na.rm=T)
medtimediffs = medtimediffs[as.character(map_la_place$NAME)]
lacolor = cut2(medtimediffs, g = 5, digits = 5)
palette = viridis(5)
col = palette[lacolor]
plot(map_la_county,bg = "gray50")
plot(map_la_place,col = col, add = T, bg = "red")
title("Median Time Difference in Postings in Los Angeles Area by City", line = 1)

la_regions=c(la[1], la[4:6], la[9])
la_labels=c("Los Angeles", la[4:5], "Ventura", la[9])
labels = as.character(map_la_county$NAME)
text(coordinates(map_la_county), labels, col = "white")
legend("topright", legend = levels(lacolor), levels(col), fill = palette)



## Problem 5 

#Bay Area
sfbay = c("San Francisco", "San Mateo", "Santa Clara", "Contra Costa", "Alameda", "Napa", "Solano", "Sonoma", "Marin")
map_sf_place = subset(Place, county %in% sfbay)
map_sf_county = subset(County, NAME %in% sfbay)
x_sfbay = subset(x, shp_county %in% sfbay)
x_sfbay$shp_place = droplevels(x_sfbay$shp_place)
x_sfbay$shp_county = droplevels(x_sfbay$shp_county)
prices = tapply(x_sfbay$price, x_sfbay$shp_place, median, na.rm=T)
prices = prices[as.character(map_sf_place$NAME)]
sfcolor = cut2(prices, g = 4, digits = 4)
palette = viridis(4)
col = palette[sfcolor]

plot(map_sf_county,bg = "gray50")
plot(map_sf_place, col = col, add = T)
title("Median Price of Apartments in San Francisco Bay Area by City", line = 1)

sf_regions=c(sfbay[1:5],sfbay[7:8],sfbay[13:14])
sf_labels=c(sfbay[1:4],"Marin",sfbay[7],"San Francisco",sfbay[13:14])
map.text(County, regions= sf_regions, labels=sf_labels, add = TRUE, col = "white")

legend("bottomleft", legend = levels(sfcolor), levels(col), fill = palette)

#LA Area


la = c("Los Angeles", "Orange", "Riverside", "San Bernardino", "Ventura")

map_la_place = subset(Place, county %in% la)
map_la_county = subset(County, NAME %in% la)
x_la = subset(x, shp_county %in% la)
x_la$shp_place = droplevels(x_la$shp_place)
x_la$shp_county = droplevels(x_la$shp_county)
prices = tapply(x_la$price, x_la$shp_place, median, na.rm=T)
prices = prices[as.character(map_la_place$NAME)]
lacolor = cut2(prices, g = 5, digits = 5)
palette = viridis(5)
col = palette[lacolor]
plot(map_la_county,bg = "gray50")
plot(map_la_place,col = col, add = T, bg = "red")
title("Median Price of Apartments in Los Angeles Area by City", line = 1)

la_regions=c(la[1], la[4:6], la[9])
la_labels=c("Los Angeles", la[4:5], "Ventura", la[9])
labels = as.character(map_la_county$NAME)
text(coordinates(map_la_county), labels, col = "white")
legend("topright", legend = levels(lacolor), levels(col), fill = palette)


# Problem 6
#subset square footage so no outliers
sqft_c = subset(x, sqft < 15000)

# All California cities map, median apartment size as color

# SF 
sfbay = c("San Francisco", "San Mateo", "Santa Clara", "Contra Costa", "Alameda", "Napa", "Solano", "Sonoma", "Marin")
map_sf_place = subset(Place, county %in% sfbay)
map_sf_county = subset(County, NAME %in% sfbay)
x_sfbay = subset(x, shp_county %in% sfbay)
x_sfbay$shp_place = droplevels(x_sfbay$shp_place)
x_sfbay$shp_county = droplevels(x_sfbay$shp_county)
meds = tapply(x_sfbay$sqft, x_sfbay$shp_place, median, na.rm=T)
meds = meds[as.character(map_sf_place$NAME)]
sfcolor = cut2(meds, g = 4, digits = 4)
palette = viridis(4)
col = palette[sfcolor]

plot(map_sf_county,bg = "gray50")
plot(map_sf_place, col = col, add = T)
title("Median Square Footage of Apartments in San Francisco Bay Area by City", line = 1)

sf_regions=c(sfbay[1:5],sfbay[7:8],sfbay[13:14])
sf_labels=c(sfbay[1:4],"Marin",sfbay[7],"San Francisco",sfbay[13:14])
map.text(County, regions= sf_regions, labels=sf_labels, add = TRUE, col = "white")

legend("bottomleft", legend = levels(sfcolor), levels(col), fill = palette)

#LA

la = c("Los Angeles", "Orange", "Riverside", "San Bernardino", "Ventura")

map_la_place = subset(Place, county %in% la)
map_la_county = subset(County, NAME %in% la)
x_la = subset(x, shp_county %in% la)
x_la$shp_place = droplevels(x_la$shp_place)
x_la$shp_county = droplevels(x_la$shp_county)
meds = tapply(x_la$sqft, x_la$shp_place, median, na.rm=T)
meds = meds[as.character(map_la_place$NAME)]
lacolor = cut2(meds, g = 5, digits = 5)
palette = viridis(5)
col = palette[lacolor]
plot(map_la_county,bg = "gray50")
plot(map_la_place,col = col, add = T, bg = "red")
title("Median Square Footage of Apartments in Los Angeles Area by City", line = 1)

la_regions=c(la[1], la[4:6], la[9])
la_labels=c("Los Angeles", la[4:5], "Ventura", la[9])
labels = as.character(map_la_county$NAME)
text(coordinates(map_la_county), labels, col = "white")
legend("topright", legend = levels(lacolor), levels(col), fill = palette)

# Problem 7
#Do smaller apartments tend to have less reserved parking?
#subset square footage again
sqft_c = subset(x, sqft < 4000)
#plot density plot of square footage by parking type
densityplot(~ sqft, sqft_c, main = "Square Footage by Parking Type", groups = parking, auto.key = T, xlab = "Square Footage", ylab = "Frequency", plot.points = F, xlim = c(0, 1700))

#Are more expensive apartments more likely to allow pets?
#plot density plot of price by pet policy
densityplot(~ price, x, main = "Price by Pet Policy", groups = pets, auto.key = T, xlab = "Price", ylab = "Frequency", plot.points = F, xlim = c(0, 6000))

#Do larger apartments have more favorable laundry accomodation?
#plot density plot of price by laundry type
densityplot(~ price, x, main = "Price by Laundry Type", groups = laundry, auto.key = T, xlab = "Price", ylab = "Frequency", plot.points = F, xlim = c(0, 5000))