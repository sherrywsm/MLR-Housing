# 1. Get the data.
housing <- read.csv('housing.csv')

#2. Discover and visualize the data to gain insights (Is there missing value in the dataframe, then how to deal with the missing value)
head(housing)
summary(housing)
dim(housing)

# Check whether each columns got missing value
apply(housing, 2, function(col)sum(is.na(col)))

# Get all the row which contains missing value
missing <- housing[rowSums(is.na(housing)) > 0,]

# remove the missing value from the data
house_no_missing <- housing[rowSums(is.na(housing)) == 0,]

# 3. Since there is geographical information (latitude and longitude), is there a way to visualize geographical data?
install.packages('ggmap')
library(ggplot2)
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)

register_google(key = "AIzaSyBmXB5S5_NIqo6lAGH-_U-TbhrQjhOsplU")
house_loc=c(-130.25,30,-110.25,45)
our_map=get_map(location = house_loc,maptype="roadmap", source="google")

ggmap(our_map)+geom_point(aes(housing$longitude, housing$latitude), data = housing)

ggmap(our_map)+geom_point(aes(housing$longitude, housing$latitude,color=median_house_value), 
                          data = housing)+scale_color_gradient(low="black", high="red")

ggmap(our_map)+geom_point(aes(housing$longitude, housing$latitude,color=median_house_value, size=housing$population), 
                          data = housing)+scale_color_gradient(low="black", high="red")

ggmap(our_map)+geom_point(aes(housing$longitude, housing$latitude,color=median_house_value, size=housing$population), 
                          data = housing)+scale_color_gradient(low="black", high="red")+scale_size(range=c(0.5,3))

# 4. Compute correlation between variables and apply multiple regression
correlation=cor(house_no_missing[,1:9])
correlation
correlation[,9]
fit=lm(median_house_value ~.-ocean_proximity, data = house_no_missing)
summary(fit)

# 5. Check multicollinearity, then how to remove multicollinearity
library(car)
vif(fit)
fit2=lm(median_house_value ~.-ocean_proximity-total_bedrooms, data = house_no_missing)
summary(fit2)
vif(fit2)

# 6. How is your final model looks like?
fit3=lm(median_house_value ~.-ocean_proximity-total_bedrooms-households, data = house_no_missing)
summary(fit3)
vif(fit3)
