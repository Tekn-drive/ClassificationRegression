data = read.csv("phones_data.csv")
data = na.omit(data)



OS <- table(data$os)
barplot(OS)

brand <- table(data$brand_name)
sum <- brand


brandName <- names(brand)

sum(brand)

piepercent <- round(100*brand / sum(brand), 2)
piepercent
pie(piepercent, label = brandName, cex = 0.6, col = rainbow(length(piepercent)))

#change currency to USD from UAH (avg of 2020, 1 USD = 26.9735 UAH)
data$best_price = data$best_price / 26.9735
data$lowest_price = data$lowest_price / 26.9735
data$highest_price = data$highest_price / 26.9735

