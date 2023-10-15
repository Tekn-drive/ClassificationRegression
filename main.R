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
