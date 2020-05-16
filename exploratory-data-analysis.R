
#Exploratory Data Analysis

##Pairplots among all variables
pairs(price~bedrooms+bathrooms+sqft_living+floors,data = df,col = "blue",pch = 16)
pairs(price~sqft_lot+sqft_above+sqft_basement+sqft_living+sqft_living15+sqft_lot15,data = df,col = "blue",pch = 16)
pairs(price~waterfront+condition+view+grade,data = df,col = "blue",pch = 16)#categorical variables

##Analyze response variable - house price distribution

##install.packages("ggpubr")
library(ggpubr)
## Basic density plot with mean line and marginal rug
a <- ggplot(df, aes(x = price))
a + geom_histogram(aes(y = ..density..), 
                   colour="black", fill="white",bins=30) +
  geom_density(alpha = 0.2, fill = "#FF6666") 
boxplot(df$price)

## Analyze how age and yr_renew of houses vs. price
plot(df$price~df$age)
plot(df$price~df$renov_age)


## Find bathrooms relation with house price
price_bath<- select(df,price,bathrooms)
boxplot(price_bath$price~price_bath$bathrooms,xlab="bathrooms",ylab="House Price",main="bathrooms vs. house price",col = "blue")


## Find how sqft_living related to house price
price_area<- select(df,price,sqft_living)
plot(price_area$price~price_area$sqft_living,xlab="sqft_living",ylab="House Price",main="sqft_living vs. house price",pch =16,col ="blue")


## Find how sqft_above related to house price
price_above<- select(df,price,sqft_above)
plot(price_above$price~price_above$sqft_above,xlab="sqft_above",ylab="House Price",main="sqft_above vs. house price",pch=16,col ="blue")

## Find how sqft_basement related to house price
price_above<- select(df,price,sqft_basement)
plot(price_above$price~price_above$sqft_basement,xlab="sqft_basement",ylab="House Price",main="sqft_basement vs. house price",pch=16,col ="blue")

## Find how sqft_living15 related to house price
price_sqft15<- select(df,price,sqft_living15)
plot(price_sqft15$price~price_sqft15$sqft_living15,xlab="sqft_living",ylab="House Price",main="sqft_living15 vs. house price",pch=16,col ="blue")

## lat and long to center
plot(df$price~df$distance_to_center)
min(df$distance_to_center)
max(df$distance_to_center)
df


## Find how grade affects the house price
price_grade<- select(df,price,grade)
boxplot(price_grade$price~price_grade$grade,xlab="grade",ylab="House Price",main="grade vs. house price",col =c("yellow","lightyellow","blue","red","green","orange","purple","pink","grey") ,border = "brown")
abline(h =mean(price_grade$price),col="red",lwd=3, lty=2)
plot(price_grade$price~price_grade$grade,xlab="grade",ylab="House Price",main=" Scatterplot of grade vs. house price")


## Find relationship with View and price
price_view<- select(df,price,view)
boxplot(price_view$price~price_view$view,xlab="view",ylab="House Price",main="view vs. house price",col = "blue")
plot(price_view$price~price_view$view,xlab="view",ylab="House Price",main="Scatterplot of view vs. house price")


## Find relationship with condition and price
price_condition<- select(df,price,condition)
boxplot(price_condition$price~price_condition$condition,xlab="condition",ylab="House Price",main="condition vs. house price",col = "blue")
plot(price_condition$price~price_condition$condition,xlab="condition",ylab="House Price",main=" Scatterplot of condition vs. house price")

## Find relationship with waterfront and price
price_waterfront<- select(df,price,waterfront)
boxplot(price_waterfront$price~price_waterfront$waterfront,xlab="waterfront",ylab="House Price",main="waterfront vs. house price",col = "blue")


## Find relationship with floors and price
price_floors<- select(df,price,floors)
boxplot(price_floors$price~price_floors$floors,xlab="floors",ylab="House Price",main="floors vs. house price",pch =16,col ="blue")


## Find relationship with price and bed_to_bath
plot(df$price~df$bed_to_bath)


## Find relationship with price and living_to_lot
plot(df$price~df$living_to_lot)


## Find relationship with price and price/sqrt,price/interior,price_per_floor, 
## price/sqft_above,price_per_dist
plot(df$price~df$price_per_sqrt)
plot(df$price~df$price_per_interior)
plot(df$price~df$price_per_floor)
plot(df$price~df$price_per_sqftabove)
plot(df$price~df$price_per_dist)


## Find relationship with price and price/bed,price/bathroom,price/condtion,
## price/grade
plot(df$price~df$price_per_bathroom)
plot(df$price~df$price_bed)
plot(df$price~df$price_per_condition)
plot(df$price~df$price_per_grade)

## Find relationship with price and floors_to_land,living_living15,lot_lot15,
## bed_living,bed_land,bed_floor,bathroom_to_floors,sqftliving_floots,
## sqftliving_sqftabove
plot(df$price~df$floors_to_land)
plot(df$price~df$living_living15)
plot(df$price~df$lot_lot15)
plot(df$price~df$bed_living)
plot(df$price~df$bed_land)
plot(df$price~df$bed_floor)
plot(df$price~df$bathroom_to_floors)
plot(df$price~df$sqftliving_floors)
plot(df$price~df$sqftliving_sqftabove)
plot(df$price~df$age_dif_renovate)


df_new<-df[-c(17:85)]#drop zipcode...
df_new

## Information about continuous variables, including quantiles, mean, median, count, 
## missing values.
exclude_ <-c("waterfront")
exclude_df<-df[ , !(names(df) %in% exclude_)]
describe(exclude_df)


ncol(df_new)


## Correlation Matrix and Correlation Heatmap
library(ggplot2)
library(reshape2)

## rebuild the dataframe
corr_matrix_data <- cor(df_new)

## plot correlation matrix
head(corr_matrix_data)#linear dependence between two variables
print("rank of correlation index")
sort(corr_matrix_data[,1])

## default is "pearson"
## plot heatmap
melted_corr_matrix <- melt(corr_matrix_data)
ggplot(data = melted_corr_matrix, aes(x=Var1, y=Var2, fill=value)) + geom_tile() +
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, hjust=1),plot.title = element_text(hjust=0.5))+ggtitle("Correlation Heatmap")+labs( fill = expression("Relation"))+scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                                                                                                                                                                                                  midpoint = 0, limit = c(-1,1),name="Pearson\nCorrelation") +geom_text(aes(Var2, Var1, label = round(value,2)), color = "black", size = 2)
plot_num(df_new)


## printcorrelation values higher than 0.5
zdf <- as.data.frame(as.table(cor(df_new)))
subset(zdf, abs(Freq) > 0.5)