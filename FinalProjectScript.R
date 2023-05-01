#Author: Aditya Kumar
library(dplyr)
#P.S: When running the code on your system, please change the file paths 

#A function to read horizonatally aligned code
read.tcsv = function(file, header=TRUE, sep=",", ...) {
  
  n = max(count.fields(file, sep=sep), na.rm=TRUE)
  x = readLines(file)
  
  .splitvar = function(x, sep, n) {
    var = unlist(strsplit(x, split=sep))
    length(var) = n
    return(var)
  }
  
  x = do.call(cbind, lapply(x, .splitvar, sep=sep, n=n))
  x = apply(x, 1, paste, collapse=sep) 
  out = read.csv(text=x, sep=sep, header=header, ...)
  return(out)
  
}

# --MAIN CODE STARTS HERE --

#First we have to read the 
daily_rate_csv <- read.csv('DATA363/Project/daily_gold_rate.csv')
daily_rate_csv$Date <-
  as.Date(daily_rate_csv$Date, format = "%Y-%m-%d")

#Turning NA values to 0
daily_rate_csv$USD[is.na(daily_rate_csv$USD)] <- 0

#---Making Data frames for USD gold rates--
#These data frame have the quarterly avg gold rate from 1985 to 2022

GoldRate.df <- data.frame(daily_rate_csv$Date, daily_rate_csv$USD)
GoldRate.df$Q <- as.numeric(format(GoldRate.df$daily_rate_csv.Date, format = "%m"))%/%4+1
GoldRate.df$YEAR <- format(GoldRate.df$daily_rate_csv.Date, format = "%Y")
GoldRate.df <- aggregate(daily_rate_csv.USD ~ Q + YEAR, GoldRate.df, mean)
#To have data only till 2020 since the rest of the data was incomplete
GoldRate.df<- GoldRate.df %>% filter(YEAR<=2020)

#--FINISHED--
#Getting GDP Data and storing it (Just 1 line of code, didn't make df because it wasn't necessary)
GDPData<-read.tcsv('DATA363/Project/GDP_DATA_US.csv')

#RESULTS
#individual vis.
boxplot(GDPData$Gross.domestic.product, ylab = "Billions (USD)", main = c("A Box Plot for USA's Quarterly GDP (1985-2020)"))
plot(GDPData$Year, GDPData$Gross.domestic.product, xlab="Quarters (1985-2020)", ylab = "Quarterly Gold Rate (USD)", type = "l" ,main = c("Quarterly Gold rates in the USA from 1985-2020"))
boxplot(GoldRate.df$daily_rate_csv.USD, ylab = "USD", main = c("A Box Plot for USA's Quarterly Gold rates(for 1 oz.) (1985-2020)"))
plot(GoldRate.df$YEAR, GoldRate.df$daily_rate_csv.USD, xlab="Quarters (1985-2020)", ylab = "Quarterly GDP (biliion USD)", type = "l" ,main = c("Quarterly Gold rates in the USA from 1985-2020"))

#join vis.
plot(GDPData$Gross.domestic.product, GoldRate.df$daily_rate_csv.USD, type = "l", xlab = "Quarterly GDP rates (billion USD)", ylab = "Quarterly Gold Rates (USD)", main = c("Gold Rate vs GDP (USA)"))
cor(GDPData$Gross.domestic.product, GoldRate.df$daily_rate_csv.USD)
lmo <- lm(GoldRate.df$daily_rate_csv.USD~GDPData$Gross.domestic.product)
summary(lmo)
