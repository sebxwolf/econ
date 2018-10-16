# set the working directory
setwd("X:/yourdirectory/")
######################################################################
#Functions

getdata <- function(file){
    #returns the file specified with processed dates.
    
    #file si the pathof the file to be read
    mydata <- read.csv(file, header = T)
    
    # create new object
    mydata$date1 <- mydata$Date 
    
    # This overwrites the variable date1 and enters the first 7 digits of the string-variable date.
    mydata$date1 <- substr(mydata$date, start = 1, stop = 7)
    
    # now we can convert it to the Date class, but first setting the locale
    lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

    mydata$date2 <- as.Date(mydata$date1, "%b%d%y")
    mydata$date2 <- ts(mydata$date2)
    
    mydata
}

createVariables <- function(mydata) {
    #creates variables of interest for regression
    
    price <- mydata$Sales_USD/mydata$Sales_U #first 16 values are nonsensical
    sales_u <-  mydata$Sales_U #first 16 values are nonsensical
    ln_sales_u <- log(mydata$Sales_U) #first 16 values are nonsensical
    ln_price <- log(price)#first 16 values are nonsensical
    
    df <- data.frame(price = price, sales_u = sales_u, ln_sales_u = ln_sales_u, ln_price = ln_price, date2 = mydata$date2)
}

runRegression <- function(vars.df) {
    #runs regression on variables of interest
    lm(vars.df$ln_sales_u ~ vars.df$ln_price)
}

merged.df <- function(data1, data2){
    #removes ln_sales from data1 and merges with data2
    
    merge(data1[, -3], data2, by = "date2")
}

##############################################################################
# 1. Hellman at Jewel

#get raw data and create new variables
hellman_at_jewel <- createVariables(getdata("JWL_HL32.csv"))

#time series plots
plot(hellman_at_jewel$date2, hellman_at_jewel$price, col = "blue", xy.labels = F, type = "l")
plot(hellman_at_jewel$date2, hellman_at_jewel$sales_u, col = "red", xy.labels = F, type = "l")

#scatter plot
plot(hellman_at_jewel$price, hellman_at_jewel$sales_u)

#regression
summary(runRegression(hellman_at_jewel))
##############################################################################3




#############################################################################
#Analysis on merged data
hellman_at_jewel <- createVariables(getdata("JWL_HL32.csv"))
kraft_at_jewel <- createVariables(getdata("JWL_KR32.csv"))
hellman_at_central <- createVariables(getdata("KC_HL32.csv"))
kraft_at_central <- createVariables(getdata( "KC_KR32.csv"))

#############################################################################
#Hellman at Jewel with Kraft price

hellman.jewel_kraft.price <- merged.df(kraft_at_jewel, hellman_at_jewel)

#x = kraft_at_jewel, y = hellman_at_jewel
summary(lm(ln_sales_u ~ ln_price.x + ln_price.y, data = hellman.jewel_kraft.price))


