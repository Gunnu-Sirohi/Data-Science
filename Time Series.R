####################################### Steps to follow ##################################################

#1) segment the whole dataset into the 21 subsets based on the market and the customer segment level
#2) aggregate - Sales, Quantity & Profit, over the Order Date to arrive at monthly values for these attributes
#3) find the 2 most profitable and consistently profitable segments using co efficient of variation
#4) forecast the sales and quantity for the next 6 months for these two segments
#5) Test accuracy of model on last 6 months


################### Set working Directory to file path #################################################

######################################## LOAD LIBRARIES ################################################
library(skimr)
library(lubridate)
library(ggplot2)
library(forecast)
library(tseries)
require(graphics)
library(dplyr)


######################################## LOAD DATA #####################################################

global_superstore<-read.csv("Global Superstore.csv",stringsAsFactors = FALSE)

######################################## DATA UNDERSTANDING ############################################

skim(global_superstore)
# no of obs: 51290 
# no of variables: 24 

#Market  :    Type = char , no missing values , 7 unique values
#Segment :    Type = char , no missing values , 3 unique values 
#order date : Type = char , no missing values , 1430 unique values

#Quantity : Type = integer , no missing value , mean = 3.48 , min = 1 ,          max = 14
#Profit   : Type = number , no missing value , mean = 28.61 , min = -6599.978  , max = 8399.976
#Sales    : Type = number , no missing value , men = 246.49 , min = 0.44 ,       max = 22638.48

unique(global_superstore$Market)
#US, APAC, EU, Africa, EMEA, LATAM, Canada

unique(global_superstore$Segment)
# Consumer, Corporate,  Home Office


# Check format of order.date column
length(parse_date_time(global_superstore$Order.Date,orders="d-m-Y"))
# All 51290 records in %m-%d-%Y



################################### DATA PREPRATION- PART 1 ###############################################
#__________________________________________________________________________________________________
# Order Date - Character to date 

global_superstore$Order.Date <- as.Date(global_superstore$Order.Date,"%d-%m-%Y")
class(global_superstore$Order.Date)
range(global_superstore$Order.Date)
#"2011-01-01" to "2014-12-31"; 48 months data

#__________________________________________________________________________________________________
#  Creating New column Year.Month to  aggregate monthly data
global_superstore$Year.Month <- format(as.Date(global_superstore$Order.Date),"%Y-%m")
range(global_superstore$Year.Month) #48 months data


#___________________________________________________________________________________________________
#Segments - Char to Factor

global_superstore$Segment=as.factor(global_superstore$Segment)

#___________________________________________________________________________________________________
#Market - Char to Factor

global_superstore$Market= as.factor(global_superstore$Market)

################################## EXPLORATORY DATA ANALYSIS - PART 1 #################################

#_____________________________________________________________________________________
#                             Market Analysis 

ggplot(global_superstore, aes(Market, Sales)) +geom_bar(stat="summary", fun.y="sum")
ggplot(global_superstore, aes(Market, Quantity)) +geom_bar(stat="summary", fun.y="sum")
ggplot(global_superstore, aes(Market, Profit)) +geom_bar(stat="summary", fun.y="sum")


#_____________________________________________________________________________________
#                            Segment Analysis

ggplot(global_superstore, aes(Segment, Sales)) +geom_bar(stat="summary", fun.y="sum")
ggplot(global_superstore, aes(Segment, Quantity)) +geom_bar(stat="summary", fun.y="sum")
ggplot(global_superstore, aes(Segment, Profit)) +geom_bar(stat="summary", fun.y="sum")


#_______________________________________________________________________________________
#                            Time Analysis

ggplot(global_superstore, aes(Order.Date, Sales)) +geom_line()
ggplot(global_superstore, aes(Order.Date, Quantity)) +geom_line()
ggplot(global_superstore, aes(Order.Date, Sales)) +geom_line()

################################# DATA PREPRATION - PART : 2 #################################

################################# SEGMENTATION ###############################################

Segment_list<-split.data.frame(global_superstore,list(global_superstore$Market,global_superstore$Segment))
View(Segment_list)

#Segment_List (list) has 21 df created based on Market-Segment combination


################################ AGGREGATION #################################################

# Aggregate Sales,Profit and Quantity in each df of Segment_list by Market,Segment and Month

#___________________________________________________________________________________________________
#Sales 

Agg_Sales<-lapply(Segment_list, function(Segment_list) 
                                 aggregate(Sales~Market+Segment+Year.Month, data = Segment_list, FUN = "sum"))

View(Agg_Sales$Africa.Consumer)

#____________________________________________________________________________________________________
# Profit

Agg_Profit<-lapply(Segment_list, function(Segment_list) 
                                 aggregate(Profit~Market+Segment+Year.Month, data = Segment_list, FUN = "sum"))

View(Agg_Profit$Africa.Consumer)

#____________________________________________________________________________________________________
# Quantity

Agg_Quantity<-lapply(Segment_list, function(Segment_list) 
                              aggregate(Quantity~Market+Segment+Year.Month, data = Segment_list, FUN = "sum"))

View(Agg_Quantity$Africa.Consumer)

############################### TOP 2 PROFITABLE SEGMENTS ###################################

# Using global_superstore df , calculate total profit and co efficient of variance per market+segment
# then find top 2 segments(market+segment), with high profit and low CV

Top_2 <- global_superstore %>%        
         group_by(Market,Segment) %>% 
         summarise(.,    
                    profit=sum(Profit),   
                    cv=sd(Profit)*100/mean(Profit))

View(Top_2)

head(Top_2[with(Top_2, order(-profit , cv)), ],2)

#  Market Segment     profit    cv
#1 APAC   Consumer    222818  213
#2 EU     Consumer    188688  240

############################### SUBSET DATA - TOP 2 SEGMENTS ###################################

#______________________________________________________________________________________________
# Sales

SALES_APAC<-Agg_Sales$APAC.Consumer
SALES_APAC$Month.Seq<-rank(SALES_APAC$Year.Month)

SALES_EU<-Agg_Sales$EU.Consumer
SALES_EU$Month.Seq<-rank(SALES_EU$Year.Month)

#_______________________________________________________________________________________________
# Quantity

QUANTITY_APAC<-Agg_Quantity$APAC.Consumer
QUANTITY_APAC$Month.Seq<-rank(QUANTITY_APAC$Year.Month)

QUANTITY_EU<-Agg_Quantity$EU.Consumer
QUANTITY_EU$Month.Seq<-rank(QUANTITY_EU$Year.Month)

#################################### SALES - TRAIN and TEST ###########################################

#_____________________________________________________________________________________________________
# APAC - Consumer

Train_sales_APAC<-head(SALES_APAC[with(SALES_APAC, order(Year.Month)), ],42)
range(Train_sales_APAC$Year.Month) # 2011-01 - 2014-06

Test_sales_APAC<-tail(SALES_APAC[with(SALES_APAC, order(Year.Month)), ],6)
range(Test_sales_APAC$Year.Month) # 2014-07 - 2014-12

#_____________________________________________________________________________________________________
# EU - Consumer

Train_sales_EU<-head(SALES_EU[with(SALES_EU, order(Year.Month)), ],42)
range(Train_sales_EU$Year.Month) # 2011-01 - 2014-06

Test_sales_EU<-tail(SALES_EU[with(SALES_EU, order(Year.Month)), ],6)
range(Test_sales_EU$Year.Month) # 2014-07 - 2014-12


#################################### Quantity - TRAIN and TEST ###########################################

#_____________________________________________________________________________________________________
# APAC - Consumer

Train_quan_APAC<-head(QUANTITY_APAC[with(QUANTITY_APAC, order(Year.Month)), ],42)
range(Train_quan_APAC$Year.Month) # 2011-01 - 2014-06

Test_quan_APAC<-tail(QUANTITY_APAC[with(QUANTITY_APAC, order(Year.Month)), ],6)
range(Test_quan_APAC$Year.Month) # 2014-07 - 2014-12

#_____________________________________________________________________________________________________
# EU - Consumer

Train_quan_EU<-head(QUANTITY_EU[with(QUANTITY_EU, order(Year.Month)), ],42)
range(Train_sales_EU$Year.Month) # 2011-01 - 2014-06

Test_quan_EU<-tail(QUANTITY_EU[with(QUANTITY_EU, order(Year.Month)), ],6)
range(Test_quan_EU$Year.Month) # 2014-07 - 2014-12

##################################### CREATE TIME SERIES #################################################

#_________________________________________________________________________________________________
# Total 

total_timeser <- ts(global_superstore$Sales)


#________________________________________________________________________________________________________
# Sales

APAC_sales_ts<- ts(Train_sales_APAC$Sales)
plot(APAC_sales_ts)

EU_sales_ts<- ts(Train_sales_EU$Sales)
plot(EU_sales_ts)

#________________________________________________________________________________________________________
# Quantity 

APAC_quan_ts<- ts(Train_quan_APAC$Quantity)
plot(APAC_quan_ts)

EU_quan_ts<- ts(Train_quan_EU$Quantity)
plot(EU_quan_ts)

#################################### SMOOTHENING #########################################################
#__________________________________________________________________________________________________________
# Exponential Smoothing

#___________________________________
# APAC Sales

# Smoothening with aplha =0.2

plot(APAC_sales_ts)

APAC_sales_smooth <- HoltWinters(APAC_sales_ts, alpha=0.2,beta=FALSE, gamma=FALSE)
APAC_sales_smooth<- (fitted(APAC_sales_smooth)[,1])

lines(APAC_sales_smooth, col="blue", lwd=2)

#___________________________________
# EU Sales

# Smoothening with alpha = 0.15
plot(EU_sales_ts)

EU_sales_smooth <- HoltWinters(EU_sales_ts, alpha=0.15,beta=FALSE, gamma=FALSE)
EU_sales_smooth<- (fitted(EU_sales_smooth)[,1])

lines(EU_sales_smooth, col="blue", lwd=2)

#___________________________________
# APAC Quan

# Smoothening with aplha = 0.15
plot(APAC_quan_ts)

APAC_quan_smooth <- HoltWinters(APAC_quan_ts, alpha=0.15,beta=FALSE, gamma=FALSE)
APAC_quan_smooth<- (fitted(APAC_quan_smooth)[,1])

lines(APAC_quan_smooth, col="blue", lwd=2)

#___________________________________
# EU Quan 


# Smoothening with alpha = 0.1

plot(EU_quan_ts)

EU_quan_smooth <- HoltWinters(EU_quan_ts, alpha= 0.1,beta=FALSE, gamma=FALSE)
EU_quan_smooth <- (fitted(EU_quan_smooth)[,1])


lines(EU_quan_smooth, col="blue", lwd=2)



#################################### CLASSICAL DECOMPOSITION #############################################

#________________________________________________________
# convert the time series to a dataframe

# APAC_quan_smooth
timevals_in<-Train_quan_APAC$Month.Seq
APAC_quan_smoothdf <- as.data.frame(cbind(timevals_in, as.vector(APAC_quan_smooth)))
APAC_quan_smoothdf<-APAC_quan_smoothdf[-42,]
colnames(APAC_quan_smoothdf) <- c('Month', 'Quan')

# EU_quan_smooth
timevals_in<-Train_quan_EU$Month.Seq
EU_quan_smoothdf <- as.data.frame(cbind(timevals_in, as.vector(EU_quan_smooth)))
EU_quan_smoothdf<-EU_quan_smoothdf[-42,]
colnames(EU_quan_smoothdf) <- c('Month', 'Quan')

# APAC_sales_smooth
timevals_in<-Train_sales_APAC$Month.Seq
APAC_sales_smoothdf <- as.data.frame(cbind(timevals_in, as.vector(APAC_sales_smooth)))
APAC_sales_smoothdf<-APAC_sales_smoothdf[-42,]
colnames(APAC_sales_smoothdf) <- c('Month', 'Sales')

# EU_sales_smooth
timevals_in<-Train_sales_EU$Month.Seq
EU_sales_smoothdf <- as.data.frame(cbind(timevals_in, as.vector(EU_sales_smooth)))
EU_sales_smoothdf<-EU_sales_smoothdf[-42,]
colnames(EU_sales_smoothdf) <- c('Month', 'Sales')

#__________________________________________________________________________________________________
#  fit a multiplicative model


# APAC_sales

timevals_in<-APAC_sales_smoothdf$Month

lmfit_APAC_sales <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=APAC_sales_smoothdf)
global_pred_APAC_sales <- predict(lmfit_APAC_sales, Month=timevals_in)
summary(global_pred_APAC_sales)


# EU_sales

timevals_in<-EU_sales_smoothdf$Month

lmfit_EU_sales <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                       + Month, data=EU_sales_smoothdf)
global_pred_EU_sales <- predict(lmfit_EU_sales, Month=timevals_in)
summary(global_pred_EU_sales)


# APAC_quan

timevals_in<-APAC_quan_smoothdf$Month

lmfit_APAC_quan <- lm(Quan ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                       + Month, data=APAC_quan_smoothdf)
global_pred_APAC_quan <- predict(lmfit_APAC_quan, Month=timevals_in)
summary(global_pred_APAC_quan)


# EU_quan

timevals_in<-EU_quan_smoothdf$Month

lmfit_EU_quan <- lm(Quan ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                       + Month, data = EU_quan_smoothdf)
global_pred_EU_quan <- predict(lmfit_EU_quan, Month=timevals_in)
summary(global_pred_EU_quan)


#_______________________________________________________________________________________________________
# Loclly predicted series

# APAC_sales

local_pred_APAC_sales <- APAC_sales_ts-global_pred_APAC_sales
plot(local_pred_APAC_sales, col='red', type = "l")

acf(local_pred_APAC_sales)
acf(local_pred_APAC_sales, type="partial")

armafit_APAC_sales <- auto.arima(local_pred_APAC_sales)
tsdiag(armafit_APAC_sales)
armafit_APAC_sales


# EU_sales

local_pred_EU_sales <- EU_sales_ts-global_pred_EU_sales
plot(local_pred_EU_sales, col='red', type = "l")

acf(local_pred_EU_sales)
acf(local_pred_EU_sales, type="partial")

armafit_EU_sales <- auto.arima(local_pred_EU_sales)
tsdiag(armafit_EU_sales)
armafit_EU_sales


# APAC_quan

local_pred_APAC_quan <- APAC_quan_ts-global_pred_APAC_quan
plot(local_pred_APAC_quan, col='red', type = "l")

acf(local_pred_APAC_quan)
acf(local_pred_APAC_quan, type="partial")

armafit_APAC_quan <- auto.arima(local_pred_APAC_quan)
tsdiag(armafit_APAC_quan)
armafit_APAC_quan


# EU_quan

local_pred_EU_quan <- EU_quan_ts-global_pred_EU_quan
plot(local_pred_EU_quan, col='red', type = "l")

acf(local_pred_EU_quan)
acf(local_pred_EU_quan, type="partial")

armafit_EU_quan <- auto.arima(local_pred_EU_quan)
tsdiag(armafit_EU_quan)
armafit_EU_quan

#________________________________________________________________________________________________
# Check Residual

# APAC Sales

Residue_APAC_sales<-local_pred_APAC_sales - fitted(armafit_APAC_sales)

adf.test(Residue_APAC_sales,alternative = "stationary")
#Dickey-Fuller = -4.3131, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary

kpss.test(Residue_APAC_sales)
#KPSS Level = 0.043997, Truncation lag parameter = 3, p-value = 0.1

# EU_Sales

Residue_EU_sales<-local_pred_EU_sales - fitted(armafit_EU_sales)

adf.test(Residue_EU_sales,alternative = "stationary")
#Dickey-Fuller = -3.8239, Lag order = 3, p-value = 0.02736
#alternative hypothesis: stationary

kpss.test(Residue_EU_sales)
#KPSS Level = 0.09595, Truncation lag parameter = 3, p-value = 0.1

#APAC_Quan

Residue_APAC_quan<-local_pred_APAC_quan - fitted(armafit_APAC_quan)

adf.test(Residue_APAC_quan,alternative = "stationary")
#Dickey-Fuller = -3.9947, Lag order = 3, p-value = 0.01943
#alternative hypothesis: stationary

kpss.test(Residue_APAC_quan)
#KPSS Level = 0.083387, Truncation lag parameter = 3, p-value = 0.1


# EU_Quan

Residue_EU_quan<-local_pred_EU_quan - fitted(armafit_EU_quan)

adf.test(Residue_EU_quan,alternative = "stationary")
#Dickey-Fuller = -3.8239, Lag order = 3, p-value = 0.02736
#alternative hypothesis: stationary

kpss.test(Residue_EU_quan)
#KPSS Level = 0.09595, Truncation lag parameter = 3, p-value = 0.1

#________________________________________________________________________
# Prediction 

# APAC_Sales
timevals_out <- Test_sales_APAC$Month.Seq
global_pred_out_APAC_sales <- predict(lmfit_APAC_sales,data.frame(Month =timevals_out))

fcast_APAC_sales <- global_pred_out_APAC_sales


# EU_Sales
timevals_out <- Test_sales_EU$Month.Seq
global_pred_out_EU_sales <- predict(lmfit_EU_sales,data.frame(Month =timevals_out))

fcast_EU_sales <- global_pred_out_EU_sales

# APAC_quan
timevals_out <- Test_quan_APAC$Month.Seq
global_pred_out_APAC_quan <- predict(lmfit_APAC_quan,data.frame(Month =timevals_out))

fcast_APAC_quan <- global_pred_out_APAC_quan

# EU_quan
timevals_out <- Test_quan_EU$Month.Seq
global_pred_out_EU_quan <- predict(lmfit_EU_quan,data.frame(Month =timevals_out))

fcast_EU_quan <- global_pred_out_EU_quan

#################################### Auto - ARIMA ###############################################################

# APAC_sales
autoarima_APAC_sales <- auto.arima(APAC_sales_ts)
autoarima_APAC_sales # ARIMA(0,1,1)  , AIC=898.23   AICc=898.55   BIC=901.66
tsdiag(autoarima_APAC_sales)
plot(autoarima_APAC_sales$x, col="black")
lines(fitted(autoarima_APAC_sales), col="red")


# EU_sales
autoarima_EU_sales <- auto.arima(EU_sales_ts)
autoarima_EU_sales # ARIMA(2,1,0), AIC=897.67   AICc=898.32   BIC=902.81
tsdiag(autoarima_EU_sales)
plot(autoarima_EU_sales$x, col="black")
lines(fitted(autoarima_EU_sales), col="red")


# APAC_quan
autoarima_APAC_quan <- auto.arima(APAC_quan_ts)
autoarima_APAC_quan # ARIMA(0,1,0), AIC=534.14   AICc=534.24   BIC=535.85
tsdiag(autoarima_APAC_quan)
plot(autoarima_APAC_quan$x, col="black")
lines(fitted(autoarima_APAC_quan), col="red")

# EU_quan
autoarima_EU_quan <- auto.arima(EU_quan_ts)
autoarima_EU_quan # ARIMA(2,1,0),AIC=898.23   AICc=898.55   BIC=901.66
tsdiag(autoarima_EU_quan)
plot(autoarima_EU_quan$x, col="black")
lines(fitted(autoarima_EU_quan), col="red")

#________________________________________________________________________________________________
# Residual

# APAC_sales
Residue_auto_arima_APAC_sales<-APAC_sales_ts - fitted(autoarima_APAC_sales)

adf.test(Residue_auto_arima_APAC_sales,alternative = "stationary")
#Dickey-Fuller = -4.2563, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary

kpss.test(Residue_auto_arima_APAC_sales)
#KPSS Level = 0.043931, Truncation lag parameter = 3, p-value = 0.1


# EU_sales
Residue_auto_arima_EU_sales<-EU_sales_ts - fitted(autoarima_EU_sales)

adf.test(Residue_auto_arima_EU_sales,alternative = "stationary")
#Dickey-Fuller = -4.3522, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary

kpss.test(Residue_auto_arima_EU_sales)
#KPSS Level = 0.067962, Truncation lag parameter = 3, p-value = 0.1


# APAC_quan
Residue_auto_arima_APAC_quan<-APAC_quan_ts - fitted(autoarima_APAC_quan)

adf.test(Residue_auto_arima_APAC_quan,alternative = "stationary")
#Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary

kpss.test(Residue_auto_arima_APAC_quan)
#KPSS Level = 0.04642, Truncation lag parameter = 3, p-value = 0.1

# EU_quan
Residue_auto_arima_EU_quan<-EU_quan_ts - fitted(autoarima_EU_quan)

adf.test(Residue_auto_arima_EU_quan,alternative = "stationary")
#Dickey-Fuller = -3.5969, Lag order = 3, p-value = 0.04521
#alternative hypothesis: stationary

kpss.test(Residue_auto_arima_EU_quan)
#KPSS Level = 0.056232, Truncation lag parameter = 3, p-value = 0.1

#______________________________________________________________________________________________
#Prediction

fcast_auto_arima_APAC_sales <- predict(autoarima_APAC_sales, n.ahead = 6)
fcast_auto_arima_EU_sales <- predict(autoarima_EU_sales, n.ahead = 6)
fcast_auto_arima_APAC_quan <- predict(autoarima_APAC_quan, n.ahead = 6)
fcast_auto_arima_EU_quan <- predict(autoarima_EU_quan, n.ahead = 6)

################################### MAPE #################################################################
#________________________________________________________________________________________________
# Classical Decomposition

# APAC_sales
MAPE_APAC_sales <- accuracy(fcast_APAC_sales,Test_sales_APAC[,4])[5]
MAPE_APAC_sales # 31.46491

# EU_sales
MAPE_EU_sales <- accuracy(fcast_EU_sales,Test_sales_EU[,4])[5]
MAPE_EU_sales # 31.23611


# APAC_quan
MAPE_APAC_quan <- accuracy(fcast_APAC_quan,Test_quan_APAC[,4])[5]
MAPE_APAC_quan # 30.34932

# EU_quan
MAPE_EU_sales <- accuracy(fcast_EU_quan,Test_quan_EU[,4])[5]
MAPE_EU_sales # 38.4679

#________________________________________________________________________________________________
# Auto Arima


MAPE_APAC_sales_arima <- accuracy(fcast_auto_arima_APAC_sales$pred,Test_sales_APAC[,4])[5]
MAPE_APAC_sales_arima #27.68952

MAPE_EU_sales_arima <- accuracy(fcast_auto_arima_EU_sales$pred,Test_sales_EU[,4])[5]
MAPE_EU_sales_arima #28.9226

MAPE_APAC_quan_arima <- accuracy(fcast_auto_arima_APAC_quan$pred,Test_quan_APAC[,4])[5]
MAPE_APAC_quan_arima #26.24458

MAPE_EU_quan_arima <- accuracy(fcast_auto_arima_EU_quan$pred,Test_quan_EU[,4])[5]
MAPE_EU_quan_arima #30.13319


# Auto - Arima , better model
################################## FORECAST #############################################################

# APAC_sales
auto_arima_pred_APAC_sales <- c(fitted(autoarima_APAC_sales),ts(fcast_auto_arima_APAC_sales$pred))
plot(APAC_sales_ts, col = "black")
lines(auto_arima_pred_APAC_sales, col = "red")

# EU_sales
auto_arima_pred_EU_sales <- c(fitted(autoarima_EU_sales),ts(fcast_auto_arima_EU_sales$pred))
plot(EU_sales_ts, col = "black")
lines(auto_arima_pred_EU_sales, col = "red")

# APAC_quan
auto_arima_pred_APAC_quan <- c(fitted(autoarima_APAC_quan),ts(fcast_auto_arima_APAC_quan$pred))
plot(APAC_quan_ts, col = "black")
lines(auto_arima_pred_APAC_quan, col = "red")

# EU_quan
auto_arima_pred_EU_quan <- c(fitted(autoarima_EU_quan),ts(fcast_auto_arima_EU_quan$pred))
plot(EU_quan_ts, col = "black")
lines(auto_arima_pred_EU_quan, col = "red")

