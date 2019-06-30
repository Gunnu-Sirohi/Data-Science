#----------------------------- Assumption ----------------------------------------------

# GMV for e-commerce retail companies means list price charged to the customer multiplied by the number of items sold.
# MRP : MRP of total units sold
# List Price = MRP / units
# Discounted Price = GMV/units 
# 0 GMV  can mean that the product was given free in some offer
# Null GMV :  min GMV = 0 i.e product can be given for free  and  max GMV = MRP
#            in case of null GMV we have assumed that product is sold without any dicount at MRP , hence GMV = MRP
# NPS is an indicator of customer satisfaction and SLA also decides customer satisfaction , hence we will
#  just consider NPS in our model and not SLA . All SLA realated columns will be dropped
# Large quatity orders can be a bulk order and not considered as outlier

#---------------------------- Set working directory to file path ------------------------

#----------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< load library >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-----------------------------------------------------------------------------------------------------

install.packages("skimr")
install.packages("readxl")
install.packages("car")
install.packages("fastDummies")
install.packages("reshape")
install.packages("dLagM")
install.packages("DataCombine")
install.packages("DAAG")
library(readxl)
library(skimr)
library(lubridate)
library(dplyr)
library(MASS)
library(car)
library(fastDummies)
library(reshape)
library(dLagM)
library(DataCombine)
library(DAAG)
library(ggplot2)
library(tidyr)
library(zoo)
library(lubridate)
#------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Load Data >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------------------------------------------------------------------------

Sales_Data<-read.csv("ConsumerElectronics.csv",stringsAsFactors = FALSE)

Media_Investment<-read_excel("Media data and other information.xlsx" , sheet = 2 , skip = 2)

NPS_Score<-read_excel("Media data and other information.xlsx" , sheet = 4   , skip = 1)


#--------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Data Understanding >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------------------------------------------------------------------------------------------

skim(Sales_Data)

#n obs: 1648824 
#n variables: 20
# Missing values : 4904 : cust_is , gmv , pincode
# Empty Strings : 0
# Number and Integer feilds with 0 : product_mrp , sla , gmv 

# order_id , order_item_id , customer_id , pincode in scientific notation
# Deliverybdays , Deliverycdays have /N string value

# As order_item_id are unique for each item in an order , order_item_id cannot be duplicate
Sales_Data$order_item_id<-format(Sales_Data$order_item_id, scientific = FALSE)
Sales_Data$order_id<-format(Sales_Data$order_id, scientific = FALSE)
Sales_Data$cust_id<-format(Sales_Data$cust_id, scientific = FALSE)
Sales_Data$pincode<-format(Sales_Data$pincode, scientific = FALSE)


#------------------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Data Prepration >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------------------------------------------------------------------------------------

# 1) Filter Data from July 2015 to June 2016
Sales_Data<-Sales_Data[Sales_Data$order_date >='2015-07-01' & Sales_Data$order_date<'2016-07-01',]

max(Sales_Data$order_date) # "2016-06-30 23:59:55"
min(Sales_Data$order_date) # "2015-07-01 00:36:11"

# 2) Filter Data for required subcategory
Sales_Data<-Sales_Data[Sales_Data$product_analytic_sub_category %in% c('CameraAccessory','HomeAudio','GamingAccessory'),]

unique(Sales_Data$product_analytic_sub_category) #"CameraAccessory" "GamingAccessory" "HomeAudio" 

# 3) Find records with MRP = 0

length(which(Sales_Data$product_mrp==0)) #3426 such records

# remove these records as  mrp cannot be zero
Sales_Data<- Sales_Data[!(Sales_Data$product_mrp==0) ,]

# 4) Find records with GMV=0 

length(which(Sales_Data$gmv == 0 )) #261 such records
# 0 GMV  can mean that the product was given free in some offer


# 5) Find null values

# As mentioned in data understanding , cust_id , gmv , pincode have null values


# Replace null GMV with MRP
 
Sales_Data$gmv<- ifelse( is.na(Sales_Data$gmv) == TRUE, Sales_Data$product_mrp, Sales_Data$gmv)

sum(is.na(Sales_Data$gmv)) # 0

# We will not remove null customer_id and pincode as these columns will not be considered in model buildng 
# and thus have no impact


#6) if GMV is greater than MRP

length(which(Sales_Data$gmv > Sales_Data$product_mrp)) # 20510

gmv_graterthan_mrp<-Sales_Data[Sales_Data$gmv > Sales_Data$product_mrp,]
View(gmv_graterthan_mrp)

# replace gmv of these records with mrp

Sales_Data$gmv<- ifelse( Sales_Data$gmv > Sales_Data$product_mrp,
                         Sales_Data$product_mrp, Sales_Data$gmv)




#7) Outlier

# GMV
quantile(Sales_Data$gmv,seq(0.1,1,0.01)) # no outlier

# Unit Sold
quantile(Sales_Data$units,seq(0.1,1,0.01)) # no outlier

units_graterthan_two<-Sales_Data[Sales_Data$units > 2,]
View(units_graterthan_two)

# mpr
quantile(Sales_Data$product_mrp,seq(0.1,1,0.01)) # no outlier


#-----------------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< KPI Engineering >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------------------------------------------------------------------------------------

#__________________________________________________________________________________________________
# Week of the month
Sales_Data$order_date <- as.Date(Sales_Data$order_date)
Sales_Data$Week<-ceiling(day(Sales_Data$order_date) / 7)

#___________________________________________________________________________________________________
# List Price 
Sales_Data$List_Price <- Sales_Data$gmv/Sales_Data$units

#____________________________________________________________________________________________________
# Discounted Price Percentage
Sales_Data$Discount_per<- ((Sales_Data$product_mrp- Sales_Data$List_Price)/Sales_Data$product_mrp)*100


#_____________________________________________________________________________________________________
# Total Sales , Avg Units sols , Avg MRP , Avg List Price , Avg Discounted Price per week per category

Sales_Data1<-Sales_Data[,c("Year","Month","Week","product_analytic_sub_category","product_analytic_vertical",
                           "order_id","gmv","units","product_mrp","List_Price","Discount_per")]

Sales_Data_Final<- Sales_Data1 %>% 
                   dplyr::group_by(Year,Month,Week,product_analytic_sub_category,product_analytic_vertical) %>% 
                   dplyr::summarise(total_gmv = sum(gmv),
                                 Avg_units = weighted.mean(units),
                                 Avg_list_price= weighted.mean(List_Price),
                                 Avg_discount_per = weighted.mean(Discount_per),
                                 Avg_mrp = weighted.mean(product_mrp)
                                 )

#_______________________________________________________________________________________________________
# Number of COD and Prepaid orders

Sales_Data1<-Sales_Data[,c("Year","Month","Week","product_analytic_sub_category","s1_fact.order_payment_type")]

Payment_Type<-Sales_Data1 %>% 
              dplyr::group_by(Year,Month,Week,product_analytic_sub_category) %>% 
              dplyr::summarise( cod = sum(s1_fact.order_payment_type== 'COD') ,
                                prepaid = sum(s1_fact.order_payment_type== 'Prepaid')
                 )


# join Sales_Data_Final and Payment Type
Sales_Data_Final<-merge(Sales_Data_Final, Payment_Type, by=c("Year","Month","Week","product_analytic_sub_category"))


#_________________________________________________________________________________________________________
## Sales_Calander
holiday_list<-c("2015-07-18","2015-07-19","2015-08-15",
                "2015-08-16","2015-08-17","2015-08-28",
                "2015-08-29","2015-08-30","2015-10-15",
                "2015-10-16","2015-10-17","2015-11-07","2015-11-08","2015-11-09","2015-11-10",
                "2015-11-11","2015-11-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26",
                "2015-12-27","2015-12-28","2015-12-29","2015-12-30","2016-01-01","2016-01-02",
                "2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01","2016-02-02",
                "2016-02-20","2016-02-21","2016-02-14","2016-02-15","2016-03-07","2016-03-08",
                "2016-03-09","2016-05-25","2016-05-26","2016-05-27")

Holiday_Calander <- data.frame(as.Date(holiday_list))
Holiday_Calander$Year<-year(Holiday_Calander$as.Date.holiday_list.)
Holiday_Calander$Month<-month(Holiday_Calander$as.Date.holiday_list.)
Holiday_Calander$Week<-ceiling(day(Holiday_Calander$as.Date.holiday_list) / 7)
Holiday_Calander$Sale_day<- 1

Holiday_Calander$as.Date.holiday_list.<- NULL
View(Holiday_Calander)

# Number of Sales_day per week
holiday_details<-Holiday_Calander %>% 
  dplyr::group_by(Year,Month,Week) %>% 
  dplyr::summarise( number_sale_day = sum(Sale_day) 
  )

Sales_Data_Final <- merge(Sales_Data_Final, holiday_details, by = c("Year","Month", "Week"), all.x = TRUE)

Sales_Data_Final$number_sale_day<-ifelse(is.na(Sales_Data_Final$number_sale_day)== TRUE , 0 ,Sales_Data_Final$number_sale_day )

#________________________________________________________________________________________________________________
# Media Investment

View(Media_Investment)

days <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),'days')
Date <- data.frame(days)
Date$Year <- year(Date$days)
Date$Month <- month(Date$days)
Date$Week<-ceiling(day(Date$days) / 7)

# number of days in week
number_of_days<-Date %>% 
                    dplyr::group_by(Year,Month,Week) %>% 
                    dplyr::summarise( number_of_days = n() 
                    )

# total days in month
total_days<-Date %>% 
            dplyr::group_by(Year,Month) %>% 
             dplyr::summarise( number_of_days = n() 
              )

Month<-merge(number_of_days,total_days, by=c("Year","Month"))

# Calculate week weightage = number of days in week / total days in month
Week_Weightage<-Month %>% 
                dplyr::group_by(Year,Month,Week) %>% 
                dplyr::summarise( Week_Weightage = (number_of_days.x/number_of_days.y )
                 )

Media_Investment_Final<-merge(Week_Weightage,Media_Investment, by=c("Year","Month"))

Media_Investment_Final[is.na(Media_Investment_Final)] <- 0

# Convert monthly media investement into weekly investment by multiplying it with weekly weightage

Media_Investment_Weekly<-Media_Investment_Final %>% 
                        dplyr::group_by(Year,Month,Week) %>% 
                        dplyr::summarise( Total_invest = `Total Investment`*Week_Weightage,
                                          TV = TV*Week_Weightage,
                                          Digital = Digital*Week_Weightage ,
                                          Sponsorship = Sponsorship*Week_Weightage,
                                          Content_Marketing = `Content Marketing`*Week_Weightage,
                                          Online_Marketing = `Online marketing`*Week_Weightage,
                                          Affiliates = Affiliates*Week_Weightage,
                                          SEM = SEM * Week_Weightage,
                                          Radio = Radio *Week_Weightage,
                                          Other = Other*Week_Weightage)
                                          
options(scipen=999)

# Convert invetment into crores
Media_Investment_Weekly[,4:13] <- Media_Investment_Weekly[,4:13]*10000000                                    
                                          
Sales_Data_Final <- merge(Sales_Data_Final, Media_Investment_Weekly, by = c("Year","Month", "Week"), all.x = TRUE)

#___________________________________________________________________________________________________________________
# NPS

View(NPS_Score)

NPS_Score <- NPS_Score[2:13]
NPS_Score_t <- t(NPS_Score)
NPS_Score_t <- cbind(NPS_Score_t, c(2015,2015,2015,2015,2015, 2015,2016,2016,2016,2016,2016,2016), c(7,8,9,10,11,12,1,2,3,4,5,6))
colnames(NPS_Score_t) <- c("NPS_Score","Year","Month") 

Sales_Data_Final <- merge(Sales_Data_Final,NPS_Score_t, by = c("Year","Month"), all.x = TRUE)


#-----------------------------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Exploratory Data Analysis >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------------------------------------------------------------------------------------------------



# Avg List Price
Mean_GA = Sales_Data%>%
  group_by(Year , Month)%>%
  summarise(Avg_List_Price = weighted.mean(List_Price))
ggplot(Mean_GA,aes(x = Month , y = Avg_List_Price , color=factor(Year)))+geom_line() +geom_point() +theme_classic() +
  scale_x_discrete(name="Months since July 2015", limits=seq(1,12,1))

# Total GMV trend 
Mean_GA = Sales_Data%>%
  group_by(Year , Month)%>%
  summarise(Total_gmv = sum(gmv))
ggplot(Mean_GA,aes(x = Month , y = Total_gmv , color=factor(Year)))+geom_line() +geom_point() +theme_classic() +
  scale_x_discrete(name="Months since July 2015", limits=seq(1,12,1))

# Units sold trend 

Mean_GA = Sales_Data%>%
  group_by(Year , Month)%>%
  summarise(Avg_units_sold = weighted.mean(units))
ggplot(Mean_GA,aes(x = Month , y = Avg_units_sold , color=factor(Year)))+geom_line() +geom_point() +theme_classic() +
  scale_x_discrete(name="Months since July 2015", limits=seq(1,12,1))


# Discounts trend
Mean_GA = Sales_Data%>%
  group_by(Year , Month)%>%
  summarise(Avg_discount_per = weighted.mean(Discount_per))
ggplot(Mean_GA,aes(x = Month , y = Avg_discount_per , color=factor(Year)))+geom_line() +geom_point() +theme_classic() +
  scale_x_discrete(name="Months since July 2015", limits=seq(1,12,1))



#Payment type vs number of orders
sales_Data_order <- Sales_Data %>% group_by ( s1_fact.order_payment_type) %>% summarise(order_count = n())

ggplot(sales_Data_order, aes(x= s1_fact.order_payment_type, y =order_count, fill = s1_fact.order_payment_type )) + geom_bar(stat = "identity")+
  labs(x="Payment Type" , y= "Total Units")




## Month wise media investment details 
Media_Investment_1 <- gather(Media_Investment, Medium, Spend, 4:12)
Media_Investment_1 <- gather(Media_Investment, Medium, Spend, 4:12)
ggplot(Media_Investment_1, aes (x = Month, y = Spend, colour = Medium)) + geom_line() +
  theme_classic() +
  scale_x_discrete(name="Months since May 2015", limits=seq(1,12,1))

#Plotting weekly total sales
ggplot(Sales_Data_Final , aes ( x = Week , y = total_gmv , fill = Week))+ geom_bar(stat = "identity")
  
#______________________________________________________________________________________________________________________
# Scale Dataset
  
  Sales_Data_Final[c(6:24)]<- scale(Sales_Data_Final[c(6:24)])  

#-----------------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Corelation >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------------------------------------------------------------------------------------

corr <- cor(Sales_Data_Final[,-c (1:5)])

cormat <- round(cor(Sales_Data_Final[,-c (1:5)]),2)
melted_cormat <- melt(cormat)
colnames(melted_cormat)<-c("Column1","Column2","Corelation")
#Corelation matrix for defaulters
View(melted_cormat)
Non_Corelated_Columns<-unique(melted_cormat[melted_cormat$Corelation > 0.1 & melted_cormat$Corelation < 0.6,2])
View(Non_Corelated_Columns)

# Total investment is highly corelated to other channel incestment , so we will remove this column 
# in next steps

#-----------------------------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Train , Test and Vaildation data sets >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------------------------------------------------------------------------------------------------

#______________________________________________________________________________________________________________________
# Gaming Accessory

Sales_Gaming<-Sales_Data_Final[Sales_Data_Final$product_analytic_sub_category== 'GamingAccessory',]

# Dummy Variable

Sales_Gaming <- fastDummies::dummy_cols(Sales_Gaming, select_columns = "product_analytic_vertical")

View(Sales_Gaming)

Train_Gaming<-Sales_Gaming[Sales_Gaming$Year %in% c(2015,2016) & Sales_Gaming$Month %in% c(7,8,9,10,11,12,1,2) , ]
Train_Gaming[c(1:5,14)]<- NULL
Test_Gaming<-Sales_Gaming[Sales_Gaming$Year == 2016 & Sales_Gaming$Month %in% c(3,4),]
Test_Gaming[c(1:5,14)]<- NULL
Validation_Gaming <-Sales_Gaming[Sales_Gaming$Year == 2016 & Sales_Gaming$Month %in% c(5,6),]
Validation_Gaming[c(1:5,14)]<- NULL

#_______________________________________________________________________________________________________________________
# CameraAccessory

Sales_Camera<-Sales_Data_Final[Sales_Data_Final$product_analytic_sub_category== 'CameraAccessory',]

# Dummy Variable
Sales_Camera <- fastDummies::dummy_cols(Sales_Camera, select_columns = "product_analytic_vertical")
View(Sales_Camera)

Train_Camera<-Sales_Camera[Sales_Camera$Year %in% c(2015,2016) & Sales_Camera$Month %in% c(7,8,9,10,11,12,1,2), ]
Train_Camera[c(1:5,14)]<- NULL
Test_Camera<-Sales_Camera[Sales_Camera$Year == 2016 & Sales_Camera$Month %in% c(3,4),]
Test_Camera[c(1:5,14)]<- NULL
Validation_Camera<-Sales_Camera[Sales_Camera$Year == 2016 & Sales_Camera$Month %in% c(5,6),]
Validation_Camera[c(1:5,14)]<- NULL

#___________________________________________________________________________________________________________________                        
# HomeAudio

Sales_HomeAudio<-Sales_Data_Final[Sales_Data_Final$product_analytic_sub_category== 'HomeAudio',]

# Dummy Variable
Sales_HomeAudio <- fastDummies::dummy_cols(Sales_HomeAudio, select_columns = "product_analytic_vertical")
View(Sales_HomeAudio)

Train_HomeAudio<-Sales_HomeAudio[Sales_HomeAudio$Year %in% c(2015,2016) & Sales_HomeAudio$Month %in% c(7,8,9,10,11,12,1,2), ]
Train_HomeAudio[c(1:5,14)]<- NULL
Test_HomeAudio<-Sales_HomeAudio[Sales_HomeAudio$Year == 2016 & Sales_HomeAudio$Month %in% c(3,4),]
Test_HomeAudio[c(1:5,14)]<- NULL
Validation_HomeAudio<-Sales_HomeAudio[Sales_HomeAudio$Year == 2016 & Sales_HomeAudio$Month %in% c(5,6),]
Validation_HomeAudio[c(1:5,14)]<- NULL

#---------------------------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Linear Regression Model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------------------------------------------------------------------------------------------------

#______________________________________________________________________________________________________________________
#Gaming Accessory

Gaming_model1<-lm(total_gmv~. , data = Train_Gaming)
summary(Gaming_model1) #Adjusted R-squared: 0.7038 


# step AIC
step_game_model1 <- stepAIC(Gaming_model1, direction = "both")
summary(step_game_model1) #Adjusted R-squared:  0.7086
vif<-vif(step_game_model1)
View(vif)

# remove Avg MRP , p value : 0.143170 
Gaming_model2<-lm(formula = total_gmv ~  cod + TV + Digital + Sponsorship + 
                    Content_Marketing + Online_Marketing + Affiliates + SEM + 
                    NPS_Score + product_analytic_vertical_GamePad + product_analytic_vertical_GamingAccessoryKit + 
                    product_analytic_vertical_GamingAdapter + product_analytic_vertical_GamingHeadset + 
                    product_analytic_vertical_GamingKeyboard + product_analytic_vertical_GamingMemoryCard + 
                    product_analytic_vertical_GamingMouse + product_analytic_vertical_GamingMousePad + 
                    product_analytic_vertical_JoystickGamingWheel + product_analytic_vertical_MotionController + 
                    product_analytic_vertical_TVOutCableAccessory, data = Train_Gaming)

summary(Gaming_model2) # Adjusted R-squared:  0.7078
vif<-vif(Gaming_model2)
View(vif)


# remove GamingAdapter , p value : 0.13637
Gaming_model3<-lm(formula = total_gmv ~  cod + TV + Digital + Sponsorship + 
                    Content_Marketing + Online_Marketing + Affiliates + SEM + 
                    NPS_Score + product_analytic_vertical_GamePad + product_analytic_vertical_GamingAccessoryKit + 
                    product_analytic_vertical_GamingHeadset + 
                    product_analytic_vertical_GamingKeyboard + product_analytic_vertical_GamingMemoryCard + 
                    product_analytic_vertical_GamingMouse + product_analytic_vertical_GamingMousePad + 
                    product_analytic_vertical_JoystickGamingWheel + product_analytic_vertical_MotionController + 
                    product_analytic_vertical_TVOutCableAccessory, data = Train_Gaming)

summary(Gaming_model3) # Adjusted R-squared:  0.7069 
vif<-vif(Gaming_model3)
View(vif)

# remove JoystickGamingWheel , p value : 0.19510
Gaming_model4<-lm(formula = total_gmv ~  cod + TV + Digital + Sponsorship + 
                    Content_Marketing + Online_Marketing + Affiliates + SEM + 
                    NPS_Score + product_analytic_vertical_GamePad + product_analytic_vertical_GamingAccessoryKit + 
                    product_analytic_vertical_GamingHeadset + 
                    product_analytic_vertical_GamingKeyboard + product_analytic_vertical_GamingMemoryCard + 
                    product_analytic_vertical_GamingMouse + product_analytic_vertical_GamingMousePad + 
                     product_analytic_vertical_MotionController + 
                    product_analytic_vertical_TVOutCableAccessory, data = Train_Gaming)

summary(Gaming_model4) # Adjusted R-squared:  0.7065
vif<-vif(Gaming_model4)
View(vif)

# remove MotionController , p value 0.23246
Gaming_model5<-lm(formula = total_gmv ~  cod + TV + Digital + Sponsorship + 
                    Content_Marketing + Online_Marketing + Affiliates + SEM + 
                    NPS_Score + product_analytic_vertical_GamePad + product_analytic_vertical_GamingAccessoryKit + 
                    product_analytic_vertical_GamingHeadset + 
                    product_analytic_vertical_GamingKeyboard + product_analytic_vertical_GamingMemoryCard + 
                    product_analytic_vertical_GamingMouse + product_analytic_vertical_GamingMousePad + 
                    product_analytic_vertical_TVOutCableAccessory, data = Train_Gaming)

summary(Gaming_model5) # Adjusted R-squared:  0.7062
vif<-vif(Gaming_model5)
View(vif)

# remove GamingMousePad , p value : 0.12347

Gaming_model6<-lm(formula = total_gmv ~  cod + TV + Digital + Sponsorship + 
                    Content_Marketing + Online_Marketing + Affiliates + SEM + 
                    NPS_Score + product_analytic_vertical_GamePad + product_analytic_vertical_GamingAccessoryKit + 
                    product_analytic_vertical_GamingHeadset + 
                    product_analytic_vertical_GamingKeyboard + product_analytic_vertical_GamingMemoryCard + 
                    product_analytic_vertical_GamingMouse + 
                    product_analytic_vertical_TVOutCableAccessory, data = Train_Gaming)

summary(Gaming_model6) # Adjusted R-squared:  0.7053
vif<-vif(Gaming_model6)
View(vif)

# remove TVOutCableAccessory , p value : 0.171419

Gaming_model7<-lm(formula = total_gmv ~  cod + TV + Digital + Sponsorship + 
                    Content_Marketing + Online_Marketing + Affiliates + SEM + 
                    NPS_Score + product_analytic_vertical_GamePad + product_analytic_vertical_GamingAccessoryKit + 
                    product_analytic_vertical_GamingHeadset + 
                    product_analytic_vertical_GamingKeyboard + product_analytic_vertical_GamingMemoryCard + 
                    product_analytic_vertical_GamingMouse , data = Train_Gaming)

summary(Gaming_model7) # Adjusted R-squared:  0.7074
vif<-vif(Gaming_model7)
View(vif)

# remove GamingAccessoryKit , p value : 0.151393 

Gaming_model8<-lm(formula = total_gmv ~  cod + TV + Digital + Sponsorship + 
                    Content_Marketing + Online_Marketing + Affiliates + SEM + 
                    NPS_Score + product_analytic_vertical_GamePad +
                    product_analytic_vertical_GamingHeadset + 
                    product_analytic_vertical_GamingKeyboard + product_analytic_vertical_GamingMemoryCard + 
                    product_analytic_vertical_GamingMouse , data = Train_Gaming)

summary(Gaming_model8) # Adjusted R-squared:  0.7039
vif<-vif(Gaming_model8)
View(vif)

# remove GamingMemoryCard , p value : 0.068128

Gaming_model9<-lm(formula = total_gmv ~  cod + TV + Digital + Sponsorship + 
                    Content_Marketing + Online_Marketing + Affiliates + SEM + 
                    NPS_Score + product_analytic_vertical_GamePad +
                    product_analytic_vertical_GamingHeadset + 
                    product_analytic_vertical_GamingKeyboard +  
                    product_analytic_vertical_GamingMouse , data = Train_Gaming)


summary(Gaming_model9) # Adjusted R-squared:  0.7023
vif<-vif(Gaming_model9)
View(vif)

# remove SEM , high VIF ; 93808

Gaming_model10<-lm(formula = total_gmv ~  cod + TV + Digital + Sponsorship + 
                     Content_Marketing + Online_Marketing + Affiliates +  
                     NPS_Score + product_analytic_vertical_GamePad +
                     product_analytic_vertical_GamingHeadset + 
                     product_analytic_vertical_GamingKeyboard +  
                     product_analytic_vertical_GamingMouse , data = Train_Gaming)

summary(Gaming_model10) # Adjusted R-squared:  0.6904
vif<-vif(Gaming_model10)
View(vif)

# remove content marketing 0.31340 , high VIF 

Gaming_model11<-lm(formula = total_gmv ~  cod + TV + Digital + Sponsorship + 
                      Online_Marketing + Affiliates +  
                     NPS_Score + product_analytic_vertical_GamePad +
                     product_analytic_vertical_GamingHeadset + 
                     product_analytic_vertical_GamingKeyboard +  
                     product_analytic_vertical_GamingMouse , data = Train_Gaming)

summary(Gaming_model11) # Adjusted R-squared:  0.6903
vif<-vif(Gaming_model11)
View(vif)

# remove Digital 
Gaming_model12<-lm(formula = total_gmv ~  cod + TV  + Sponsorship + 
                     Online_Marketing + Affiliates +  
                     NPS_Score + product_analytic_vertical_GamePad +
                     product_analytic_vertical_GamingHeadset + 
                     product_analytic_vertical_GamingKeyboard +  
                     product_analytic_vertical_GamingMouse , data = Train_Gaming)

summary(Gaming_model12) # Adjusted R-squared:  0.691
vif<-vif(Gaming_model12)
View(vif)


# remove TV 
Gaming_model13<-lm(formula = total_gmv ~  cod  + Sponsorship + 
                     Online_Marketing + Affiliates +  
                     NPS_Score + product_analytic_vertical_GamePad +
                     product_analytic_vertical_GamingHeadset + 
                     product_analytic_vertical_GamingKeyboard +  
                     product_analytic_vertical_GamingMouse , data = Train_Gaming)

summary(Gaming_model13) # Adjusted R-squared:  0.6902
vif<-vif(Gaming_model13)
View(vif)

# remove Affiliates
Gaming_model14<-lm(formula = total_gmv ~  cod  + Sponsorship + 
                     Online_Marketing +   
                     NPS_Score + product_analytic_vertical_GamePad +
                     product_analytic_vertical_GamingHeadset + 
                     product_analytic_vertical_GamingKeyboard +  
                     product_analytic_vertical_GamingMouse , data = Train_Gaming)

summary(Gaming_model14) # Adjusted R-squared:  0.6902
vif<-vif(Gaming_model14)
View(vif)

# remove sponshorship
Gaming_model15<-lm(formula = total_gmv ~  cod  + 
                     Online_Marketing +   
                     NPS_Score + product_analytic_vertical_GamePad +
                     product_analytic_vertical_GamingHeadset + 
                     product_analytic_vertical_GamingKeyboard +  
                     product_analytic_vertical_GamingMouse , data = Train_Gaming)

summary(Gaming_model15) # Adjusted R-squared:  0.6869
vif<-vif(Gaming_model15)
View(vif)

# All variable with high significance and low VIF

# Test


Predict_ga <- predict(Gaming_model15,Test_Gaming[,-1])
Test_Gaming$predict_gmv <- Predict_ga

r <- cor(Test_Gaming$total_gmv,Test_Gaming$predict_gmv)
rsquared <- cor(Test_Gaming$total_gmv,Test_Gaming$predict_gmv)^2
rsquared # 0.70469

# Cross Validation
cv.lm(data = Sales_Gaming, form.lm = Gaming_model15, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 
#MSE : 0.0693
#_________________________________________________________________________________________________________________
#Camera Accessory

Camera_model1<-lm(total_gmv~. , data = Train_Camera)
summary(Camera_model1) #Adjusted R-squared:  0.743


# step AIC
step_camera_model1 <- stepAIC(Camera_model1, direction = "both")
summary(step_camera_model1) #Adjusted R-squared:  0.7467
vif<- vif(step_camera_model1)
View(vif)

# remove CameraAccessory
Camera_model2<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                    prepaid + TV + Digital + Content_Marketing + Online_Marketing + 
                    Affiliates + SEM + NPS_Score + product_analytic_vertical_Binoculars + 
                     product_analytic_vertical_CameraBattery + 
                    product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                    product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                    product_analytic_vertical_Lens + product_analytic_vertical_Strap + 
                    product_analytic_vertical_Telescope + product_analytic_vertical_ExtensionTube + 
                    product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                    product_analytic_vertical_CameraBag, data = Train_Camera)

summary(Camera_model2) #Adjusted R-squared:  0.7462
vif<- vif(Camera_model2)
View(vif)

# remove strap
Camera_model3<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                    prepaid + TV + Digital + Content_Marketing + Online_Marketing + 
                    Affiliates + SEM + NPS_Score + product_analytic_vertical_Binoculars + 
                    product_analytic_vertical_CameraBattery + 
                    product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                    product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                    product_analytic_vertical_Lens +  
                    product_analytic_vertical_Telescope + product_analytic_vertical_ExtensionTube + 
                    product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                    product_analytic_vertical_CameraBag, data = Train_Camera)

summary(Camera_model3) #Adjusted R-squared:  0.7457
vif<- vif(Camera_model3)
View(vif)

# remove Online_Marketing  
Camera_model4<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                    prepaid + TV + Digital + Content_Marketing + 
                    Affiliates + SEM + NPS_Score + product_analytic_vertical_Binoculars + 
                    product_analytic_vertical_CameraBattery + 
                    product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                    product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                    product_analytic_vertical_Lens +  
                    product_analytic_vertical_Telescope + product_analytic_vertical_ExtensionTube + 
                    product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                    product_analytic_vertical_CameraBag, data = Train_Camera)

summary(Camera_model4) #Adjusted R-squared:  0.7451
vif<- vif(Camera_model4)
View(vif)

# remove digital 

Camera_model5<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                    prepaid + TV + Content_Marketing + 
                    Affiliates + SEM + NPS_Score + product_analytic_vertical_Binoculars + 
                    product_analytic_vertical_CameraBattery + 
                    product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                    product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                    product_analytic_vertical_Lens +  
                    product_analytic_vertical_Telescope + product_analytic_vertical_ExtensionTube + 
                    product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                    product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model5) #Adjusted R-squared:  0.7452
vif<- vif(Camera_model5)
View(vif)

# remove tv

Camera_model6<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                    prepaid + Content_Marketing + 
                    Affiliates + SEM + NPS_Score + product_analytic_vertical_Binoculars + 
                    product_analytic_vertical_CameraBattery + 
                    product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                    product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                    product_analytic_vertical_Lens +  
                    product_analytic_vertical_Telescope + product_analytic_vertical_ExtensionTube + 
                    product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                    product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model6) #Adjusted R-squared:  0.7456
vif<- vif(Camera_model6)
View(vif)

# remove sem

Camera_model7<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                    prepaid + Content_Marketing + 
                    Affiliates  + NPS_Score + product_analytic_vertical_Binoculars + 
                    product_analytic_vertical_CameraBattery + 
                    product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                    product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                    product_analytic_vertical_Lens +  
                    product_analytic_vertical_Telescope + product_analytic_vertical_ExtensionTube + 
                    product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                    product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model7) #Adjusted R-squared:  0.7459
vif<- vif(Camera_model7)
View(vif)

# remove affilates

Camera_model8<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                    prepaid + Content_Marketing + 
                     NPS_Score + product_analytic_vertical_Binoculars + 
                    product_analytic_vertical_CameraBattery + 
                    product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                    product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                    product_analytic_vertical_Lens +  
                    product_analytic_vertical_Telescope + product_analytic_vertical_ExtensionTube + 
                    product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                    product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model8) #Adjusted R-squared:  0.7458
vif<- vif(Camera_model8)
View(vif)

# remove content marketing
Camera_model9<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                    prepaid +  
                    NPS_Score + product_analytic_vertical_Binoculars + 
                    product_analytic_vertical_CameraBattery + 
                    product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                    product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                    product_analytic_vertical_Lens +  
                    product_analytic_vertical_Telescope + product_analytic_vertical_ExtensionTube + 
                    product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                    product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model9) #Adjusted R-squared:  0.7452
vif<- vif(Camera_model9)
View(vif)

# remove CameraEyeCup
Camera_model10<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                    prepaid +  
                    NPS_Score + product_analytic_vertical_Binoculars + 
                    product_analytic_vertical_CameraBattery + 
                    product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                    product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                    product_analytic_vertical_Lens +  
                    product_analytic_vertical_Telescope + product_analytic_vertical_ExtensionTube + 
                     product_analytic_vertical_CameraMicrophone + 
                    product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model10) #Adjusted R-squared:  0.7444
vif<- vif(Camera_model10)
View(vif)


# remove Telescope 
Camera_model11<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                     prepaid +  
                     NPS_Score + product_analytic_vertical_Binoculars + 
                     product_analytic_vertical_CameraBattery + 
                     product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                     product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                     product_analytic_vertical_Lens +  
                      product_analytic_vertical_ExtensionTube + 
                     product_analytic_vertical_CameraMicrophone + 
                     product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model11) #Adjusted R-squared:  0.7432
vif<- vif(Camera_model11)
View(vif)


# remove Filter 
Camera_model12<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                     prepaid +  
                     NPS_Score + product_analytic_vertical_Binoculars + 
                     product_analytic_vertical_CameraBattery + 
                     product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                     product_analytic_vertical_CameraRemoteControl + 
                     product_analytic_vertical_Lens +  
                     product_analytic_vertical_ExtensionTube + 
                     product_analytic_vertical_CameraMicrophone + 
                     product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model12) #Adjusted R-squared:  0.7417
vif<- vif(Camera_model12)
View(vif)

# remove CameraBatteryGrip 
Camera_model13<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                     prepaid +  
                     NPS_Score + product_analytic_vertical_Binoculars + 
                     product_analytic_vertical_CameraBattery + 
                     product_analytic_vertical_CameraMount + 
                     product_analytic_vertical_CameraRemoteControl + 
                     product_analytic_vertical_Lens +  
                     product_analytic_vertical_ExtensionTube + 
                     product_analytic_vertical_CameraMicrophone + 
                     product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model13) #Adjusted R-squared:  0.7402
vif<- vif(Camera_model13)
View(vif)

# remove CameraMicrophone 
Camera_model14<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                     prepaid +  
                     NPS_Score + product_analytic_vertical_Binoculars + 
                     product_analytic_vertical_CameraBattery + 
                     product_analytic_vertical_CameraMount + 
                     product_analytic_vertical_CameraRemoteControl + 
                     product_analytic_vertical_Lens +  
                     product_analytic_vertical_ExtensionTube + 
                     product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model14) #Adjusted R-squared:  0.739
vif<- vif(Camera_model14)
View(vif)


# remove Avg_list_price 
Camera_model15<-lm(formula = total_gmv ~  Avg_discount_per + 
                     prepaid +  
                     NPS_Score + product_analytic_vertical_Binoculars + 
                     product_analytic_vertical_CameraBattery + 
                     product_analytic_vertical_CameraMount + 
                     product_analytic_vertical_CameraRemoteControl + 
                     product_analytic_vertical_Lens +  
                     product_analytic_vertical_ExtensionTube + 
                     product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model15) #Adjusted R-squared:  0.7375
vif<- vif(Camera_model15)
View(vif)

# remove CameraMount
Camera_model16<-lm(formula = total_gmv ~  Avg_discount_per + 
                     prepaid +  
                     NPS_Score + product_analytic_vertical_Binoculars + 
                     product_analytic_vertical_CameraBattery + 
                     product_analytic_vertical_CameraRemoteControl + 
                     product_analytic_vertical_Lens +  
                     product_analytic_vertical_ExtensionTube + 
                     product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model16) #Adjusted R-squared:  0.7355
vif<- vif(Camera_model16)
View(vif)

# remove CameraRemoteControl
Camera_model17<-lm(formula = total_gmv ~  Avg_discount_per + 
                     prepaid +  
                     NPS_Score + product_analytic_vertical_Binoculars + 
                     product_analytic_vertical_CameraBattery + 
                     product_analytic_vertical_Lens +  
                     product_analytic_vertical_ExtensionTube + 
                     product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model17) #Adjusted R-squared:  0.7314
vif<- vif(Camera_model17)
View(vif)



# remove ExtensionTube
Camera_model18<-lm(formula = total_gmv ~  Avg_discount_per + 
                     prepaid +  
                     NPS_Score + product_analytic_vertical_Binoculars + 
                     product_analytic_vertical_CameraBattery + 
                     product_analytic_vertical_Lens +  
                     product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model18) #Adjusted R-squared:  0.729
vif<- vif(Camera_model18)
View(vif)


# remove npsscore
Camera_model19<-lm(formula = total_gmv ~  Avg_discount_per + 
                     prepaid +  
                     product_analytic_vertical_Binoculars + 
                     product_analytic_vertical_CameraBattery + 
                     product_analytic_vertical_Lens +  
                     product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_model19) #Adjusted R-squared:  0.7247
vif<- vif(Camera_model19)
View(vif) #Adjusted R-squared:  0.725

# All variable significant and with low VIF

# Test


Predict_ca <- predict(Camera_model19,Test_Camera[,-1])
Test_Camera$predict_gmv <- Predict_ca

r <- cor(Test_Camera$total_gmv,Test_Camera$predict_gmv)
rsquared <- cor(Test_Camera$total_gmv,Test_Camera$predict_gmv)^2
rsquared # 0.43


# Cross Validation
cv.lm(data = Sales_Camera, form.lm = Camera_model19, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 
#MSE : 0.161
#__________________________________________________________________________________________________________________________
# Home Audio



HomeAudio_model1<-lm(total_gmv~. , data = Train_HomeAudio)
summary(HomeAudio_model1) #Adjusted R-squared:  0.7185


# step AIC
step_homeaudio_model1 <- stepAIC(HomeAudio_model1, direction = "both")
summary(step_homeaudio_model1) #Adjusted R-squared:  0.7244
vif<- vif(step_homeaudio_model1)
View(vif)

# remove content marketing , 
HomeAudio_model2<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                       cod + TV + Digital +  Online_Marketing + 
                       Affiliates + SEM + NPS_Score + product_analytic_vertical_Dock + 
                       product_analytic_vertical_DockingStation + product_analytic_vertical_HomeAudioSpeaker + 
                       product_analytic_vertical_VoiceRecorder, data = Train_HomeAudio)
summary(HomeAudio_model2) #Adjusted R-squared:  0.7229
vif<- vif(HomeAudio_model2)
View(vif)

# remove digital : 

HomeAudio_model3<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                       cod + TV +   Online_Marketing + 
                       Affiliates + SEM + NPS_Score + product_analytic_vertical_Dock + 
                       product_analytic_vertical_DockingStation + product_analytic_vertical_HomeAudioSpeaker + 
                       product_analytic_vertical_VoiceRecorder, data = Train_HomeAudio)
summary(HomeAudio_model3) #Adjusted R-squared:  0.7237
vif<- vif(HomeAudio_model3)
View(vif)

# remove tv

HomeAudio_model4<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                       cod +  Online_Marketing + 
                       Affiliates + SEM + NPS_Score + product_analytic_vertical_Dock + 
                       product_analytic_vertical_DockingStation + product_analytic_vertical_HomeAudioSpeaker + 
                       product_analytic_vertical_VoiceRecorder, data = Train_HomeAudio)
summary(HomeAudio_model4) #Adjusted R-squared:  0.7246
vif<- vif(HomeAudio_model4)
View(vif)

# remove SEM

HomeAudio_model5<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                       cod +  Online_Marketing + 
                       Affiliates +  NPS_Score + product_analytic_vertical_Dock + 
                       product_analytic_vertical_DockingStation + product_analytic_vertical_HomeAudioSpeaker + 
                       product_analytic_vertical_VoiceRecorder, data = Train_HomeAudio)
summary(HomeAudio_model5) #Adjusted R-squared:  0.7248
vif<- vif(HomeAudio_model5)
View(vif)

# remove Affiliates

HomeAudio_model6<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                       cod +  Online_Marketing + 
                         NPS_Score + product_analytic_vertical_Dock + 
                       product_analytic_vertical_DockingStation + product_analytic_vertical_HomeAudioSpeaker + 
                       product_analytic_vertical_VoiceRecorder, data = Train_HomeAudio)
summary(HomeAudio_model6) #Adjusted R-squared:  0.7256
vif<- vif(HomeAudio_model6)
View(vif)

# remove Online_Marketing

HomeAudio_model7<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                       cod +  
                       NPS_Score + product_analytic_vertical_Dock + 
                       product_analytic_vertical_DockingStation + product_analytic_vertical_HomeAudioSpeaker + 
                       product_analytic_vertical_VoiceRecorder, data = Train_HomeAudio)
summary(HomeAudio_model7) #Adjusted R-squared:  0.7264
vif<- vif(HomeAudio_model7)
View(vif)

# remove NPS_Score

HomeAudio_model8<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                       cod +  
                        product_analytic_vertical_Dock + 
                       product_analytic_vertical_DockingStation + product_analytic_vertical_HomeAudioSpeaker + 
                       product_analytic_vertical_VoiceRecorder, data = Train_HomeAudio)
summary(HomeAudio_model8) #Adjusted R-squared:  0.7263
vif<- vif(HomeAudio_model8)
View(vif)

# remove VoiceRecorder

HomeAudio_model9<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                       cod +  
                       product_analytic_vertical_Dock + 
                       product_analytic_vertical_DockingStation + product_analytic_vertical_HomeAudioSpeaker 
                     , data = Train_HomeAudio)
summary(HomeAudio_model9) #Adjusted R-squared:  0.7247
vif<- vif(HomeAudio_model9)
View(vif)

# remove DockingStation

HomeAudio_model10<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                       cod +  
                       product_analytic_vertical_Dock + 
                        product_analytic_vertical_HomeAudioSpeaker 
                     , data = Train_HomeAudio)
summary(HomeAudio_model10) #Adjusted R-squared:  0.722
vif<- vif(HomeAudio_model10)
View(vif)

# remove Avg_list_price

HomeAudio_model11<-lm(formula = total_gmv ~ Avg_discount_per + 
                        cod +  
                        product_analytic_vertical_Dock + 
                        product_analytic_vertical_HomeAudioSpeaker 
                      , data = Train_HomeAudio)
summary(HomeAudio_model11) #Adjusted R-squared:  0.7193
vif<- vif(HomeAudio_model11)
View(vif)

# test


Predict_ha <- predict(HomeAudio_model11,Test_HomeAudio[,-1])
Test_HomeAudio$predict_gmv <- Predict_ha

r <- cor(Test_HomeAudio$total_gmv,Test_HomeAudio$predict_gmv)
rsquared <- cor(Test_HomeAudio$total_gmv,Test_HomeAudio$predict_gmv)^2
rsquared # 0.774

# Cross Validation
cv.lm(data = Sales_HomeAudio, form.lm = HomeAudio_model11, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 
#MSE : 0.933

#-----------------------------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Multiplicative  Model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-----------------------------------------------------------------------------------------------------------------------

#_______________________________________________________________________________________________________________________
#Camera Accessory

Sales_Camera_mul <- Sales_Camera
Sales_Camera_mul[c(1:5,14)]<- NULL

Sales_Cam_ln <- as.data.frame(log1p(Sales_Camera_mul))
Sales_Cam_ln <- Sales_Cam_ln[complete.cases(Sales_Cam_ln),]


set.seed(100)
indices= sample(1:nrow(Sales_Cam_ln), 0.7*nrow(Sales_Cam_ln))

train_cam <- Sales_Cam_ln[indices,]
test_cam <- Sales_Cam_ln[-indices,]



model_1 <-lm(total_gmv~.,data=train_cam)
summary(model_1)

#Stepwise Approach
model_2 <- stepAIC(model_1, direction="both")
summary(model_2)
vif(model_2)


#remove Camera housing
model_3 <- lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                Avg_mrp + cod + number_sale_day + TV + Digital + Sponsorship + 
                Content_Marketing + SEM + Radio + Other + product_analytic_vertical_Binoculars + 
                product_analytic_vertical_CameraAccessory + product_analytic_vertical_CameraBag + 
                product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                product_analytic_vertical_Lens + product_analytic_vertical_Strap + 
                product_analytic_vertical_Telescope + product_analytic_vertical_CameraEyeCup + 
                product_analytic_vertical_CameraMicrophone, 
              data = train_cam)
summary(model_3)
vif(model_3)


#remove CameraMicrophone 
model_4 <- lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                Avg_mrp + cod + number_sale_day + TV + Digital + Sponsorship + 
                Content_Marketing + SEM + Radio + Other + product_analytic_vertical_Binoculars + 
                product_analytic_vertical_CameraAccessory + product_analytic_vertical_CameraBag + 
                product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                product_analytic_vertical_Lens + product_analytic_vertical_Strap + 
                product_analytic_vertical_Telescope + product_analytic_vertical_CameraEyeCup, 
              data = train_cam)
summary(model_4)
vif(model_4)


#remove Digital
model_5 <- lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                Avg_mrp + cod + number_sale_day + TV + Sponsorship + 
                Content_Marketing + SEM + Radio + Other + product_analytic_vertical_Binoculars + 
                product_analytic_vertical_CameraAccessory + product_analytic_vertical_CameraBag + 
                product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                product_analytic_vertical_Lens + product_analytic_vertical_Strap + 
                product_analytic_vertical_Telescope + product_analytic_vertical_CameraEyeCup, 
              data = train_cam)
summary(model_5)
vif(model_5)


#remove Avg_list_price
model_6 <- lm(formula = total_gmv ~ Avg_discount_per + 
                Avg_mrp + cod + number_sale_day + TV + Sponsorship + 
                Content_Marketing + SEM + Radio + Other + product_analytic_vertical_Binoculars + 
                product_analytic_vertical_CameraAccessory + product_analytic_vertical_CameraBag + 
                product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                product_analytic_vertical_Lens + product_analytic_vertical_Strap + 
                product_analytic_vertical_Telescope + product_analytic_vertical_CameraEyeCup, 
              data = train_cam)
summary(model_6)
vif(model_6)


#remove CameraEyeCup
model_7 <- lm(formula = total_gmv ~ Avg_discount_per + 
                Avg_mrp + cod + number_sale_day + TV + Sponsorship + 
                Content_Marketing + SEM + Radio + Other + product_analytic_vertical_Binoculars + 
                product_analytic_vertical_CameraAccessory + product_analytic_vertical_CameraBag + 
                product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                product_analytic_vertical_Lens + product_analytic_vertical_Strap + 
                product_analytic_vertical_Telescope, 
              data = train_cam)
summary(model_7)
vif(model_7)


#remove CameraAccessory
model_8 <- lm(formula = total_gmv ~ Avg_discount_per + 
                Avg_mrp + cod + number_sale_day + TV + Sponsorship + 
                Content_Marketing + SEM + Radio + Other + product_analytic_vertical_Binoculars + 
                product_analytic_vertical_CameraBag + 
                product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                product_analytic_vertical_Lens + product_analytic_vertical_Strap + 
                product_analytic_vertical_Telescope, 
              data = train_cam)
summary(model_8)
vif(model_8)


#remove Strap
model_9 <- lm(formula = total_gmv ~ Avg_discount_per + 
                Avg_mrp + cod + number_sale_day + TV + Sponsorship + 
                Content_Marketing + SEM + Radio + Other + product_analytic_vertical_Binoculars + 
                product_analytic_vertical_CameraBag + 
                product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                product_analytic_vertical_Lens + 
                product_analytic_vertical_Telescope, 
              data = train_cam)
summary(model_9)
vif(model_9)


#remove Avg_discount_per
model_10 <- lm(formula = total_gmv ~ Avg_mrp + cod + number_sale_day + TV + Sponsorship + 
                 Content_Marketing + SEM + Radio + Other + product_analytic_vertical_Binoculars + 
                 product_analytic_vertical_CameraBag + 
                 product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                 product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                 product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                 product_analytic_vertical_Lens + 
                 product_analytic_vertical_Telescope, 
               data = train_cam)
summary(model_10)
vif(model_10)



#remove Radio, other
model_11 <- lm(formula = total_gmv ~ Avg_mrp + cod + number_sale_day + TV + Sponsorship + 
                 Content_Marketing + SEM + product_analytic_vertical_Binoculars + 
                 product_analytic_vertical_CameraBag + 
                 product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                 product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                 product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                 product_analytic_vertical_Lens + 
                 product_analytic_vertical_Telescope, 
               data = train_cam)
summary(model_11)
vif(model_11)


#remove TV
model_12 <- lm(formula = total_gmv ~ Avg_mrp + cod + number_sale_day + Sponsorship + 
                 Content_Marketing + SEM + product_analytic_vertical_Binoculars + 
                 product_analytic_vertical_CameraBag + 
                 product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                 product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                 product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                 product_analytic_vertical_Lens + 
                 product_analytic_vertical_Telescope, 
               data = train_cam)
summary(model_12)
vif(model_12)


#remove Content Marketing
model_13 <- lm(formula = total_gmv ~ Avg_mrp + cod + number_sale_day + Sponsorship + 
                 SEM + product_analytic_vertical_Binoculars + 
                 product_analytic_vertical_CameraBag + 
                 product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                 product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                 product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                 product_analytic_vertical_Lens + 
                 product_analytic_vertical_Telescope, 
               data = train_cam)
summary(model_13)
vif(model_13)


#remove SEM
model_14 <- lm(formula = total_gmv ~ Avg_mrp + cod + number_sale_day + Sponsorship + 
                 product_analytic_vertical_Binoculars + 
                 product_analytic_vertical_CameraBag + 
                 product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                 product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                 product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                 product_analytic_vertical_Lens + 
                 product_analytic_vertical_Telescope, 
               data = train_cam)
summary(model_14)
vif(model_14)


#remove number_sale_day
model_15 <- lm(formula = total_gmv ~ Avg_mrp + cod + Sponsorship + 
                 product_analytic_vertical_Binoculars + 
                 product_analytic_vertical_CameraBag + 
                 product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                 product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                 product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                 product_analytic_vertical_Lens + 
                 product_analytic_vertical_Telescope, 
               data = train_cam)
summary(model_15)
vif(model_15)



#remove Telescope
model_16 <- lm(formula = total_gmv ~ Avg_mrp + cod + Sponsorship + 
                 product_analytic_vertical_Binoculars + 
                 product_analytic_vertical_CameraBag + 
                 product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                 product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_CameraTripod + 
                 product_analytic_vertical_Filter + product_analytic_vertical_Flash + 
                 product_analytic_vertical_Lens, 
               data = train_cam) # 0.94
summary(model_16)
vif(model_16)



Final_Model_Cam <- model_16


## Checking the acuracy of the Test model
Predict_ca <- predict(Final_Model_Cam,test_cam[,-1])
test_cam$predict_gmv <- Predict_ca

r <- cor(test_cam$total_gmv,test_cam$predict_gmv)
rsquared <- cor(test_cam$total_gmv,test_cam$predict_gmv)^2
rsquared # 0.92


# Cross Validation
cv.lm(data = Sales_Camera, form.lm = model_16, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 
#MSE : 0.135

#_______________________________________________________________________________________________________________________
#Gaming Accessory 

Sales_Gaming_mul <- Sales_Gaming
Sales_Gaming_mul[c(1:5,14)]<- NULL

Sales_Gam_ln <- as.data.frame(log1p(Sales_Gaming_mul))
Sales_Gam_ln <- Sales_Gam_ln[complete.cases(Sales_Gam_ln),]


set.seed(100)
indices= sample(1:nrow(Sales_Gam_ln), 0.7*nrow(Sales_Gam_ln))

train_gam <- Sales_Gam_ln[indices,]
test_gam <- Sales_Gam_ln[-indices,]


modelga_1 <-lm(total_gmv~.,data=train_gam)
summary(modelga_1)

#Stepwise Approach
modelga_2 <- stepAIC(modelga_1, direction="both")
summary(modelga_2)
vif(modelga_2)


#remove GamingAdapter

modelga_3 <- lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                  Avg_mrp + cod + number_sale_day + TV + Digital + Sponsorship + 
                  Content_Marketing + Online_Marketing + SEM + Radio + product_analytic_vertical_GamePad + 
                  product_analytic_vertical_GamingAccessoryKit + 
                  product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                  product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                  product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                  product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamingChargingStation, 
                data = train_gam)
summary(modelga_3)
vif(modelga_3)



#remove number_sale_day, Avg_discount_per 
modelga_4 <- lm(formula = total_gmv ~ Avg_list_price + 
                  Avg_mrp + cod + TV + Digital + Sponsorship + 
                  Content_Marketing + Online_Marketing + SEM + Radio + product_analytic_vertical_GamePad + 
                  product_analytic_vertical_GamingAccessoryKit + 
                  product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                  product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                  product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                  product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamingChargingStation, 
                data = train_gam)
summary(modelga_4)
vif(modelga_4)


#remove Avg_mrp
modelga_5 <- lm(formula = total_gmv ~ Avg_list_price + 
                  cod + TV + Digital + Sponsorship + 
                  Content_Marketing + Online_Marketing + SEM + Radio + product_analytic_vertical_GamePad + 
                  product_analytic_vertical_GamingAccessoryKit + 
                  product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                  product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                  product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                  product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamingChargingStation, 
                data = train_gam)
summary(modelga_5)
vif(modelga_5)


#remove GamingChargingStation
modelga_6 <- lm(formula = total_gmv ~ Avg_list_price + 
                  cod + TV + Digital + Sponsorship + 
                  Content_Marketing + Online_Marketing + SEM + Radio + product_analytic_vertical_GamePad + 
                  product_analytic_vertical_GamingAccessoryKit + 
                  product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                  product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                  product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                  product_analytic_vertical_TVOutCableAccessory, 
                data = train_gam)
summary(modelga_6)
vif(modelga_6)


#remove cod
modelga_7 <- lm(formula = total_gmv ~ Avg_list_price + 
                  TV + Digital + Sponsorship + 
                  Content_Marketing + Online_Marketing + SEM + Radio + product_analytic_vertical_GamePad + 
                  product_analytic_vertical_GamingAccessoryKit + 
                  product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                  product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                  product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                  product_analytic_vertical_TVOutCableAccessory, 
                data = train_gam)
summary(modelga_7)
vif(modelga_7)


#remove Online_Marketing
modelga_8 <- lm(formula = total_gmv ~ Avg_list_price + 
                  TV + Digital + Sponsorship + 
                  Content_Marketing + SEM + Radio + product_analytic_vertical_GamePad + 
                  product_analytic_vertical_GamingAccessoryKit + 
                  product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                  product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                  product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                  product_analytic_vertical_TVOutCableAccessory, 
                data = train_gam)
summary(modelga_8)
vif(modelga_8)


#remove TV
modelga_9 <- lm(formula = total_gmv ~ Avg_list_price + 
                  Digital + Sponsorship + 
                  Content_Marketing + SEM + Radio + product_analytic_vertical_GamePad + 
                  product_analytic_vertical_GamingAccessoryKit + 
                  product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                  product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                  product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                  product_analytic_vertical_TVOutCableAccessory, 
                data = train_gam)
summary(modelga_9)
vif(modelga_9)


#remove Content_Marketing
modelga_10 <- lm(formula = total_gmv ~ Avg_list_price + 
                   Digital + Sponsorship + SEM + Radio + product_analytic_vertical_GamePad + 
                   product_analytic_vertical_GamingAccessoryKit + 
                   product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                   product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                   product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                   product_analytic_vertical_TVOutCableAccessory, 
                 data = train_gam)
summary(modelga_10)
vif(modelga_10)


#remove SEM
modelga_11 <- lm(formula = total_gmv ~ Avg_list_price + 
                   Digital + Sponsorship + Radio + product_analytic_vertical_GamePad + 
                   product_analytic_vertical_GamingAccessoryKit + 
                   product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                   product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                   product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                   product_analytic_vertical_TVOutCableAccessory, 
                 data = train_gam)
summary(modelga_11)
vif(modelga_11)



#remove Radio
modelga_12 <- lm(formula = total_gmv ~ Avg_list_price + 
                   Digital + Sponsorship + product_analytic_vertical_GamePad + 
                   product_analytic_vertical_GamingAccessoryKit + 
                   product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                   product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                   product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                   product_analytic_vertical_TVOutCableAccessory, 
                 data = train_gam)
summary(modelga_12)
vif(modelga_12)



#remove Digital
modelga_13 <- lm(formula = total_gmv ~ Avg_list_price + 
                   Sponsorship + product_analytic_vertical_GamePad + 
                   product_analytic_vertical_GamingAccessoryKit + 
                   product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                   product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                   product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                   product_analytic_vertical_TVOutCableAccessory, 
                 data = train_gam)
summary(modelga_13)
vif(modelga_13) #0.9095


Final_Model_Gam <- modelga_13


## Checking the acuracy of the Test model
Predict_ga <- predict(Final_Model_Gam,test_gam[,-1])
test_gam$predict_gmv <- Predict_ga

r <- cor(test_gam$total_gmv,test_gam$predict_gmv)
rsquared <- cor(test_gam$total_gmv,test_gam$predict_gmv)^2
rsquared #0.927


# Cross Validation
cv.lm(data = Sales_Gaming, form.lm = modelga_13, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 
#MSE : 0.0793

#_______________________________________________________________________________________________________________________
#Home Audio

Sales_Home_mul <- Sales_HomeAudio
Sales_Home_mul[c(1:5,14)]<- NULL

Sales_Hom_ln <- as.data.frame(log1p(Sales_Home_mul))
Sales_Hom_ln <- Sales_Hom_ln[complete.cases(Sales_Hom_ln),]


set.seed(100)
indices= sample(1:nrow(Sales_Hom_ln), 0.7*nrow(Sales_Hom_ln))

train_hom <- Sales_Hom_ln[indices,]
test_hom <- Sales_Hom_ln[-indices,]


modelha_1 <-lm(total_gmv~.,data=train_hom)
summary(modelha_1)

#Stepwise Approach
modelha_2 <- stepAIC(modelha_1, direction="both")
summary(modelha_2)
vif(modelha_2)


#remove Avg_list_price 
modelha_3 <- lm(formula = total_gmv ~ cod + TV + Digital + 
                  Sponsorship + Online_Marketing + Affiliates + product_analytic_vertical_BoomBox + 
                  product_analytic_vertical_Dock + product_analytic_vertical_DockingStation + 
                  product_analytic_vertical_FMRadio + product_analytic_vertical_HomeAudioSpeaker + 
                  product_analytic_vertical_VoiceRecorder, data = train_hom)
summary(modelha_3)
vif(modelha_3)


#remove Digital
modelha_4 <- lm(formula = total_gmv ~ cod + TV + 
                  Sponsorship + Online_Marketing + Affiliates + product_analytic_vertical_BoomBox + 
                  product_analytic_vertical_Dock + product_analytic_vertical_DockingStation + 
                  product_analytic_vertical_FMRadio + product_analytic_vertical_HomeAudioSpeaker + 
                  product_analytic_vertical_VoiceRecorder, data = train_hom)
summary(modelha_4)
vif(modelha_4)



#remove cod
modelha_5 <- lm(formula = total_gmv ~ TV + 
                  Sponsorship + Online_Marketing + Affiliates + product_analytic_vertical_BoomBox + 
                  product_analytic_vertical_Dock + product_analytic_vertical_DockingStation + 
                  product_analytic_vertical_FMRadio + product_analytic_vertical_HomeAudioSpeaker + 
                  product_analytic_vertical_VoiceRecorder, data = train_hom)
summary(modelha_5)
vif(modelha_5)



#remove Online_Marketing
modelha_6 <- lm(formula = total_gmv ~ TV + 
                  Sponsorship + Affiliates + product_analytic_vertical_BoomBox + 
                  product_analytic_vertical_Dock + product_analytic_vertical_DockingStation + 
                  product_analytic_vertical_FMRadio + product_analytic_vertical_HomeAudioSpeaker + 
                  product_analytic_vertical_VoiceRecorder, data = train_hom)
summary(modelha_6)
vif(modelha_6)



#remove Affiliates
modelha_7 <- lm(formula = total_gmv ~ TV + 
                  Sponsorship + product_analytic_vertical_BoomBox + 
                  product_analytic_vertical_Dock + product_analytic_vertical_DockingStation + 
                  product_analytic_vertical_FMRadio + product_analytic_vertical_HomeAudioSpeaker + 
                  product_analytic_vertical_VoiceRecorder, data = train_hom)
summary(modelha_7)
vif(modelha_7)


#remove Dock
modelha_8 <- lm(formula = total_gmv ~ TV + 
                  Sponsorship + product_analytic_vertical_BoomBox + 
                  product_analytic_vertical_DockingStation + 
                  product_analytic_vertical_FMRadio + product_analytic_vertical_HomeAudioSpeaker + 
                  product_analytic_vertical_VoiceRecorder, data = train_hom)
summary(modelha_8)
vif(modelha_8)



#remove DockingStation
modelha_9 <- lm(formula = total_gmv ~ TV + 
                  Sponsorship + product_analytic_vertical_BoomBox + 
                  product_analytic_vertical_FMRadio + product_analytic_vertical_HomeAudioSpeaker + 
                  product_analytic_vertical_VoiceRecorder, data = train_hom)
summary(modelha_9) # 0.9493
vif(modelha_9)


Final_Model_hom <- modelha_9


## Checking the acuracy of the Test model
Predictha_1 <- predict(Final_Model_hom,test_hom[,-1])
test_hom$predict_gmv <- Predictha_1

r <- cor(test_hom$total_gmv,test_hom$predict_gmv)
rsquared <- cor(test_hom$total_gmv,test_hom$predict_gmv)^2
rsquared #0.9642961


# Cross Validation
cv.lm(data = Sales_HomeAudio, form.lm = modelha_9, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE) 
#MSE : 1.02

#-----------------------------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Distributed Lag Model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------------------------------------------------------------------------------------------------

#_______________________________________________________________________________________________________________________
# GamingAccessory

Sales_Gaming_mul_lag <- Sales_Data_Final[Sales_Data_Final$product_analytic_sub_category== 'GamingAccessory',]
Sales_Gaming_mul_lag<-Sales_Gaming_mul_lag[order(Sales_Gaming_mul_lag$Year,Sales_Gaming_mul_lag$Month,Sales_Gaming_mul_lag$Week),]

# Lag of dependent and independent variables
Mul_lag_gam <- slide(Sales_Gaming_mul_lag, Var = "total_gmv" , NewVar = 'total_gmv_lag' ,slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Avg_units" , NewVar = 'Avg_units_lag',slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "number_sale_day" , NewVar = 'number_sale_day_lag' ,slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "TV" , NewVar = 'TV_lag',slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Digital" , NewVar = 'Digital_lag',slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Sponsorship" , NewVar = 'Sponsorship_lag',slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Content_Marketing" , NewVar = 'Content_Marketing_lag',slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Online_Marketing" , NewVar = 'Online_Marketing_lag' ,slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Affiliates" , NewVar = 'Affiliates_lag' ,slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "SEM" , NewVar = 'SEM_lag' ,slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Radio" , NewVar = 'Radio_lag' ,slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Other" , NewVar = 'Other_lag' ,slideBy = -2)


Sales_Gaming_mul_lag <- na.omit(Mul_lag_gam)


Sales_Gaming_mul_lag <- fastDummies::dummy_cols(Sales_Gaming_mul_lag, select_columns = "product_analytic_vertical")


Sales_Gaming_mul_lag[c(1:5,14)]<- NULL


set.seed(100)
indices1= sample(1:nrow(Sales_Gaming_mul_lag), 0.7*nrow(Sales_Gaming_mul_lag))

train_gam_lag <- Sales_Gaming_mul_lag[indices1,]
test_gam_lag <- Sales_Gaming_mul_lag[-indices1,]


model_gam_lag <-lm(total_gmv~.,data=train_gam_lag)
summary(model_gam_lag) # Adjusted R-squared:  0.781


#perform StepAIC
model_gam_lag_2 <- stepAIC(model_gam_lag, direction="both")
summary(model_gam_lag_2)
vif(model_gam_lag_2) #Adjusted R-squared:  0.785

#Remove product_analytic_vertical_CoolingPad

model_gam_lag_3 <- lm(formula = total_gmv ~ Avg_list_price + cod + prepaid + Sponsorship + 
                        Content_Marketing + Online_Marketing + Affiliates + SEM + 
                        NPS_Score + total_gmv_lag + Digital_lag + Content_Marketing_lag + 
                        Radio_lag + Other_lag + product_analytic_vertical_GamingAdapter + 
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit ,
                      data = train_gam_lag)

summary(model_gam_lag_3)
vif(model_gam_lag_3)



#remove Online_Marketing

model_gam_lag_4 <- lm(formula = total_gmv ~ Avg_list_price + cod + prepaid + Sponsorship + 
                        Content_Marketing +  Affiliates + SEM + 
                        NPS_Score + total_gmv_lag + Digital_lag + Content_Marketing_lag + 
                        Radio_lag + Other_lag + product_analytic_vertical_GamingAdapter + 
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit ,
                      data = train_gam_lag)

summary(model_gam_lag_4)
vif(model_gam_lag_4)



#remove Affiliates
model_gam_lag_5 <- lm(formula = total_gmv ~ Avg_list_price + cod + prepaid + Sponsorship + 
                        Content_Marketing  + SEM + 
                        NPS_Score + total_gmv_lag + Digital_lag + Content_Marketing_lag + 
                        Radio_lag + Other_lag + product_analytic_vertical_GamingAdapter + 
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit ,
                      data = train_gam_lag)

summary(model_gam_lag_5)
vif(model_gam_lag_5)


#remove GamingAdapter
model_gam_lag_6 <- lm(formula = total_gmv ~ Avg_list_price + cod + prepaid + Sponsorship + 
                        Content_Marketing  + SEM + 
                        NPS_Score + total_gmv_lag + Digital_lag + Content_Marketing_lag + 
                        Radio_lag + Other_lag +
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit ,
                      data = train_gam_lag)
summary(model_gam_lag_6)
vif(model_gam_lag_6)


#remove total_gmv_lag

model_gam_lag_7 <- lm(formula = total_gmv ~ Avg_list_price + cod + prepaid + Sponsorship + 
                        Content_Marketing  + SEM + 
                        NPS_Score + Digital_lag + Content_Marketing_lag + 
                        Radio_lag + Other_lag +
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit ,
                      data = train_gam_lag)

summary(model_gam_lag_7)
vif(model_gam_lag_7)


#remove TVOutCableAccessory
model_gam_lag_8 <- lm(formula = total_gmv ~ Avg_list_price + cod + prepaid + Sponsorship + 
                        Content_Marketing  + SEM + 
                        NPS_Score + Digital_lag + Content_Marketing_lag + 
                        Radio_lag + Other_lag +
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit ,
                      data = train_gam_lag)

summary(model_gam_lag_8)
vif(model_gam_lag_8)


#remove GamingMousePad

model_gam_lag_9 <- lm(formula = total_gmv ~ Avg_list_price + cod + prepaid + Sponsorship + 
                        Content_Marketing  + SEM + 
                        NPS_Score + Digital_lag + Content_Marketing_lag + 
                        Radio_lag + Other_lag +
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit ,
                      data = train_gam_lag)
summary(model_gam_lag_9)
vif(model_gam_lag_9)


#remove GamingMemoryCard

model_gam_lag_10 <- lm(formula = total_gmv ~ Avg_list_price + cod + prepaid + Sponsorship + 
                         Content_Marketing  + SEM + 
                         NPS_Score + Digital_lag + Content_Marketing_lag + 
                         Radio_lag + Other_lag +
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_JoystickGamingWheel + 
                         product_analytic_vertical_GamePad + 
                         product_analytic_vertical_GamingAccessoryKit ,
                       data = train_gam_lag)

summary(model_gam_lag_10)
vif(model_gam_lag_10)


#remove Avg_list_price

model_gam_lag_11 <- lm(formula = total_gmv ~  cod + prepaid + Sponsorship + 
                         Content_Marketing  + SEM + 
                         NPS_Score + Digital_lag + Content_Marketing_lag + 
                         Radio_lag + Other_lag +
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_JoystickGamingWheel + 
                         product_analytic_vertical_GamePad + 
                         product_analytic_vertical_GamingAccessoryKit ,
                       data = train_gam_lag)

summary(model_gam_lag_11)
vif(model_gam_lag_11)

# GamingAccessoryKit

model_gam_lag_12 <- lm(formula = total_gmv ~  cod + prepaid + Sponsorship + 
                         Content_Marketing  + SEM + 
                         NPS_Score + Digital_lag + Content_Marketing_lag + 
                         Radio_lag + Other_lag +
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_JoystickGamingWheel + 
                         product_analytic_vertical_GamePad  ,
                       data = train_gam_lag)

summary(model_gam_lag_12)
vif(model_gam_lag_12)


# remove JoystickGamingWheel
model_gam_lag_13 <- lm(formula = total_gmv ~  cod + prepaid + Sponsorship + 
                         Content_Marketing  + SEM + 
                         NPS_Score + Digital_lag + Content_Marketing_lag + 
                         Radio_lag + Other_lag +
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad  ,
                       data = train_gam_lag)

summary(model_gam_lag_13)
vif(model_gam_lag_13)

# remove SEM
model_gam_lag_14 <- lm(formula = total_gmv ~  cod + prepaid + Sponsorship + 
                         Content_Marketing  +  
                         NPS_Score + Digital_lag + Content_Marketing_lag + 
                         Radio_lag + Other_lag +
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad  ,
                       data = train_gam_lag)

summary(model_gam_lag_14)
vif(model_gam_lag_14)



# remove Content MArketing
model_gam_lag_15 <- lm(formula = total_gmv ~  cod + prepaid + Sponsorship + 
                         NPS_Score + Digital_lag + Content_Marketing_lag + 
                         Radio_lag + Other_lag +
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad  ,
                       data = train_gam_lag)

summary(model_gam_lag_15)
vif(model_gam_lag_15)

# remove Sponsorship
model_gam_lag_16 <- lm(formula = total_gmv ~  cod + prepaid + 
                         NPS_Score + Digital_lag + Content_Marketing_lag + 
                         Radio_lag + Other_lag +
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad  ,
                       data = train_gam_lag)

summary(model_gam_lag_16)
vif(model_gam_lag_16)

# Other_lag

model_gam_lag_17 <- lm(formula = total_gmv ~  cod + prepaid + 
                         NPS_Score + Digital_lag + Content_Marketing_lag + 
                         Radio_lag + 
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad  ,
                       data = train_gam_lag)

summary(model_gam_lag_17)
vif(model_gam_lag_17)

# Radio_lag
model_gam_lag_18 <- lm(formula = total_gmv ~  cod + prepaid + 
                         NPS_Score + Digital_lag + Content_Marketing_lag + 
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad  ,
                       data = train_gam_lag)

summary(model_gam_lag_18)
vif(model_gam_lag_18)

# Digital_lag
model_gam_lag_19 <- lm(formula = total_gmv ~  cod + prepaid + 
                         NPS_Score  + Content_Marketing_lag + 
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad  ,
                       data = train_gam_lag)

summary(model_gam_lag_19)
vif(model_gam_lag_19) #Adjusted R-squared:  0.766

Final_gam_lag <- model_gam_lag_19

## Checking the acuracy of the Test model
Predict_gam_lag <- predict(Final_gam_lag,test_gam_lag[,-1])
test_gam_lag$predict_gmv_lag <- Predict_gam_lag

r <- cor(test_gam_lag$total_gmv,test_gam_lag$predict_gmv_lag)
rsquared <- cor(test_gam_lag$total_gmv,test_gam_lag$predict_gmv_lag)^2
rsquared # 0.708


cv.lm(data = Sales_Gaming_mul_lag, form.lm = Final_gam_lag, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
#MS
#0.0622
#______________________________________________________________________________________________________________________
#Home Audio 

Sales_Home_mul_lag <- Sales_Data_Final[Sales_Data_Final$product_analytic_sub_category== 'HomeAudio',]
Sales_Home_mul_lag<-Sales_Home_mul_lag[order(Sales_Home_mul_lag$Year,Sales_Home_mul_lag$Month,Sales_Home_mul_lag$Week),]


# Lag of dependent and independent variables
Mul_lag_hom <- slide(Sales_Home_mul_lag, Var = "total_gmv" , NewVar = 'total_gmv_lag' ,slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Avg_units" , NewVar = 'Avg_units_lag',slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "number_sale_day" , NewVar = 'number_sale_day_lag' ,slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "TV" , NewVar = 'TV_lag',slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Digital" , NewVar = 'Digital_lag',slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Sponsorship" , NewVar = 'Sponsorship_lag',slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Content_Marketing" , NewVar = 'Content_Marketing_lag',slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Online_Marketing" , NewVar = 'Online_Marketing_lag' ,slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Affiliates" , NewVar = 'Affiliates_lag' ,slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "SEM" , NewVar = 'SEM_lag' ,slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Radio" , NewVar = 'Radio_lag' ,slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Other" , NewVar = 'Other_lag' ,slideBy = -2)


Sales_Home_mul_lag <- na.omit(Mul_lag_hom)

Sales_Home_mul_lag <- fastDummies::dummy_cols(Sales_Home_mul_lag, select_columns = "product_analytic_vertical")
Sales_Home_mul_lag[c(1:5,14)]<- NULL

set.seed(100)
indices1= sample(1:nrow(Sales_Home_mul_lag), 0.7*nrow(Sales_Home_mul_lag))

train_hom_lag <- Sales_Home_mul_lag[indices1,]
test_hom_lag <- Sales_Home_mul_lag[-indices1,]


model_hom_lag_1 <-lm(total_gmv~.,data=train_hom_lag)
summary(model_hom_lag_1) #Adjusted R-squared:  0.774


#Stepwise Approach
model_hom_lag_2 <- stepAIC(model_hom_lag_1, direction="both")
summary(model_hom_lag_2)
vif(model_hom_lag_2)


#remove Sponsorship

model_hom_lag_3 <- lm(formula = total_gmv ~ Avg_discount_per + cod + prepaid +  
                        Content_Marketing + SEM + NPS_Score + TV_lag + Online_Marketing_lag + 
                        Radio_lag + Other_lag + product_analytic_vertical_FMRadio + 
                        product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                        product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                        product_analytic_vertical_DockingStation, data = train_hom_lag)


summary(model_hom_lag_3)
vif(model_hom_lag_3)


#remove SEM

model_hom_lag_4 <- lm(formula = total_gmv ~ Avg_discount_per + cod + prepaid +  
                        Content_Marketing  + NPS_Score + TV_lag + Online_Marketing_lag + 
                        Radio_lag + Other_lag + product_analytic_vertical_FMRadio + 
                        product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                        product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                        product_analytic_vertical_DockingStation, data = train_hom_lag)



summary(model_hom_lag_4)
vif(model_hom_lag_4)


#remove Other_lag

model_hom_lag_5 <- lm(formula = total_gmv ~ Avg_discount_per + cod + prepaid +  
                        Content_Marketing  + NPS_Score + TV_lag + Online_Marketing_lag + 
                        Radio_lag + product_analytic_vertical_FMRadio + 
                        product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                        product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                        product_analytic_vertical_DockingStation, data = train_hom_lag)

summary(model_hom_lag_5)
vif(model_hom_lag_5)


#remove Radio_lag

model_hom_lag_6 <- lm(formula = total_gmv ~ Avg_discount_per + cod + prepaid +  
                        Content_Marketing  + NPS_Score + TV_lag + Online_Marketing_lag + 
                        product_analytic_vertical_FMRadio + 
                        product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                        product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                        product_analytic_vertical_DockingStation, data = train_hom_lag)


summary(model_hom_lag_6)
vif(model_hom_lag_6)



#remove TV_lag

model_hom_lag_7 <- lm(formula = total_gmv ~ Avg_discount_per + cod + prepaid +  
                        Content_Marketing  + NPS_Score  + Online_Marketing_lag + 
                        product_analytic_vertical_FMRadio + 
                        product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                        product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                        product_analytic_vertical_DockingStation, data = train_hom_lag)
summary(model_hom_lag_7)
vif(model_hom_lag_7)


#remove Online_Marketing_lag
model_hom_lag_8 <- lm(formula = total_gmv ~ Avg_discount_per + cod + prepaid +  
                        Content_Marketing  + NPS_Score  + 
                        product_analytic_vertical_FMRadio + 
                        product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                        product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                        product_analytic_vertical_DockingStation, data = train_hom_lag)

summary(model_hom_lag_8)
vif(model_hom_lag_8)


#remove vertical_FMRadio
model_hom_lag_9 <- lm(formula = total_gmv ~ Avg_discount_per + cod + prepaid +  
                        Content_Marketing  + NPS_Score  + 
                        product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                        product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                        product_analytic_vertical_DockingStation, data = train_hom_lag)


summary(model_hom_lag_9)
vif(model_hom_lag_9)

# remove prepaid
model_hom_lag_10 <- lm(formula = total_gmv ~ Avg_discount_per + cod +  
                         Content_Marketing  + NPS_Score  + 
                         product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                         product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                         product_analytic_vertical_DockingStation, data = train_hom_lag)

summary(model_hom_lag_10)
vif(model_hom_lag_10)


# remove NPS_Score
model_hom_lag_11 <- lm(formula = total_gmv ~ Avg_discount_per + cod +  
                         Content_Marketing  + 
                         product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                         product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                         product_analytic_vertical_DockingStation, data = train_hom_lag)


summary(model_hom_lag_11)
vif(model_hom_lag_11) 


# remove Content_Marketing
model_hom_lag_12 <- lm(formula = total_gmv ~ Avg_discount_per + cod +  
                         product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                         product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                         product_analytic_vertical_DockingStation, data = train_hom_lag)


summary(model_hom_lag_12)
vif(model_hom_lag_12) 


# DJController


model_hom_lag_13 <- lm(formula = total_gmv ~ Avg_discount_per + cod +  
                         product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                         product_analytic_vertical_Dock + 
                         product_analytic_vertical_DockingStation, data = train_hom_lag)


summary(model_hom_lag_13)
vif(model_hom_lag_13)


# DockingStation


model_hom_lag_14 <- lm(formula = total_gmv ~ Avg_discount_per + cod +  
                         product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                         product_analytic_vertical_Dock , data = train_hom_lag)


summary(model_hom_lag_14)
vif(model_hom_lag_14)


# VoiceRecorder


model_hom_lag_15 <- lm(formula = total_gmv ~ Avg_discount_per + cod +  
                         product_analytic_vertical_HomeAudioSpeaker +  
                         product_analytic_vertical_Dock , data = train_hom_lag)


summary(model_hom_lag_15)
vif(model_hom_lag_15) #Adjusted R-squared:  0.763

Final_hom_lag <- model_hom_lag_15


## Checking the acuracy of the Test model
Predict_hom_lag <- predict(Final_hom_lag,test_hom_lag[,-1])
test_hom_lag$predict_gmv_lag <- Predict_hom_lag

r <- cor(test_hom_lag$total_gmv,test_hom_lag$predict_gmv_lag)
rsquared <- cor(test_hom_lag$total_gmv,test_hom_lag$predict_gmv_lag)^2
rsquared # 0.644

cv.lm(data = Sales_Home_mul_lag, form.lm = model_hom_lag_11, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
#ms : 0.934

#________________________________________________________________________________________________________________________
#CameraAccessory

Sales_Camera_mul_lag <- Sales_Data_Final[Sales_Data_Final$product_analytic_sub_category== 'CameraAccessory',]
Sales_Camera_mul_lag<-Sales_Camera_mul_lag[order(Sales_Camera_mul_lag$Year,Sales_Camera_mul_lag$Month,Sales_Camera_mul_lag$Week),]

# Lag of dependent and independent variables
Mul_lag_cam <- slide(Sales_Camera_mul_lag, Var = "total_gmv" , NewVar = 'total_gmv_lag' ,slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "number_sale_day" , NewVar = 'number_sale_day_lag' ,slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "TV" , NewVar = 'TV_lag',slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Digital" , NewVar = 'Digital_lag',slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Sponsorship" , NewVar = 'Sponsorship_lag',slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Content_Marketing" , NewVar = 'Content_Marketing_lag',slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Online_Marketing" , NewVar = 'Online_Marketing_lag' ,slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Affiliates" , NewVar = 'Affiliates_lag' ,slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "SEM" , NewVar = 'SEM_lag' ,slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Radio" , NewVar = 'Radio_lag' ,slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Other" , NewVar = 'Other_lag' ,slideBy = -2)


Sales_Camera_mul_lag <- na.omit(Mul_lag_cam)

Sales_Camera_mul_lag <- fastDummies::dummy_cols(Sales_Camera_mul_lag, select_columns = "product_analytic_vertical")
Sales_Camera_mul_lag[c(1:5,14)]<- NULL

set.seed(100)
indices= sample(1:nrow(Sales_Camera_mul_lag), 0.7*nrow(Sales_Camera_mul_lag))

train_cam_lag <- Sales_Camera_mul_lag[indices,]
test_cam_lag <- Sales_Camera_mul_lag[-indices,]


model_cam_lag_1 <-lm(total_gmv~.,data=train_cam_lag)
summary(model_cam_lag_1) #Adjusted R-squared:  0.768 

#Stepwise Approach
model_cam_lag_2 <- stepAIC(model_cam_lag_1, direction="both")
summary(model_cam_lag_2)
vif(model_cam_lag_2)


#remove Filter   

model_cam_lag_3 <- lm(formula = total_gmv ~ Avg_discount_per + cod + prepaid + TV + 
                        Digital + SEM + Radio + Other + product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                        product_analytic_vertical_CameraTripod + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Binoculars, data = train_cam_lag)


summary(model_cam_lag_3)
vif(model_cam_lag_3)



#remove Avg_discount_per

model_cam_lag_4 <- lm(formula = total_gmv ~  cod + prepaid + TV + 
                        Digital + SEM + Radio + Other + product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                        product_analytic_vertical_CameraTripod + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Binoculars, data = train_cam_lag)

summary(model_cam_lag_4)
vif(model_cam_lag_4)


#remove CameraBatteryCharger
model_cam_lag_5 <- lm(formula = total_gmv ~  cod + prepaid + TV + 
                        Digital + SEM + Radio + Other + product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + 
                        product_analytic_vertical_CameraTripod + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Binoculars, data = train_cam_lag)

summary(model_cam_lag_5)
vif(model_cam_lag_5)



#remove TV

model_cam_lag_6 <- lm(formula = total_gmv ~  cod + prepaid +  
                        Digital + SEM + Radio + Other + product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + 
                        product_analytic_vertical_CameraTripod + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Binoculars, data = train_cam_lag)

summary(model_cam_lag_6)
vif(model_cam_lag_6)


#remove other

model_cam_lag_7 <- lm(formula = total_gmv ~  cod + prepaid +  
                        Digital + SEM + Radio  + product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + 
                        product_analytic_vertical_CameraTripod + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Binoculars, data = train_cam_lag)

summary(model_cam_lag_7)
vif(model_cam_lag_7)


#remove radio

model_cam_lag_8 <- lm(formula = total_gmv ~  cod + prepaid +  
                        Digital + SEM  + product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + 
                        product_analytic_vertical_CameraTripod + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Binoculars, data = train_cam_lag)

summary(model_cam_lag_8)
vif(model_cam_lag_8)

#remove digital

model_cam_lag_9 <- lm(formula = total_gmv ~  cod + prepaid +  
                        SEM  + product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + 
                        product_analytic_vertical_CameraTripod + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Binoculars, data = train_cam_lag)

summary(model_cam_lag_9)
vif(model_cam_lag_9)

#remove SEM

model_cam_lag_10 <- lm(formula = total_gmv ~  cod + prepaid +  
                         product_analytic_vertical_CameraBag + 
                         product_analytic_vertical_CameraBattery + 
                         product_analytic_vertical_CameraTripod + 
                         product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                         product_analytic_vertical_Binoculars, data = train_cam_lag)

summary(model_cam_lag_10) #Adjusted R-squared:  0.764 
vif(model_cam_lag_10)



Final_cam_lag <- model_cam_lag_10

## Checking the acuracy of the Test model
Predict_cam_lag <- predict(Final_cam_lag,test_cam_lag[,-1])
test_cam_lag$predict_gmv_lag <- Predict_cam_lag

r <- cor(test_cam_lag$total_gmv,test_cam_lag$predict_gmv_lag)
rsquared <- cor(test_cam_lag$total_gmv,test_cam_lag$predict_gmv_lag)^2
rsquared #0.758


cv.lm(data = Sales_Camera_mul_lag, form.lm = Final_cam_lag, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
#ms : 0.141

#------------------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Kyock Model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------------------------------------------------------------------------------------

#_________________________________________________________________________________________________________________________
# Gaming Accessory

#-------------------------------------- Data Prep : Lag Variable ----------------------------------------------

Sales_Gaming<-Sales_Data_Final[Sales_Data_Final$product_analytic_sub_category== 'GamingAccessory',]
Sales_Gaming<-Sales_Gaming[order(Sales_Gaming$Year,Sales_Gaming$Month,Sales_Gaming$Week),]
View(Sales_Gaming)

# GMV Lag variable

Gaming_Koyck <- slide(Sales_Gaming, Var = "total_gmv", NewVar = 'GMV_Lag' ,slideBy = -1)

Gaming_Koyck <- na.omit(Gaming_Koyck)

# Dummy Variable

Gaming_Koyck <- fastDummies::dummy_cols(Gaming_Koyck, select_columns = "product_analytic_vertical")

View(Gaming_Koyck)
Gaming_Koyck$product_analytic_vertical<- NULL

Train_Gaming<-Gaming_Koyck[Gaming_Koyck$Year %in% c(2015,2016) & Gaming_Koyck$Month %in% c(7,8,9,10,11,12,1,2) , ]
Train_Gaming[c(1:4,13)]<- NULL
Test_Gaming<-Gaming_Koyck[Gaming_Koyck$Year == 2016 & Gaming_Koyck$Month %in% c(3,4),]
Test_Gaming[c(1:4,13)]<- NULL
Validation_Gaming <-Gaming_Koyck[Gaming_Koyck$Year == 2016 & Gaming_Koyck$Month %in% c(5,6),]
Validation_Gaming[c(1:4,13)]<- NULL

#------------------------------------------------ model ------------------------------------------------------

View(Train_Gaming)

Gaming_Kyock_model1<-lm(total_gmv~. , data = Train_Gaming)
summary(Gaming_Kyock_model1) #Adjusted R-squared: 0.7156 


# step AIC
step_game_kyock_model1 <- stepAIC(Gaming_Kyock_model1, direction = "both")
summary(step_game_kyock_model1) #Adjusted R-squared:  0.7196
vif<-vif(step_game_kyock_model1)
View(vif)

#GamingAdapter

step_game_kyock_model2 <- lm(formula = total_gmv ~ prepaid + TV + Digital + Sponsorship + 
     Content_Marketing + Online_Marketing + Affiliates + SEM + 
     NPS_Score + GMV_Lag + product_analytic_vertical_GamingAccessoryKit + 
      product_analytic_vertical_GamingHeadset + 
     product_analytic_vertical_GamingKeyboard + product_analytic_vertical_GamingMemoryCard + 
     product_analytic_vertical_GamingMouse + product_analytic_vertical_GamingMousePad + 
     product_analytic_vertical_JoystickGamingWheel + product_analytic_vertical_MotionController + 
     product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad, 
   data = Train_Gaming)

summary(step_game_kyock_model2) #Adjusted R-squared:  0.7186
vif<-vif(step_game_kyock_model2)
View(vif)


#JoystickGamingWheel
step_game_kyock_model3 <- lm(formula = total_gmv ~ prepaid + TV + Digital + Sponsorship + 
                               Content_Marketing + Online_Marketing + Affiliates + SEM + 
                               NPS_Score + GMV_Lag + product_analytic_vertical_GamingAccessoryKit + 
                               product_analytic_vertical_GamingHeadset + 
                               product_analytic_vertical_GamingKeyboard + product_analytic_vertical_GamingMemoryCard + 
                               product_analytic_vertical_GamingMouse + product_analytic_vertical_GamingMousePad + 
                               product_analytic_vertical_MotionController + 
                               product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad, 
                             data = Train_Gaming)

summary(step_game_kyock_model3) #Adjusted R-squared:  0.7186
vif<-vif(step_game_kyock_model3)
View(vif)


#MotionController

step_game_kyock_model4 <- lm(formula = total_gmv ~ prepaid + TV + Digital + Sponsorship + 
                               Content_Marketing + Online_Marketing + Affiliates + SEM + 
                               NPS_Score + GMV_Lag + product_analytic_vertical_GamingAccessoryKit + 
                               product_analytic_vertical_GamingHeadset + 
                               product_analytic_vertical_GamingKeyboard + product_analytic_vertical_GamingMemoryCard + 
                               product_analytic_vertical_GamingMouse + product_analytic_vertical_GamingMousePad + 
                               product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad, 
                             data = Train_Gaming)

summary(step_game_kyock_model4) #Adjusted R-squared:  0.7185
vif<-vif(step_game_kyock_model4)
View(vif)

#TVOutCableAccessory

step_game_kyock_model5 <- lm(formula = total_gmv ~ prepaid + TV + Digital + Sponsorship + 
                               Content_Marketing + Online_Marketing + Affiliates + SEM + 
                               NPS_Score + GMV_Lag + product_analytic_vertical_GamingAccessoryKit + 
                               product_analytic_vertical_GamingHeadset + 
                               product_analytic_vertical_GamingKeyboard + product_analytic_vertical_GamingMemoryCard + 
                               product_analytic_vertical_GamingMouse + product_analytic_vertical_GamingMousePad + 
                               product_analytic_vertical_GamePad, 
                             data = Train_Gaming)

summary(step_game_kyock_model5) #Adjusted R-squared:  0.7176
vif<-vif(step_game_kyock_model5)
View(vif)

#GamingMousePad
step_game_kyock_model6 <- lm(formula = total_gmv ~ prepaid + TV + Digital + Sponsorship + 
                               Content_Marketing + Online_Marketing + Affiliates + SEM + 
                               NPS_Score + GMV_Lag + product_analytic_vertical_GamingAccessoryKit + 
                               product_analytic_vertical_GamingHeadset + 
                               product_analytic_vertical_GamingKeyboard + product_analytic_vertical_GamingMemoryCard + 
                               product_analytic_vertical_GamingMouse +  
                               product_analytic_vertical_GamePad, 
                             data = Train_Gaming)

summary(step_game_kyock_model6) #Adjusted R-squared:  0.7158
vif<-vif(step_game_kyock_model6)
View(vif)

#GamingMemoryCard

step_game_kyock_model7 <- lm(formula = total_gmv ~ prepaid + TV + Digital + Sponsorship + 
                               Content_Marketing + Online_Marketing + Affiliates + SEM + 
                               NPS_Score + GMV_Lag + product_analytic_vertical_GamingAccessoryKit + 
                               product_analytic_vertical_GamingHeadset + 
                               product_analytic_vertical_GamingKeyboard + 
                               product_analytic_vertical_GamingMouse +  
                               product_analytic_vertical_GamePad, 
                             data = Train_Gaming)

summary(step_game_kyock_model7) #Adjusted R-squared:  0.7135
vif<-vif(step_game_kyock_model7)
View(vif)


#GamingAccessoryKit

step_game_kyock_model8 <- lm(formula = total_gmv ~ prepaid + TV + Digital + Sponsorship + 
                               Content_Marketing + Online_Marketing + Affiliates + SEM + 
                               NPS_Score + GMV_Lag + 
                               product_analytic_vertical_GamingHeadset + 
                               product_analytic_vertical_GamingKeyboard + 
                               product_analytic_vertical_GamingMouse +  
                               product_analytic_vertical_GamePad, 
                             data = Train_Gaming)

summary(step_game_kyock_model8) #Adjusted R-squared:  0.7108
vif<-vif(step_game_kyock_model8)
View(vif)

#GMV_Lag

step_game_kyock_model9 <- lm(formula = total_gmv ~ prepaid + TV + Digital + Sponsorship + 
                               Content_Marketing + Online_Marketing + Affiliates + SEM + 
                               NPS_Score +  
                               product_analytic_vertical_GamingHeadset + 
                               product_analytic_vertical_GamingKeyboard + 
                               product_analytic_vertical_GamingMouse +  
                               product_analytic_vertical_GamePad, 
                             data = Train_Gaming)

summary(step_game_kyock_model9) #Adjusted R-squared:  0.7102
vif<-vif(step_game_kyock_model9)
View(vif)

#Sponsorship
step_game_kyock_model10 <- lm(formula = total_gmv ~ prepaid + TV + Digital + 
                               Content_Marketing + Online_Marketing + Affiliates + SEM + 
                               NPS_Score +  
                               product_analytic_vertical_GamingHeadset + 
                               product_analytic_vertical_GamingKeyboard + 
                               product_analytic_vertical_GamingMouse +  
                               product_analytic_vertical_GamePad, 
                             data = Train_Gaming)

summary(step_game_kyock_model10) #Adjusted R-squared:  0.7046
vif<-vif(step_game_kyock_model10)
View(vif)

#Online_Marketing
step_game_kyock_model11 <- lm(formula = total_gmv ~ prepaid + TV + Digital + 
                                Content_Marketing +  Affiliates + SEM + 
                                NPS_Score +  
                                product_analytic_vertical_GamingHeadset + 
                                product_analytic_vertical_GamingKeyboard + 
                                product_analytic_vertical_GamingMouse +  
                                product_analytic_vertical_GamePad, 
                              data = Train_Gaming)

summary(step_game_kyock_model11) #Adjusted R-squared:  0.7009
vif<-vif(step_game_kyock_model11)
View(vif)

#Digital
step_game_kyock_model12 <- lm(formula = total_gmv ~ prepaid +   
                                Content_Marketing +  Affiliates +  SEM +
                                NPS_Score +  TV +
                                product_analytic_vertical_GamingHeadset + 
                                product_analytic_vertical_GamingKeyboard + 
                                product_analytic_vertical_GamingMouse +  
                                product_analytic_vertical_GamePad, 
                              data = Train_Gaming)

summary(step_game_kyock_model12) #Adjusted R-squared:  0.6992
vif<-vif(step_game_kyock_model12)
View(vif)


#SEM
step_game_kyock_model13 <- lm(formula = total_gmv ~ prepaid +   
                                Content_Marketing +  Affiliates +  
                                NPS_Score +  TV +
                                product_analytic_vertical_GamingHeadset + 
                                product_analytic_vertical_GamingKeyboard + 
                                product_analytic_vertical_GamingMouse +  
                                product_analytic_vertical_GamePad, 
                              data = Train_Gaming)

summary(step_game_kyock_model13) #Adjusted R-squared:  0.6993
vif<-vif(step_game_kyock_model13)
View(vif)


#Affiliates
step_game_kyock_model14 <- lm(formula = total_gmv ~ prepaid +   
                                Content_Marketing +  TV + 
                                NPS_Score +  
                                product_analytic_vertical_GamingHeadset + 
                                product_analytic_vertical_GamingKeyboard + 
                                product_analytic_vertical_GamingMouse +  
                                product_analytic_vertical_GamePad, 
                              data = Train_Gaming)

summary(step_game_kyock_model14) #Adjusted R-squared:  0.6997
vif<-vif(step_game_kyock_model14)
View(vif)

#TV
step_game_kyock_model15 <- lm(formula = total_gmv ~ prepaid +   
                                Content_Marketing + 
                                NPS_Score +  
                                product_analytic_vertical_GamingHeadset + 
                                product_analytic_vertical_GamingKeyboard + 
                                product_analytic_vertical_GamingMouse +  
                                product_analytic_vertical_GamePad, 
                              data = Train_Gaming)

summary(step_game_kyock_model15) #Adjusted R-squared:  0.7003
vif<-vif(step_game_kyock_model15)
View(vif)

# Test
Test_game_kyock_model <- lm(formula = total_gmv ~ prepaid +   
                                Content_Marketing + 
                                NPS_Score +  
                                product_analytic_vertical_GamingHeadset + 
                                product_analytic_vertical_GamingKeyboard + 
                                product_analytic_vertical_GamingMouse +  
                                product_analytic_vertical_GamePad, 
                              data = Test_Gaming)

summary(Test_game_kyock_model) #Adjusted R-squared:  0.84


Predict_game_kyock <- predict(step_game_kyock_model15,Test_Gaming[,-1])
Test_Gaming$predict_gmv_lag <- Predict_game_kyock

r <- cor(Test_Gaming$total_gmv,Test_Gaming$predict_gmv_lag)
rsquared <- cor(Test_Gaming$total_gmv,Test_Gaming$predict_gmv_lag)^2
rsquared # 0.48


cv.lm(data = Gaming_Koyck, form.lm = step_game_kyock_model15 , m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
#ms : 0.068
#______________________________________________________________________________________________________________________
# Camera Accessory


#-------------------------------------- Data Prep : Lag Variable ----------------------------------------------

Sales_Camera<-Sales_Data_Final[Sales_Data_Final$product_analytic_sub_category== 'CameraAccessory',]
Sales_Camera<-Sales_Camera[order(Sales_Camera$Year,Sales_Camera$Month,Sales_Camera$Week),]
View(Sales_Camera)

# GMV Lag variable

Camera_Koyck <- slide( Sales_Camera , Var = "total_gmv", NewVar = 'GMV_Lag' ,slideBy = -1)

Camera_Koyck <- na.omit(Camera_Koyck)

# Dummy Variable

Camera_Koyck <- fastDummies::dummy_cols(Camera_Koyck, select_columns = "product_analytic_vertical")

Camera_Koyck$product_analytic_vertical<- NULL

View(Camera_Koyck)

Train_Camera<-Camera_Koyck[Camera_Koyck$Year %in% c(2015,2016) & Camera_Koyck$Month %in% c(7,8,9,10,11,12,1,2) , ]
Train_Camera[c(1:4,13)]<- NULL
Test_Camera<-Camera_Koyck[Camera_Koyck$Year == 2016 & Camera_Koyck$Month %in% c(3,4),]
Test_Camera[c(1:4,13)]<- NULL
Validation_Camera <-Camera_Koyck[Camera_Koyck$Year == 2016 & Camera_Koyck$Month %in% c(5,6),]
Validation_Camera[c(1:4,13)]<- NULL

#----------------------------------- model ---------------------------------------------------------------------

Camera_Kyock_model1<-lm(total_gmv~. , data = Train_Camera)
summary(Camera_Kyock_model1) #Adjusted R-squared: 0.7438


# step AIC
step_camera_kyock_model1 <- stepAIC(Camera_Kyock_model1, direction = "both")
summary(step_camera_kyock_model1) #Adjusted R-squared:  0.7477
vif<-vif(step_camera_kyock_model1)
View(vif)

# Online_Marketing
Camera_Kyock_model2<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
     prepaid + TV + Digital + Content_Marketing + 
     Affiliates + SEM + NPS_Score + GMV_Lag + product_analytic_vertical_CameraBattery + 
     product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
     product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
     product_analytic_vertical_Lens + product_analytic_vertical_Telescope + 
     product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
     product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
     product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model2) #Adjusted R-squared:  0.747
vif<-vif(Camera_Kyock_model2)
View(vif)

# Digital
Camera_Kyock_model3<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                          prepaid + TV +  Content_Marketing + 
                          Affiliates + SEM + NPS_Score + GMV_Lag + product_analytic_vertical_CameraBattery + 
                          product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                          product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                          product_analytic_vertical_Lens + product_analytic_vertical_Telescope + 
                          product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                          product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                          product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model3) #Adjusted R-squared:  0.747
vif<-vif(Camera_Kyock_model3)
View(vif)


# TV
Camera_Kyock_model4<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                          prepaid  +  Content_Marketing + 
                          Affiliates + SEM + NPS_Score + GMV_Lag + product_analytic_vertical_CameraBattery + 
                          product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                          product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                          product_analytic_vertical_Lens + product_analytic_vertical_Telescope + 
                          product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                          product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                          product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model4) #Adjusted R-squared:  0.747
vif<-vif(Camera_Kyock_model4)
View(vif)

# SEM
Camera_Kyock_model5<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                          prepaid  +  Content_Marketing + 
                          Affiliates +  NPS_Score + GMV_Lag + product_analytic_vertical_CameraBattery + 
                          product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                          product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                          product_analytic_vertical_Lens + product_analytic_vertical_Telescope + 
                          product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                          product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                          product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model5) #Adjusted R-squared:  0.747
vif<-vif(Camera_Kyock_model5)
View(vif)

# Affiliates
Camera_Kyock_model6<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                          prepaid  +  Content_Marketing + 
                          NPS_Score + GMV_Lag + product_analytic_vertical_CameraBattery + 
                          product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                          product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                          product_analytic_vertical_Lens + product_analytic_vertical_Telescope + 
                          product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                          product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                          product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model6) #Adjusted R-squared:  0.747
vif<-vif(Camera_Kyock_model6)
View(vif)


# Content_Marketing
Camera_Kyock_model7<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                          prepaid  +  
                          NPS_Score + GMV_Lag + product_analytic_vertical_CameraBattery + 
                          product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                          product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                          product_analytic_vertical_Lens + product_analytic_vertical_Telescope + 
                          product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                          product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone + 
                          product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model7) #Adjusted R-squared:  0.747
vif<-vif(Camera_Kyock_model7)
View(vif)


#CameraEyeCup

Camera_Kyock_model8<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                          prepaid  +  
                          NPS_Score + GMV_Lag + product_analytic_vertical_CameraBattery + 
                          product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                          product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                          product_analytic_vertical_Lens + product_analytic_vertical_Telescope + 
                          product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                          product_analytic_vertical_CameraMicrophone + 
                          product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model8) #Adjusted R-squared:  0.7458
vif<-vif(Camera_Kyock_model8)
View(vif)



#NPS_Score

Camera_Kyock_model9<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                          prepaid  +  
                          GMV_Lag + product_analytic_vertical_CameraBattery + 
                          product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                          product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                          product_analytic_vertical_Lens + product_analytic_vertical_Telescope + 
                          product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                          product_analytic_vertical_CameraMicrophone + 
                          product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model9) #Adjusted R-squared:  0.7414
vif<-vif(Camera_Kyock_model9)
View(vif)


#GMV_Lag

Camera_Kyock_model10<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                          prepaid  +  
                          product_analytic_vertical_CameraBattery + 
                          product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                          product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                          product_analytic_vertical_Lens + product_analytic_vertical_Telescope + 
                          product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                          product_analytic_vertical_CameraMicrophone + 
                          product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model10) #Adjusted R-squared:  0.7401
vif<-vif(Camera_Kyock_model10)
View(vif)

#Telescope
Camera_Kyock_model11<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                           prepaid  +  
                           product_analytic_vertical_CameraBattery + 
                           product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                           product_analytic_vertical_CameraRemoteControl + product_analytic_vertical_Filter + 
                           product_analytic_vertical_Lens + 
                           product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                           product_analytic_vertical_CameraMicrophone + 
                           product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model11) #Adjusted R-squared:  0.7389


#Filter
Camera_Kyock_model12<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                           prepaid  +  
                           product_analytic_vertical_CameraBattery + 
                           product_analytic_vertical_CameraBatteryGrip + product_analytic_vertical_CameraMount + 
                           product_analytic_vertical_CameraRemoteControl + 
                           product_analytic_vertical_Lens + 
                           product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                           product_analytic_vertical_CameraMicrophone + 
                           product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model12) #Adjusted R-squared:  0.7374


#CameraBatteryGrip

Camera_Kyock_model13<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                           prepaid  +  
                           product_analytic_vertical_CameraBattery + 
                           product_analytic_vertical_CameraMount + 
                           product_analytic_vertical_CameraRemoteControl + 
                           product_analytic_vertical_Lens + 
                           product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                           product_analytic_vertical_CameraMicrophone + 
                           product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model13) #Adjusted R-squared:  0.7359


#CameraMicrophone

Camera_Kyock_model14<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                           prepaid  +  
                           product_analytic_vertical_CameraBattery + 
                           product_analytic_vertical_CameraMount + 
                           product_analytic_vertical_CameraRemoteControl + 
                           product_analytic_vertical_Lens + 
                           product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                           product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model14)#Adjusted R-squared:  0.7345

#Avg_list_price
Camera_Kyock_model15<-lm(formula = total_gmv ~ Avg_discount_per + 
                           prepaid  +  
                           product_analytic_vertical_CameraBattery + 
                           product_analytic_vertical_CameraMount + 
                           product_analytic_vertical_CameraRemoteControl + 
                           product_analytic_vertical_Lens + 
                           product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                           product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model15)#Adjusted R-squared:  0.7329


#CameraMount
Camera_Kyock_model16<-lm(formula = total_gmv ~ Avg_discount_per + 
                           prepaid  +  
                           product_analytic_vertical_CameraBattery + 
                           product_analytic_vertical_CameraRemoteControl + 
                           product_analytic_vertical_Lens + 
                           product_analytic_vertical_Binoculars + product_analytic_vertical_ExtensionTube + 
                           product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model16)#Adjusted R-squared:  0.731

#ExtensionTube

Camera_Kyock_model17<-lm(formula = total_gmv ~ Avg_discount_per + 
                           prepaid  +  
                           product_analytic_vertical_CameraBattery + 
                           product_analytic_vertical_CameraRemoteControl + 
                           product_analytic_vertical_Lens + 
                           product_analytic_vertical_Binoculars +  
                           product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model17)#Adjusted R-squared:  0.7283

#CameraRemoteControl

Camera_Kyock_model18<-lm(formula = total_gmv ~ Avg_discount_per + 
                           prepaid  +  product_analytic_vertical_CameraBattery + 
                           product_analytic_vertical_Lens + 
                           product_analytic_vertical_Binoculars +  
                           product_analytic_vertical_CameraBag, data = Train_Camera)
summary(Camera_Kyock_model18)#Adjusted R-squared:  0.7247


# Predict
Predict_Camera_kyock <- predict(Camera_Kyock_model18,Test_Camera[,-1])
Test_Camera$predict_gmv_lag <- Predict_Camera_kyock

r <- cor(Test_Camera$total_gmv,Test_Camera$predict_gmv_lag)
rsquared <- cor(Test_Camera$total_gmv,Test_Camera$predict_gmv_lag)^2
rsquared # 0.44

cv.lm(data = Camera_Koyck, form.lm = Camera_Kyock_model18, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
#  MS : 0.165
#____________________________________________________________________________________________________________________
# Home Audio
#-------------------------------------- Data Prep : Lag Variable ----------------------------------------------

Sales_HomeAudio<-Sales_Data_Final[Sales_Data_Final$product_analytic_sub_category== 'HomeAudio',]
Sales_HomeAudio<-Sales_HomeAudio[order(Sales_HomeAudio$Year,Sales_HomeAudio$Month,Sales_HomeAudio$Week),]
View(Sales_HomeAudio)

# GMV Lag variable

HomeAudio_Koyck <- slide( Sales_HomeAudio , Var = "total_gmv", NewVar = 'GMV_Lag' ,slideBy = -1)

HomeAudio_Koyck <- na.omit(HomeAudio_Koyck)

# Dummy Variable

HomeAudio_Koyck <- fastDummies::dummy_cols(HomeAudio_Koyck, select_columns = "product_analytic_vertical")

View(HomeAudio_Koyck)

HomeAudio_Koyck$product_analytic_vertical<- NULL

Train_HomeAudio<-HomeAudio_Koyck[HomeAudio_Koyck$Year %in% c(2015,2016) & HomeAudio_Koyck$Month %in% c(7,8,9,10,11,12,1,2) , ]
Train_HomeAudio[c(1:4,13)]<- NULL
Test_HomeAudio<-HomeAudio_Koyck[HomeAudio_Koyck$Year == 2016 & HomeAudio_Koyck$Month %in% c(3,4),]
Test_HomeAudio[c(1:4,13)]<- NULL
Validation_HomeAudio <-HomeAudio_Koyck[HomeAudio_Koyck$Year == 2016 & HomeAudio_Koyck$Month %in% c(5,6),]
Validation_HomeAudio[c(1:4,13)]<- NULL

#------------------------------------------- model ------------------------------------------------------------

HomeAudio_kyock_model1<-lm(total_gmv~. , data = Train_HomeAudio)
summary(HomeAudio_kyock_model1) #Adjusted R-squared:  0.7183


# step AIC
step_homeaudio_model1 <- stepAIC(HomeAudio_kyock_model1, direction = "both")
summary(step_homeaudio_model1) #Adjusted R-squared:  0.7246
vif<- vif(step_homeaudio_model1)
View(vif)

#Content_Marketing
HomeAudio_kyock_model2<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
     cod + TV + Digital +  Online_Marketing + 
     Affiliates + SEM + NPS_Score + product_analytic_vertical_Dock + 
     product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
     product_analytic_vertical_DockingStation, data = Train_HomeAudio)
summary(HomeAudio_kyock_model2) #Adjusted R-squared:  0.7236
vif<- vif(HomeAudio_kyock_model2)
View(vif)

#Digital
HomeAudio_kyock_model3<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                             cod + TV +   Online_Marketing + 
                             Affiliates + SEM + NPS_Score + product_analytic_vertical_Dock + 
                             product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                             product_analytic_vertical_DockingStation, data = Train_HomeAudio)
summary(HomeAudio_kyock_model3) #Adjusted R-squared:  0.7242
vif<- vif(HomeAudio_kyock_model3)
View(vif)

# TV
HomeAudio_kyock_model4<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                             cod +  Online_Marketing + 
                             Affiliates + SEM + NPS_Score + product_analytic_vertical_Dock + 
                             product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                             product_analytic_vertical_DockingStation, data = Train_HomeAudio)
summary(HomeAudio_kyock_model4) #Adjusted R-squared:  0.725
vif<- vif(HomeAudio_kyock_model4)
View(vif)

# SEM
HomeAudio_kyock_model4<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                             cod +  Online_Marketing + 
                               NPS_Score + product_analytic_vertical_Dock + 
                             product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                             product_analytic_vertical_DockingStation, data = Train_HomeAudio)
summary(HomeAudio_kyock_model4) #Adjusted R-squared:  0.725
vif<- vif(HomeAudio_kyock_model4)
View(vif)

# Online Marketing
HomeAudio_kyock_model5<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                             cod +  
                             NPS_Score + product_analytic_vertical_Dock + 
                             product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                             product_analytic_vertical_DockingStation, data = Train_HomeAudio)
summary(HomeAudio_kyock_model5) #Adjusted R-squared:  0.7269
vif<- vif(HomeAudio_kyock_model5)
View(vif)


# nps score
HomeAudio_kyock_model6<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                             cod +  
                             product_analytic_vertical_Dock + 
                             product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                             product_analytic_vertical_DockingStation, data = Train_HomeAudio)
summary(HomeAudio_kyock_model6) #Adjusted R-squared:  0.7267
vif<- vif(HomeAudio_kyock_model6)
View(vif)

# product_analytic_vertical_VoiceRecorder
HomeAudio_kyock_model7<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                             cod +  
                             product_analytic_vertical_Dock + 
                             product_analytic_vertical_HomeAudioSpeaker +  
                             product_analytic_vertical_DockingStation, data = Train_HomeAudio)
summary(HomeAudio_kyock_model7) #Adjusted R-squared:  0.7251
vif<- vif(HomeAudio_kyock_model7)
View(vif)


# product_analytic_vertical_DockingStation
HomeAudio_kyock_model8<-lm(formula = total_gmv ~ Avg_list_price + Avg_discount_per + 
                             cod +  
                             product_analytic_vertical_Dock + 
                             product_analytic_vertical_HomeAudioSpeaker , data = Train_HomeAudio)
summary(HomeAudio_kyock_model8) #Adjusted R-squared:  0.7225
vif<- vif(HomeAudio_kyock_model8)
View(vif)

# Avg list price
HomeAudio_kyock_model9<-lm(formula = total_gmv ~  Avg_discount_per + 
                             cod +  
                             product_analytic_vertical_Dock + 
                             product_analytic_vertical_HomeAudioSpeaker , data = Train_HomeAudio)
summary(HomeAudio_kyock_model9) #Adjusted R-squared:  0.7198
vif<- vif(HomeAudio_kyock_model9)
View(vif)


# Predict
Predict_Home_kyock <- predict(HomeAudio_kyock_model9,Test_HomeAudio[,-1])
Test_HomeAudio$predict_gmv_lag <- Predict_Home_kyock

r <- cor(Test_HomeAudio$total_gmv,Test_HomeAudio$predict_gmv_lag)
rsquared <- cor(Test_HomeAudio$total_gmv,Test_HomeAudio$predict_gmv_lag)^2
rsquared # 0.77


cv.lm(data = HomeAudio_Koyck, form.lm = HomeAudio_kyock_model9, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
#  MS : 0.969

#------------------------------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Multiplicative -Distributed Lag >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------------------------------------------------------------------------------------------------

#_______________________________________________________________________________________________________________________
# GamingAccessory

Sales_Gaming_mul_lag <- Sales_Data_Final[Sales_Data_Final$product_analytic_sub_category== 'GamingAccessory',]
Sales_Gaming_mul_lag<-Sales_Gaming_mul_lag[order(Sales_Gaming_mul_lag$Year,Sales_Gaming_mul_lag$Month,Sales_Gaming_mul_lag$Week),]

# Lag of dependent and independent variables
Mul_lag_gam <- slide(Sales_Gaming_mul_lag, Var = "total_gmv" , NewVar = 'total_gmv_lag' ,slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Avg_units" , NewVar = 'Avg_units_lag',slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "number_sale_day" , NewVar = 'number_sale_day_lag' ,slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "TV" , NewVar = 'TV_lag',slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Digital" , NewVar = 'Digital_lag',slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Sponsorship" , NewVar = 'Sponsorship_lag',slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Content_Marketing" , NewVar = 'Content_Marketing_lag',slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Online_Marketing" , NewVar = 'Online_Marketing_lag' ,slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Affiliates" , NewVar = 'Affiliates_lag' ,slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "SEM" , NewVar = 'SEM_lag' ,slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Radio" , NewVar = 'Radio_lag' ,slideBy = -2)
Mul_lag_gam <- slide(Mul_lag_gam, Var = "Other" , NewVar = 'Other_lag' ,slideBy = -2)


Sales_Gaming_mul_lag <- na.omit(Mul_lag_gam)

# Dummy Variables
Sales_Gaming_mul_lag <- fastDummies::dummy_cols(Sales_Gaming_mul_lag, select_columns = "product_analytic_vertical")


Sales_Gaming_mul_lag[c(1:5,14)]<- NULL

# Log of data set
Sales_Gaming_mul_lag <- as.data.frame(log1p(Sales_Gaming_mul_lag))
Sales_Gaming_mul_lag <- Sales_Gaming_mul_lag[complete.cases(Sales_Gaming_mul_lag),]


set.seed(100)
indices1= sample(1:nrow(Sales_Gaming_mul_lag), 0.7*nrow(Sales_Gaming_mul_lag))

train_gam_lag <- Sales_Gaming_mul_lag[indices1,]
test_gam_lag <- Sales_Gaming_mul_lag[-indices1,]


model_gam_lag <-lm(total_gmv~.,data=train_gam_lag)
summary(model_gam_lag) # Adjusted R-squared:  0.937


#perform StepAIC
model_gam_lag_2 <- stepAIC(model_gam_lag, direction="both")
summary(model_gam_lag_2)
vif(model_gam_lag_2) 

#Remove number_sale_day_lag

model_gam_lag_3 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + Avg_discount_per + 
                        Avg_mrp + cod + number_sale_day + Digital + Sponsorship + 
                        Content_Marketing + Online_Marketing + SEM + Radio + total_gmv_lag + 
                        TV_lag + product_analytic_vertical_GamingAdapter + 
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)

summary(model_gam_lag_3)
vif(model_gam_lag_3)



#remove number_sale_day

model_gam_lag_4 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + Avg_discount_per + 
                        Avg_mrp + cod +  Digital + Sponsorship + 
                        Content_Marketing + Online_Marketing + SEM + Radio + total_gmv_lag + 
                        TV_lag + product_analytic_vertical_GamingAdapter + 
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)

summary(model_gam_lag_4)
vif(model_gam_lag_4)



#remove Avg_mrp
model_gam_lag_5 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + Avg_discount_per + 
                        cod +  Digital + Sponsorship + 
                        Content_Marketing + Online_Marketing + SEM + Radio + total_gmv_lag + 
                        TV_lag + product_analytic_vertical_GamingAdapter + 
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)

summary(model_gam_lag_5)
vif(model_gam_lag_5)


#remove Avg_discount_per 
model_gam_lag_6 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + 
                        cod +  Digital + Sponsorship + 
                        Content_Marketing + Online_Marketing + SEM + Radio + total_gmv_lag + 
                        TV_lag + product_analytic_vertical_GamingAdapter + 
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_GamingMousePad + product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)
summary(model_gam_lag_6)
vif(model_gam_lag_6)


#remove GamingMousePad

model_gam_lag_7 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + 
                        cod +  Digital + Sponsorship + 
                        Content_Marketing + Online_Marketing + SEM + Radio + total_gmv_lag + 
                        TV_lag + product_analytic_vertical_GamingAdapter + 
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_TVOutCableAccessory + product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)

summary(model_gam_lag_7)
vif(model_gam_lag_7)


#remove TVOutCableAccessory
model_gam_lag_8 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + 
                        cod +  Digital + Sponsorship + 
                        Content_Marketing + Online_Marketing + SEM + Radio + total_gmv_lag + 
                        TV_lag + product_analytic_vertical_GamingAdapter + 
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_JoystickGamingWheel + 
                        product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)

summary(model_gam_lag_8)
vif(model_gam_lag_8)


#remove JoystickGamingWheel

model_gam_lag_9 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + 
                        cod +  Digital + Sponsorship + 
                        Content_Marketing + Online_Marketing + SEM + Radio + total_gmv_lag + 
                        TV_lag + product_analytic_vertical_GamingAdapter + 
                        product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                        product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                        product_analytic_vertical_GamePad + 
                        product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)
summary(model_gam_lag_9)
vif(model_gam_lag_9)


#remove Digital

model_gam_lag_10 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + 
                         cod +   Sponsorship + 
                         Content_Marketing + Online_Marketing + SEM + Radio + total_gmv_lag + 
                         TV_lag + product_analytic_vertical_GamingAdapter + 
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad + 
                         product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)

summary(model_gam_lag_10)
vif(model_gam_lag_10)


#remove GamingAdapter

model_gam_lag_11 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + 
                         cod +   Sponsorship + 
                         Content_Marketing + Online_Marketing + SEM + Radio + total_gmv_lag + 
                         TV_lag +  
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad + 
                         product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)
summary(model_gam_lag_11)
vif(model_gam_lag_11)

# Content_Marketing 

model_gam_lag_12 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + 
                         cod +   Sponsorship + 
                         Online_Marketing + SEM + Radio + total_gmv_lag + 
                         TV_lag +  
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad + 
                         product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)

summary(model_gam_lag_12)
vif(model_gam_lag_12)


# remove SEM
model_gam_lag_13 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + 
                         cod +   Sponsorship + 
                         Online_Marketing +  Radio + total_gmv_lag + 
                         TV_lag +  
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad + 
                         product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)

summary(model_gam_lag_13)
vif(model_gam_lag_13)

# remove Online_Marketing
model_gam_lag_14 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + 
                         cod +   Sponsorship + 
                         Radio + total_gmv_lag + 
                         TV_lag +  
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad + 
                         product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)

summary(model_gam_lag_14)
vif(model_gam_lag_14)



# remove Radio
model_gam_lag_15 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + 
                         cod +   Sponsorship + 
                         total_gmv_lag + 
                         TV_lag +  
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad + 
                         product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)

summary(model_gam_lag_15)
vif(model_gam_lag_15)

# remove TV_LAg
model_gam_lag_16 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + 
                         cod +   Sponsorship + 
                         total_gmv_lag + 
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad + 
                         product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)

summary(model_gam_lag_16) #Adjusted R-squared:  0.921
vif(model_gam_lag_16)


# remove Total_gmv_lag
model_gam_lag_17 <- lm(formula = total_gmv ~ Avg_units + Avg_list_price + 
                         cod +   Sponsorship + 
                         product_analytic_vertical_GamingHeadset + product_analytic_vertical_GamingKeyboard + 
                         product_analytic_vertical_GamingMemoryCard + product_analytic_vertical_GamingMouse + 
                         product_analytic_vertical_GamePad + 
                         product_analytic_vertical_GamingAccessoryKit, data = train_gam_lag)

summary(model_gam_lag_17) #Adjusted R-squared:   0.6416 
vif(model_gam_lag_17)


Final_gam_lag <- model_gam_lag_17

## Checking the acuracy of the Test model
Predict_gam_lag <- predict(Final_gam_lag,test_gam_lag[,-1])
test_gam_lag$predict_gmv_lag <- Predict_gam_lag

r <- cor(test_gam_lag$total_gmv,test_gam_lag$predict_gmv_lag)
rsquared <- cor(test_gam_lag$total_gmv,test_gam_lag$predict_gmv_lag)^2
rsquared # 0.5922


cv.lm(data = Sales_Gaming_mul_lag, form.lm = Final_gam_lag, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
#MS
#1.27
#______________________________________________________________________________________________________________________
#Home Audio 

Sales_Home_mul_lag <- Sales_Data_Final[Sales_Data_Final$product_analytic_sub_category== 'HomeAudio',]
Sales_Home_mul_lag<-Sales_Home_mul_lag[order(Sales_Home_mul_lag$Year,Sales_Home_mul_lag$Month,Sales_Home_mul_lag$Week),]


# Lag of dependent and independent variables
Mul_lag_hom <- slide(Sales_Home_mul_lag, Var = "total_gmv" , NewVar = 'total_gmv_lag' ,slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Avg_units" , NewVar = 'Avg_units_lag',slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "number_sale_day" , NewVar = 'number_sale_day_lag' ,slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "TV" , NewVar = 'TV_lag',slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Digital" , NewVar = 'Digital_lag',slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Sponsorship" , NewVar = 'Sponsorship_lag',slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Content_Marketing" , NewVar = 'Content_Marketing_lag',slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Online_Marketing" , NewVar = 'Online_Marketing_lag' ,slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Affiliates" , NewVar = 'Affiliates_lag' ,slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "SEM" , NewVar = 'SEM_lag' ,slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Radio" , NewVar = 'Radio_lag' ,slideBy = -2)
Mul_lag_hom <- slide(Mul_lag_hom, Var = "Other" , NewVar = 'Other_lag' ,slideBy = -2)


Sales_Home_mul_lag <- na.omit(Mul_lag_hom)

# Dummy Variable
Sales_Home_mul_lag <- fastDummies::dummy_cols(Sales_Home_mul_lag, select_columns = "product_analytic_vertical")
Sales_Home_mul_lag[c(1:5,14)]<- NULL

# Log od data set
Sales_Home_mul_lag <- as.data.frame(log1p(Sales_Home_mul_lag))
Sales_Home_mul_lag <- Sales_Home_mul_lag[complete.cases(Sales_Home_mul_lag),]

set.seed(100)
indices1= sample(1:nrow(Sales_Home_mul_lag), 0.7*nrow(Sales_Home_mul_lag))

train_hom_lag <- Sales_Home_mul_lag[indices1,]
test_hom_lag <- Sales_Home_mul_lag[-indices1,]


model_hom_lag_1 <-lm(total_gmv~.,data=train_hom_lag)
summary(model_hom_lag_1) #Adjusted R-squared:  0.976


#Stepwise Approach
model_hom_lag_2 <- stepAIC(model_hom_lag_1, direction="both")
summary(model_hom_lag_2)
vif(model_hom_lag_2)


#remove Sponsorship

model_hom_lag_3 <- lm(formula = total_gmv ~ Avg_list_price + cod + prepaid + Digital + 
                        Content_Marketing + Online_Marketing + Affiliates + 
                        product_analytic_vertical_FMRadio + product_analytic_vertical_HomeAudioSpeaker + 
                        product_analytic_vertical_VoiceRecorder + product_analytic_vertical_BoomBox + 
                        product_analytic_vertical_Dock + product_analytic_vertical_DockingStation, 
                      data = train_hom_lag)


summary(model_hom_lag_3)
vif(model_hom_lag_3)


#remove Digital

model_hom_lag_4 <- lm(formula = total_gmv ~ Avg_list_price + cod + prepaid +  
                        Content_Marketing + Online_Marketing + Affiliates + 
                        product_analytic_vertical_FMRadio + product_analytic_vertical_HomeAudioSpeaker + 
                        product_analytic_vertical_VoiceRecorder + product_analytic_vertical_BoomBox + 
                        product_analytic_vertical_Dock + product_analytic_vertical_DockingStation, 
                      data = train_hom_lag)



summary(model_hom_lag_4)
vif(model_hom_lag_4)


#remove Other_lag

model_hom_lag_5 <- lm(formula = total_gmv ~ Avg_list_price + cod + prepaid +  
                        Content_Marketing + Online_Marketing + Affiliates + 
                        product_analytic_vertical_FMRadio + product_analytic_vertical_HomeAudioSpeaker + 
                        product_analytic_vertical_VoiceRecorder + product_analytic_vertical_BoomBox + 
                        product_analytic_vertical_Dock + product_analytic_vertical_DockingStation, 
                      data = train_hom_lag)

summary(model_hom_lag_5)
vif(model_hom_lag_5)


#remove Content_Marketing

model_hom_lag_6 <- lm(formula = total_gmv ~ Avg_discount_per + cod + prepaid +  
                        NPS_Score + TV_lag + Online_Marketing_lag + 
                        product_analytic_vertical_FMRadio + 
                        product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                        product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                        product_analytic_vertical_DockingStation, data = train_hom_lag)


summary(model_hom_lag_6)
vif(model_hom_lag_6)



#remove NPS_Score

model_hom_lag_7 <- lm(formula = total_gmv ~ Avg_discount_per + cod + prepaid +  
                        TV_lag + Online_Marketing_lag + 
                        product_analytic_vertical_FMRadio + 
                        product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                        product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                        product_analytic_vertical_DockingStation, data = train_hom_lag)
summary(model_hom_lag_7)
vif(model_hom_lag_7)


#remove Avg_discount_per
model_hom_lag_8 <- lm(formula = total_gmv ~  cod + prepaid +  
                        TV_lag + Online_Marketing_lag + 
                        product_analytic_vertical_FMRadio + 
                        product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                        product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                        product_analytic_vertical_DockingStation, data = train_hom_lag)

summary(model_hom_lag_8)
vif(model_hom_lag_8)


#remove TV_lag
model_hom_lag_9 <- lm(formula = total_gmv ~  cod + prepaid +  
                        Online_Marketing_lag + 
                        product_analytic_vertical_FMRadio + 
                        product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                        product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                        product_analytic_vertical_DockingStation, data = train_hom_lag)


summary(model_hom_lag_9)
vif(model_hom_lag_9)

# remove cod
model_hom_lag_10 <- lm(formula = total_gmv ~   prepaid +  
                         Online_Marketing_lag + 
                         product_analytic_vertical_FMRadio + 
                         product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                         product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                         product_analytic_vertical_DockingStation, data = train_hom_lag)

summary(model_hom_lag_10)
vif(model_hom_lag_10)


# remove Online_Marketing_lag
model_hom_lag_11 <- lm(formula = total_gmv ~   prepaid +  
                         product_analytic_vertical_FMRadio + 
                         product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                         product_analytic_vertical_DJController + product_analytic_vertical_Dock + 
                         product_analytic_vertical_DockingStation, data = train_hom_lag)

summary(model_hom_lag_11)
vif(model_hom_lag_11) 


# remove DJController
model_hom_lag_12 <- lm(formula = total_gmv ~   prepaid +  
                         product_analytic_vertical_FMRadio + 
                         product_analytic_vertical_HomeAudioSpeaker + product_analytic_vertical_VoiceRecorder + 
                         product_analytic_vertical_Dock + 
                         product_analytic_vertical_DockingStation, data = train_hom_lag)


summary(model_hom_lag_12) #Adjusted R-squared:  0.835
vif(model_hom_lag_12) 


Final_hom_lag <- model_hom_lag_12


## Checking the acuracy of the Test model
Predict_hom_lag <- predict(Final_hom_lag,test_hom_lag[,-1])
test_hom_lag$predict_gmv_lag <- Predict_hom_lag

r <- cor(test_hom_lag$total_gmv,test_hom_lag$predict_gmv_lag)
rsquared <- cor(test_hom_lag$total_gmv,test_hom_lag$predict_gmv_lag)^2
rsquared # 0.852

cv.lm(data = Sales_Home_mul_lag, form.lm = model_hom_lag_11, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
#ms : 0.599

#________________________________________________________________________________________________________________________
#CameraAccessory

Sales_Camera_mul_lag <- Sales_Data_Final[Sales_Data_Final$product_analytic_sub_category== 'CameraAccessory',]
Sales_Camera_mul_lag<-Sales_Camera_mul_lag[order(Sales_Camera_mul_lag$Year,Sales_Camera_mul_lag$Month,Sales_Camera_mul_lag$Week),]

# Lag of dependent and independent variables
Mul_lag_cam <- slide(Sales_Camera_mul_lag, Var = "total_gmv" , NewVar = 'total_gmv_lag' ,slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "number_sale_day" , NewVar = 'number_sale_day_lag' ,slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "TV" , NewVar = 'TV_lag',slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Digital" , NewVar = 'Digital_lag',slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Sponsorship" , NewVar = 'Sponsorship_lag',slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Content_Marketing" , NewVar = 'Content_Marketing_lag',slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Online_Marketing" , NewVar = 'Online_Marketing_lag' ,slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Affiliates" , NewVar = 'Affiliates_lag' ,slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "SEM" , NewVar = 'SEM_lag' ,slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Radio" , NewVar = 'Radio_lag' ,slideBy = -2)
Mul_lag_cam <- slide(Mul_lag_cam, Var = "Other" , NewVar = 'Other_lag' ,slideBy = -2)


Sales_Camera_mul_lag <- na.omit(Mul_lag_cam)

# Dummy Variables
Sales_Camera_mul_lag <- fastDummies::dummy_cols(Sales_Camera_mul_lag, select_columns = "product_analytic_vertical")
Sales_Camera_mul_lag[c(1:5,14)]<- NULL

# Log of data set
Sales_Camera_mul_lag <- as.data.frame(log1p(Sales_Camera_mul_lag))
Sales_Camera_mul_lag <- Sales_Camera_mul_lag[complete.cases(Sales_Camera_mul_lag),]

set.seed(100)
indices= sample(1:nrow(Sales_Camera_mul_lag), 0.7*nrow(Sales_Camera_mul_lag))

train_cam_lag <- Sales_Camera_mul_lag[indices,]
test_cam_lag <- Sales_Camera_mul_lag[-indices,]


model_cam_lag_1 <-lm(total_gmv~.,data=train_cam_lag)
summary(model_cam_lag_1) #Adjusted R-squared:  0.955

#Stepwise Approach
model_cam_lag_2 <- stepAIC(model_cam_lag_1, direction="both")
summary(model_cam_lag_2) # Adjusted R-squared:  0.956
vif(model_cam_lag_2)


#remove Avg_units   

model_cam_lag_3 <- lm(formula = total_gmv ~  Avg_list_price + Avg_mrp + 
                        cod + number_sale_day + TV + Sponsorship + Content_Marketing + 
                        SEM + Radio + Other + total_gmv_lag + Digital_lag + product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                        product_analytic_vertical_CameraMount + product_analytic_vertical_CameraRemoteControl + 
                        product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Telescope + product_analytic_vertical_Binoculars + 
                        product_analytic_vertical_CameraAccessory + product_analytic_vertical_Strap + 
                        product_analytic_vertical_CameraEyeCup + product_analytic_vertical_CameraMicrophone, 
                      data = train_cam_lag)


summary(model_cam_lag_3)
vif(model_cam_lag_3)



#remove CameraMicrophone

model_cam_lag_4 <- lm(formula = total_gmv ~  Avg_list_price + Avg_mrp + 
                        cod + number_sale_day + TV + Sponsorship + Content_Marketing + 
                        SEM + Radio + Other + total_gmv_lag + Digital_lag + product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                        product_analytic_vertical_CameraMount + product_analytic_vertical_CameraRemoteControl + 
                        product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Telescope + product_analytic_vertical_Binoculars + 
                        product_analytic_vertical_CameraAccessory + product_analytic_vertical_Strap + 
                        product_analytic_vertical_CameraEyeCup , 
                      data = train_cam_lag)

summary(model_cam_lag_4)
vif(model_cam_lag_4)


#remove total_gmv_lag
model_cam_lag_5 <- lm(formula = total_gmv ~  Avg_list_price + Avg_mrp + 
                        cod + number_sale_day + TV + Sponsorship + Content_Marketing + 
                        SEM + Radio + Other +  Digital_lag + product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                        product_analytic_vertical_CameraMount + product_analytic_vertical_CameraRemoteControl + 
                        product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Telescope + product_analytic_vertical_Binoculars + 
                        product_analytic_vertical_CameraAccessory + product_analytic_vertical_Strap + 
                        product_analytic_vertical_CameraEyeCup , 
                      data = train_cam_lag)

summary(model_cam_lag_5)
vif(model_cam_lag_5)



#remove Digital_lag

model_cam_lag_6 <- lm(formula = total_gmv ~  Avg_list_price + Avg_mrp + 
                        cod + number_sale_day + TV + Sponsorship + Content_Marketing + 
                        SEM + Radio + Other +   product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                        product_analytic_vertical_CameraMount + product_analytic_vertical_CameraRemoteControl + 
                        product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Telescope + product_analytic_vertical_Binoculars + 
                        product_analytic_vertical_CameraAccessory + product_analytic_vertical_Strap + 
                        product_analytic_vertical_CameraEyeCup , 
                      data = train_cam_lag)

summary(model_cam_lag_6)
vif(model_cam_lag_6)


#remove Avg_list_price

model_cam_lag_7 <- lm(formula = total_gmv ~   Avg_mrp + 
                        cod + number_sale_day + TV + Sponsorship + Content_Marketing + 
                        SEM + Radio + Other +   product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                        product_analytic_vertical_CameraMount + product_analytic_vertical_CameraRemoteControl + 
                        product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Telescope + product_analytic_vertical_Binoculars + 
                        product_analytic_vertical_CameraAccessory + product_analytic_vertical_Strap + 
                        product_analytic_vertical_CameraEyeCup , 
                      data = train_cam_lag)

summary(model_cam_lag_7)
vif(model_cam_lag_7)


#remove Strap

model_cam_lag_8 <- lm(formula = total_gmv ~   Avg_mrp + 
                        cod + number_sale_day + TV + Sponsorship + Content_Marketing + 
                        SEM + Radio + Other +   product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                        product_analytic_vertical_CameraMount + product_analytic_vertical_CameraRemoteControl + 
                        product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Telescope + product_analytic_vertical_Binoculars + 
                        product_analytic_vertical_CameraAccessory + 
                        product_analytic_vertical_CameraEyeCup , 
                      data = train_cam_lag)

summary(model_cam_lag_8)
vif(model_cam_lag_8)

#remove CameraEyeCup

model_cam_lag_9 <- lm(formula = total_gmv ~   Avg_mrp + 
                        cod + number_sale_day + TV + Sponsorship + Content_Marketing + 
                        SEM + Radio + Other +   product_analytic_vertical_CameraBag + 
                        product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                        product_analytic_vertical_CameraMount + product_analytic_vertical_CameraRemoteControl + 
                        product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                        product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                        product_analytic_vertical_Telescope + product_analytic_vertical_Binoculars + 
                        product_analytic_vertical_CameraAccessory  , 
                      data = train_cam_lag)

summary(model_cam_lag_9)
vif(model_cam_lag_9)

#remove CameraAccessory

model_cam_lag_10 <- lm(formula = total_gmv ~   Avg_mrp + 
                         cod + number_sale_day + TV + Sponsorship + Content_Marketing + 
                         SEM + Radio + Other +   product_analytic_vertical_CameraBag + 
                         product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                         product_analytic_vertical_CameraMount + product_analytic_vertical_CameraRemoteControl + 
                         product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                         product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                         product_analytic_vertical_Telescope + product_analytic_vertical_Binoculars, 
                       data = train_cam_lag)


summary(model_cam_lag_10) 
vif(model_cam_lag_10)

# remove CameraMount

model_cam_lag_11 <- lm(formula = total_gmv ~   Avg_mrp + 
                         cod + number_sale_day + TV + Sponsorship + Content_Marketing + 
                         SEM + Radio + Other +   product_analytic_vertical_CameraBag + 
                         product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                         product_analytic_vertical_CameraRemoteControl + 
                         product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                         product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                         product_analytic_vertical_Telescope + product_analytic_vertical_Binoculars, 
                       data = train_cam_lag)


summary(model_cam_lag_11) 
vif(model_cam_lag_11)

# Telescope

model_cam_lag_12 <- lm(formula = total_gmv ~   Avg_mrp + 
                         cod + number_sale_day + TV + Sponsorship + Content_Marketing + 
                         SEM + Radio + Other +   product_analytic_vertical_CameraBag + 
                         product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                         product_analytic_vertical_CameraRemoteControl + 
                         product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                         product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                         product_analytic_vertical_Binoculars, 
                       data = train_cam_lag)


summary(model_cam_lag_12) 
vif(model_cam_lag_12)

# Radio

model_cam_lag_13 <- lm(formula = total_gmv ~   Avg_mrp + 
                         cod + number_sale_day + TV + Sponsorship + Content_Marketing + 
                         SEM +  Other +   product_analytic_vertical_CameraBag + 
                         product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                         product_analytic_vertical_CameraRemoteControl + 
                         product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                         product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                         product_analytic_vertical_Binoculars, 
                       data = train_cam_lag)


summary(model_cam_lag_13) 
vif(model_cam_lag_13)

# SEM

model_cam_lag_14 <- lm(formula = total_gmv ~   Avg_mrp + 
                         cod + number_sale_day + TV + Sponsorship + Content_Marketing + 
                         Other +   product_analytic_vertical_CameraBag + 
                         product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                         product_analytic_vertical_CameraRemoteControl + 
                         product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                         product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                         product_analytic_vertical_Binoculars, 
                       data = train_cam_lag)


summary(model_cam_lag_14) 
vif(model_cam_lag_14)

# Content_Marketing

model_cam_lag_15 <- lm(formula = total_gmv ~   Avg_mrp + 
                         cod + number_sale_day + TV + Sponsorship +  
                         Other +   product_analytic_vertical_CameraBag + 
                         product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                         product_analytic_vertical_CameraRemoteControl + 
                         product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                         product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                         product_analytic_vertical_Binoculars, 
                       data = train_cam_lag)


summary(model_cam_lag_15) 
vif(model_cam_lag_15)

# TV

model_cam_lag_16 <- lm(formula = total_gmv ~   Avg_mrp + 
                         cod + number_sale_day +  Sponsorship +  
                         Other +   product_analytic_vertical_CameraBag + 
                         product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                         product_analytic_vertical_CameraRemoteControl + 
                         product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                         product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                         product_analytic_vertical_Binoculars, 
                       data = train_cam_lag)


summary(model_cam_lag_16) 
vif(model_cam_lag_16)

# number_sale_day

model_cam_lag_17 <- lm(formula = total_gmv ~   Avg_mrp + 
                         cod +   Sponsorship +  
                         Other +   product_analytic_vertical_CameraBag + 
                         product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                         product_analytic_vertical_CameraRemoteControl + 
                         product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                         product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                         product_analytic_vertical_Binoculars, 
                       data = train_cam_lag)


summary(model_cam_lag_17) 
vif(model_cam_lag_17)

#CameraRemoteControl

model_cam_lag_18 <- lm(formula = total_gmv ~   Avg_mrp + 
                         cod +   Sponsorship +  
                         Other +   product_analytic_vertical_CameraBag + 
                         product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                         product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                         product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                         product_analytic_vertical_Binoculars, 
                       data = train_cam_lag)


summary(model_cam_lag_18) 
vif(model_cam_lag_18)


#Avg_mrp

model_cam_lag_19 <- lm(formula = total_gmv ~    
                         cod +   Sponsorship +  
                         Other +   product_analytic_vertical_CameraBag + 
                         product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                         product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                         product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                         product_analytic_vertical_Binoculars, 
                       data = train_cam_lag)


summary(model_cam_lag_19) #Adjusted R-squared:  0.716 
vif(model_cam_lag_19)


#Other

model_cam_lag_20 <- lm(formula = total_gmv ~    
                         cod +   Sponsorship +  
                           product_analytic_vertical_CameraBag + 
                         product_analytic_vertical_CameraBattery + product_analytic_vertical_CameraBatteryCharger + 
                         product_analytic_vertical_CameraTripod + product_analytic_vertical_Filter + 
                         product_analytic_vertical_Flash + product_analytic_vertical_Lens + 
                         product_analytic_vertical_Binoculars, 
                       data = train_cam_lag)


summary(model_cam_lag_20) #Adjusted R-squared:  0.715
vif(model_cam_lag_20)

Final_cam_lag <- model_cam_lag_20

## Checking the acuracy of the Test model
Predict_cam_lag <- predict(Final_cam_lag,test_cam_lag[,-1])
test_cam_lag$predict_gmv_lag <- Predict_cam_lag

r <- cor(test_cam_lag$total_gmv,test_cam_lag$predict_gmv_lag)
rsquared <- cor(test_cam_lag$total_gmv,test_cam_lag$predict_gmv_lag)^2
rsquared #0.703


cv.lm(data = Sales_Camera_mul_lag, form.lm = Final_cam_lag, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
#ms : 1.22

#-------------------------------------------------------------------------------------------------------------------------------
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Final Model >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------------------------------------------------------------------------------------------------------------
#Gaming Accessory : 
#  Distributed lag model,
#  Adjusted R2 : 0.766  & mean square error : 0.0622
#Camera Accessory : 
#  Distributed lag model,
#  Adjusted R2 : 0.764 & mean square error : 0.141
#Home Audio : 
#  Multiplicative - Distributed lag model,
#  Adjusted R2 : 0.835 & mean square error : 0.599


