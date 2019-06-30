# Breif explanation of data frames 
# train_original and test_original are df of mnist_train and mnist_test respectively
# train_subset is created by taking 15% data from train_original 
# train_subset is further divided into train and validation df
# train df is used to build models 
# validation df is used for validation of model
# test_original df is used to test the final model

##############------- file path is working directory ----------#################
##############------- load libraries --------------------------#################

library(ggplot2)
library(caTools)
library(kernlab)
library(e1071)

###################################------- LOAD FILES ----------####################################

train_original<-read.csv("mnist_train.csv" , stringsAsFactors = FALSE , header = F)

test_original<-read.csv("mnist_test.csv" , stringsAsFactors = FALSE , header = F)

dim(train_original)
#59999 obs. of  785 variables

dim(test_original)
#9999 obs. of  785 variables

#_______________________________________________________________________________

# Rename first column to label and covert to factor

names(train_original)[1] <- "label"
train_original$label <- factor(train_original$label)

names(test_original)[1] <- "label"
test_original$label <- factor(test_original$label)

#_______________________________________________________________________________

# Subset train and test due to large size and computation time

set.seed(100)
train_indices <- sample(1: nrow(train_original), .15*nrow(train_original))
train_subset<- train_original[train_indices, ]

dim(train_subset) # 8999 obs. of  785 variables


################################-----EXPLORATORY DATA ANALYSIS- Part 1----####################################

#_________________________________________________________________________________________________
##########------Intensities of labels in original train_orignal df and train_subset

Train_original_plot <- ggplot(train_original, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar()+
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..))))

Train_original_plot 

Train_subset_plot <- ggplot(train_subset, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + 
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..))))

Train_subset_plot

# Intensities of labels are same in train_original and train

Test_original_plot <- ggplot(test_original, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + 
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..))))

Test_original_plot 


# Intensities of labels are same in test_original 




##################################-------- DATA CLEANING -----##############################################

#__________________________________________________________________________________
#Data type
unique(lapply(train_subset, class)) # All variable are int except label
unique(lapply( test_original, class)) # All variable are int  except label

#___________________________________________________________________________________
#Missing values
train_NA<-colnames(train_subset)[colSums(is.na(train_subset)) > 0]
train_NA   # No Missing value

test_NA<-colnames(test_original)[colSums(is.na(test_original)) > 0]
test_NA   # No Missing value

#___________________________________________________________________________________
#Duplicate Values

train_dup<-sum(duplicated(train_subset))
train_dup # no duplicates
test_dup<-sum(duplicated(test_original)) 
test_dup# no duplicates


#check which columns have only one value
train_unique_count<-apply(train_subset,2,function(x) length(unique(x)))
train_unique_count

test_unique_count<-apply(test_original,2,function(x) length(unique(x)))
test_unique_count

# Both Train and Test have many column with just one value
# but since this is a pixel data ranging from 0 to 255
# and each row is number determined by the combination of these pixels
# We will not remove them


#########################-------- Final Train and Validation df ------------########################


set.seed(100)

indices = sample.split(train_subset$label, SplitRatio = 0.7)

train = train_subset[indices,]

validation = train_subset[!(indices),]

#########################----------EXPLORATORY DATA ANALYSIS-PART-2------############################

#_________________________________________________________________________________________________
##########------Intensities of labels in original train and Validation

train_plot <- ggplot(train, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + 
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..))))

train_plot 

validation_plot <- ggplot(validation, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..))))

validation_plot

# intensities same in both train and validation

################################---- MODEL BUILDING & EVALUATION -----####################################

###############################----- LINEAR MODEL -------------------####################################

#______________________________________________________________________________________________________________#
#                                 LINEAR MODEL-1 , C = 1                                                       #

#model
model_1<- ksvm(label ~ ., data = train,scale = FALSE,C=1)
print(model_1)

# Predict
predict_1<- predict(model_1, validation)

# Confusion Matrix 
confusionMatrix_1<-confusionMatrix(predict_1, validation$label)
confusionMatrix_1

#Accuracy :    0.94
#Sensitivity > 0.90
#Specificity > 0.98



#___________________________________________________________________________________________________________________#
#                             LINEAR MODEL-2 , C=10                                                                 #


#model
model_10<- ksvm(label ~ ., data = train,scale = FALSE,C=10)
print(model_10)

# Predict
predict_10<- predict(model_10, validation)

# Confusion Matrix 
confusionMatrix_10<-confusionMatrix(predict_10, validation$label)
confusionMatrix_10


#Accuracy :    0.95
#Sensitivity > 0.93
#Specificity > 0.99

#better Accuracy , Sensitivity ans Specificity than previous model

#__________________________________________________________________________________________________________________#
#                                Linera SVM - Hyperparameter tuning and Cross Validation                           #

set.seed(100)
#______________________________
#1)
# sequence of C values. 
grid_linear1 <- expand.grid(C=c(0.001, 0.1 ,1,10 ,100))

# 5-fold cross validation
cv_linear1 <- train(label ~ ., data = train, metric = "Accuracy", method = "svmLinear",
                    tuneGrid = grid_linear1, preProcess = NULL,
                    trControl = trainControl(method = "cv", number = 5))

print(cv_linear1)
plot(cv_linear1)
#Best tune at C = 0.001 # very low value , model is too simple , can be highly biased
# Accuracy - .90

predict_cv_linear1 <- predict(cv_linear1, newdata = validation)
confusionMatrix(predict_cv_linear1, validation$label)

# Accuracy   = .89 
#Sensitivity > .81
#Specificity > .98


#______________________________
#2)
#another sequence of C values
grid_linear2 <- expand.grid(C=c(1,2,3,4,5,6,7,8,9,10))

# second 5-fold cross validation
cv_linear2 <- train(label ~ ., data = train, metric = "Accuracy", method = "svmLinear",
                     tuneGrid = grid_linear2, preProcess = NULL,
                     trControl = trainControl(method = "cv", number = 5))

print(cv_linear2)
plot(cv_linear2)
#Best tune at C = 1 
# Accuracy - 0.91

predict_cv_linear2 <- predict(cv_linear2, newdata = validation)
confusionMatrix(predict_cv_linear2, validation$label)

# Accuracy -   .89
#Sensitivity > .81
#Specificity > .98


# No differnce in Accuracy  , Sensitivity ,Specificity between two models

#################################----------- POLYNOMIAL KERNEL---------###############################################

#____________________________________________________________________________________________________________________#
#                                         Polynomila SVM Model -1 , C=1 , degree = 2                                 #
#model
svm_model<-  ksvm(label ~ ., data = train, scaled = FALSE, C = 1,kernel = "polydot" , degree = 2)
print(svm_model)
#scale =  1  offset =  1

#predict
predict_cv_poly1 <- predict(svm_model, newdata = validation)

confusionMatrix(predict_cv_poly1, validation$label)

#Accuracy     :   0.90 , better than cv linear SVM
#Sensitivity  >    .83
#Specificity  >    .98



#____________________________________________________________________________________________________________________#
#                                         Polynomila SVM Model -2 , Best Tune                                        #
#model
tune_poly<-tune.svm(label~.,data=train,kernel='polynomial',degree=2)

tune_poly$best.model

#SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  1 
#degree:  2 
#gamma:  0.00127551 
#coef.0:  0 

best_poly<-svm(label~.,train,
               cost=tune_poly$best.model$cost,
               gamma=tune_poly$best.model$gamma,
               coef0=tune_poly$best.model$coef0,
               kernel='polynomial',degree=2)


#predict
predict_poly2 <- predict(best_poly, newdata = validation)

confusionMatrix(predict_poly2, validation$label)


#Accuracy     : 0.96
#Sensitivity  > 0.93
#Specificity  > 0.99


##################################----------- RFB KERNEL -------------################################################

#_______________________________________________________________________________________________________________#
#                                        RFB KERNEL , C=1                                                       #

model_rfb <- ksvm(label~ ., data = train, scale = FALSE, kernel = "rbfdot",C = 1, kpar = "automatic")
print(model_rfb) 
#sigma =  1.636e-07 , C=1

predict_rfb<- predict(model_rfb, validation)

#confusion matrix - RBF Kernel
confusionMatrix(predict_rfb,validation$label)

#Accuracy :    .94
#Sensitivity > .90
#Specificity > .98

# Note : Accuracy,Specificity , Sensitivity greater than tune linear svm but less than tuned polynomial kernel
#_________________________________________________________________________________________________________________#
#                                         RFB KERNEL , Higher Sigma                                               #

model2_rbf <- ksvm(label ~ ., data = train, scaled = FALSE, kernel = "rbfdot",
                   C = 1, kpar = list(sigma = 2e-07))
print(model2_rbf)

predict_rbf2 <- predict(model2_rbf, newdata = validation)
confusionMatrix(predict_rbf2, validation$label) 

#Accuracy    = .95 , Better accuracy than previous rfb model , but still less than best tuned polynomial kernel
#Sensitivity > .91
#Specificity > .99


#__________________________________________________________________________________________________________________#
#                               RFB KERNEL - Hyperparameter tuning and Cross Validation                            #

set.seed(100)
#________________________________
# 2 cross validation

#1)
# sequence of C and sigma values. 
grid_rfb <- expand.grid(sigma=c(1.0e-07,1.6e-07,2e-07,2.5e-07), C=c(0.1,0.5,1,2))

# Performing 2-fold cross validation
cv_rfb1 <- train(label ~ ., data = train, metric = "Accuracy", method = "svmRadial",
                    tuneGrid = grid_rfb, preProcess = NULL,
                    trControl = trainControl(method = "cv", number = 2))

print(cv_rfb1)
plot(cv_rfb1)
#sigma = 2.5e-07 and C = 2


predict_rbf<- predict(cv_rfb1, newdata = validation)
confusionMatrix(predict_rbf, validation$label)

#Accuracy :    0.95
#Sensitivity > 0.92
#Specificity > 0.99

# No improvement as compared to previous rfb model

#______________________________
# 5 cross validation

#2)
# sequence of C and sigma values. 
grid_rfb2 <- expand.grid(sigma=seq(2e-07,3e-07, by=.5e-07), C=seq(1.5,2.5,by=.5))

# Performing 5-fold cross validation
cv_rfb2 <- train(label ~ ., data = train, metric = "Accuracy", method = "svmRadial",
                 tuneGrid = grid_rfb2, preProcess = NULL,
                 trControl = trainControl(method = "cv", number = 5))

print(cv_rfb2)
plot(cv_rfb2)
#sigma = 3e-07 and C = 2

predict_rbf2<- predict(cv_rfb2, newdata = validation)
confusionMatrix(predict_rbf2, validation$label)

#Accuracy :    0.96 
#Sensitivity > 0.93
#Specificity > 0.99

# Accuracy , Specificity and Sensitivity improved as compared to previous rfb kernel
# same as best tuned polynomial kernel


#######################################-------- MODEL TESTING -----#############################################

# cv_rfb2 and best_poly have been the best model so far , both having accuracy of 96 %
#best_poly(polynomial kernel) has gamma = 0.00127551 and C=1 
#cv_rfb2(rfb kernel) has sigma = 3e-07 ie .0000003 and C = 2 
# lets test these on test_original df

#__________________________________________________________
#cv_rfb2(rfb kernel)
predict_test_rbf2<- predict(cv_rfb2, newdata = test_original)
confusionMatrix(predict_test_rbf2, test_original$label)

# Test Accuracy : 0.9639
# Sensitivity   > 0.94
# Specificity   > 0.99
# sigma = .0000003 
# C = 2

#_____________________________________________________________
#best_poly(polynomila kernel)
predict_test_poly<- predict(best_poly, newdata = test_original)
confusionMatrix(predict_test_poly, test_original$label)

# Test Accuracy : 0.9552
# Sensitivity   > 0.92
# Specificity   > 0.99
# gamma = 0.00127551 
# C=1


Final_model<-cv_rfb2



