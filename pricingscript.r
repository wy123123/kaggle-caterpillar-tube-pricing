#caterpiller competition
#To build price prediction model.
library(caret)
library(reshape2)
library(dplyr)
library(data.table)
library(glmnet)
library(MASS)
library(randomForest)
setwd("C:/Users/Documents/Kaggle/kaggle/competition_data")
getwd()
set.seed(123)
tube<- read.csv("tube.csv")
supplier <- read.csv("train_set.csv")

#test set 
testset <- read.cs("test_set.csv")

#bill material, these are the main features to differentiate one tube from another
bill_material <- read.csv("bill_of_materials.csv")
bill_1 <- bill_material[,1:3]
bill_2 <- bill_material[,c(1,4,5)]
bill_3 <- bill_material[,c(1,6,7)]
bill_4 <- bill_material[,c(1,8,9)]
bill_5 <- bill_material[,c(1,10,11)]
bill_6 <- bill_material[,c(1,12,13)]
bill_7 <- bill_material[,c(1,14,15)]
bill_8 <- bill_material[,c(1,16,17)]
setnames(bill_1,c("type","comp","quantity"))
setnames(bill_2,c("type","comp","quantity"))
setnames(bill_3,c("type","comp","quantity"))
setnames(bill_4,c("type","comp","quantity"))
setnames(bill_5,c("type","comp","quantity"))
setnames(bill_6,c("type","comp","quantity"))
setnames(bill_7,c("type","comp","quantity"))
setnames(bill_8,c("type","comp","quantity"))
bills<-rbind(bill_1,bill_2,bill_3,bill_4,bill_5,bill_6,bill_7,bill_8)
#remove NA rows
bills <-(bills[!is.na(bills$comp),])
bills$type <- as.character(bills$type)
#remove duplicates
bills <- bills[!duplicated(bills),]
#cast the data into a dataframe
bill_casted <- data.frame(dcast(data = bills,type~comp,value.var="quantity"))

#join tube_material and supplier_info
#there ar NA values in the material_id culoum, replace them with "others"
tube$material_id <- as.character(tube$material_id)
tube[is.na(tube$material_id),]$material_id = "other"
#form a model matrix of material_id
temp_matrix <- model.matrix(tube_assembly_id~0+material_id,data = tube)
dim(temp_matrix)#check dimensions
#add back the matrix
tube <- cbind(tube,temp_matrix)
#remove material_id
tube <- tube[,-2]

#combine test and training set
testset$cost <-0
testset <- testset[,-1]#remove id column
supplier <- rbind(supplier, testset)

#expand supplier feature
temp <- model.matrix.lm(tube_assembly_id~0+supplier,data = supplier)
supplier <- cbind(supplier,temp)
#remove supplier id in supplier
supplier <- supplier[,-2]
#change bracket column from yes to 1, no to 0
#change factor to character
supplier$bracket_pricing <- as.character(supplier$bracket_pricing)
supplier[supplier$bracket_pricing=="Yes",]$bracket_pricing = 1
supplier[supplier$bracket_pricing=="No",]$bracket_pricing = 0
table(supplier$bracket_pricing)
#remove quote date
supplier <- supplier[,-2]

#combine supplier_infor with tube_material
all <- left_join(supplier,tube,by=c("tube_assembly_id"="tube_assembly_id"))
#combine bill_material features
final <- left_join(all,bill_casted,by=c("tube_assembly_id"="type"))
#assign all NA value into 0
final[is.na(final)]=0

#change the end_form_labels
end_form <- read.csv("tube_end_form.csv")
final$end_a <- as.character(final$end_a)
final$end_x <- as.character(final$end_x)
final.1 <- left_join(final,end_form,by=c("end_a"="end_form_id"))
final.2 <- left_join(final.1,end_form,by=c("end_x"="end_form_id"))
final.2$forming.x <- as.character(final.2$forming.x)
final.2$forming.x[final.2$forming.x=="Yes"]=1
final.2$forming.x[final.2$forming.x=="No"|is.na(final.2$forming.x)]=0
table(final.2$forming.y)
final.2$forming.y <- as.character(final.2$forming.y)
final.2$forming.y[final.2$forming.y=="Yes"]=1
final.2$forming.y[final.2$forming.y=="No"|is.na(final.2$forming.y)]=0

#remove material_id, product_id, end_form_id
finals <- subset(final.2,select=-c(tube_assembly_id,end_a_1x,end_a_2x,end_x_1x,end_x_2x,end_a,end_x))

#check if cost is numeric
class(finals$cost)

#change colunm names; colunm names contain "-" which is hard to recogize as a name as a model.matrix object
xnam <- colnames(finals)
kk <- gsub(" ","",xnam)
kk <- gsub("\\-","_",xnam)
setnames(finals,kk)

#seperate testing and training set
test <- finals[finals$cost==0,]
training <- finals[finals$cost!=0,]

#randomforest regression
##create training set and validation set
intrain <- createDataPartition(y=training$cost,p=0.8,list=F)
train <-training[intrain,] 
validation <- training[-intrain,]
n.data <- subset(validation,select=-c(cost))

#randomforest regression
##create training set and validation set, 80% training set, 20% testing set
intrain <- createDataPartition(y=finals$cost,p=0.8,list=F)
train <-finals[intrain,] 
validation <- finals[-intrain,]
#create a data frame without response variable
n.data <- subset(validation,select=-c(cost))

#train 10 trees for feature selection using randomForest package without CV
fit.RF <- randomForest(cost~.,data=train, ntree = 10,replace = T,do.trace = F)

#training with CV using CARET
caret.fit.rf <- train(cost~.,data=train,method = "rf",trControl=trainControl(method ="cv")

#log error estimation
n <- dim(validation)[1]
prediction <- predict(fit.RF, newdata=n.data)
actual <- validation$cost
RMSLE <- sqrt(1/n*sum((log(prediction+1)-log(actual+1))^2))
RMSLE
