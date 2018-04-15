library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)

train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

#Getting rid of the IDs but keeping the test IDs in a vector.
test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL

test$SalePrice <- NA
all <- rbind(train, test)

#Exploring some of the most important variables
 ##The response variable; SalePrice
 ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
 
 ###Correlations with SalePrice
 numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
 numericVarNames <- names(numericVars)
 cat('There are', length(numericVars), 'numeric variables')
 all_numVar <- all[, numericVars]
 cor_numVar <- cor(all_numVar, use="pairwise.complete.obs")
 #sort on decreasing correlations with SalePrice
 cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
 CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
 cor_numVar <- cor_numVar[CorHigh, CorHigh]
 corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
 
 ###Overall Quality
 ggplot(data=all[!is.na(all$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
   geom_boxplot() + labs(x='Overall Quality') +
   scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
 
 ###Above Grade (Ground) Living Area (square feet)
 ggplot(data=all[!is.na(all$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
   geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
   scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
   geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), '')))
 ### From graph it can be seen that houses no 524 and 1299 are outliers
 
#Missing data, label encoding, and factorizing variables
 ##Completeness of the data
 NAcol <- which(colSums(is.na(all)) > 0)
 sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
 cat('There are', length(NAcol), 'columns with missing values')
 
 ##Imputing missing data {.tabset}
 Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
 
  ### Pool variables
 all$PoolQC[is.na(all$PoolQC)] <- 'None'
 all$PoolQC<-as.integer(revalue(all$PoolQC, Qualities))
 table(all$PoolQC)
 all[all$PoolArea>0 & all$PoolQC==0, c('PoolArea', 'PoolQC', 'OverallQual')]
 ###There are 3 houses whose pool area was greater than 0 and PoolQc as NA orignally
 all$PoolQC[2421] <- 2
 all$PoolQC[2504] <- 3
 all$PoolQC[2600] <- 2
 
 ###Miscellaneous Feature
 
 levels(all$MiscFeature) <- c(levels(all$MiscFeature), "None")
 all$MiscFeature[is.na(all$MiscFeature)] <- "None"
 all$MiscFeature <- as.factor(all$MiscFeature)
 
 ggplot(all[!is.na(all$SalePrice),], aes(x=MiscFeature, y=SalePrice)) +
   geom_bar(stat='summary', fun.y = "median", fill='blue') +
   scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
   geom_label(stat = "count", aes(label = ..count.., y = ..count..))
 
 ###Alley
 levels(all$Alley) <- c(levels(all$Alley),"None")
 all$Alley[is.na(all$Alley)] <- "None"
 all$Alley <- as.factor(all$Alley)
 
 ggplot(all[!is.na(all$SalePrice),], aes(x=Alley, y=SalePrice)) +
   geom_bar(stat='summary', fun.y = "median", fill='blue')+
   scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma)+
   geom_label(stat = "count", aes(label = ..count.., y = ..count..))
 
 
 ###Fence
 levels(all$Fence) <- c(levels(all$Fence),"None")
 all$Fence[is.na(all$Fence)] <- "None" 
 all[!is.na(all$SalePrice),] %>% group_by(Fence) %>% summarise(median = median(SalePrice), counts=n())
 # I conclude that fence variable is not ordinal as none of the types of fence is a best fence as all of it has almost the same sale price
 all$Fence <- as.factor(all$Fence)
 
 ###Fireplace variables
 levels(all$FireplaceQu) <- c(levels(all$FireplaceQu),"None")
 all$FireplaceQu[is.na(all$FireplaceQu)] <- "None" 
 all$FireplaceQu<-as.integer(revalue(all$FireplaceQu, Qualities))
 
 ###Lot variables
 ggplot(all[!is.na(all$LotFrontage),], aes(x=as.factor(Neighborhood), y=LotFrontage)) +
   geom_bar(stat='summary', fun.y = "median", fill='blue') +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 for (i in 1:nrow(all)){
   if(is.na(all$LotFrontage[i])){
     all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood==all$Neighborhood[i]], na.rm=TRUE)) 
   }
 }
 
 all$LotShape<-as.integer(revalue(all$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
 
 ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(LotConfig), y=SalePrice)) +
   geom_bar(stat='summary', fun.y = "median", fill='blue')+
   scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
   geom_label(stat = "count", aes(label = ..count.., y = ..count..))
  #graph shows that LotConfig is not ordinal
 
 all$LotConfig <- as.factor(all$LotConfig)
 
 
 
 ###Garage variables
 
 all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]
 #check if all 157 NAs are the same observations among the variables with 157/159 NAs
 length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))
 #Find the 2 additional NAs
 kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish), c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])
 #Imputing modes.
 all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
 all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
 all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]
 #fixing 3 values for house 2577
 all$GarageCars[2577] <- 0
 all$GarageArea[2577] <- 0
 all$GarageType[2577] <- NA
 
 levels(all$GarageType) <- c(levels(all$GarageType),"None")
 all$GarageType[is.na(all$GarageType)] <- "None" 
 all$GarageType <- as.factor(all$GarageType)
 
 levels(all$GarageFinish) <- c(levels(all$GarageFinish),"None")
 all$GarageFinish[is.na(all$GarageFinish)] <- "None"
 Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
 all$GarageFinish<-as.integer(revalue(all$GarageFinish, Finish))
 
 levels(all$GarageQual) <- c(levels(all$GarageQual),"None")
 all$GarageQual[is.na(all$GarageQual)] <- "None"
 all$GarageQual<-as.integer(revalue(all$GarageQual, Qualities))
 
 levels(all$GarageCond) <- c(levels(all$GarageCond),"None")
 all$GarageCond[is.na(all$GarageCond)] <- "None"
 all$GarageCond<-as.integer(revalue(all$GarageCond, Qualities))
   
 
 ###Basement Variables
 #check if all 79 NAs are the same observations among the variables with 80+ NAs
 length(which(is.na(all$BsmtQual) & is.na(all$BsmtCond) & is.na(all$BsmtExposure) & is.na(all$BsmtFinType1) & is.na(all$BsmtFinType2)))
 
 #Find the additional NAs; BsmtFinType1 is the one with 79 NAs
 all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtCond)|is.na(all$BsmtQual)|is.na(all$BsmtExposure)|is.na(all$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]
 
 #Imputing modes.
 all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
 all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
 all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]
 all$BsmtQual[c(2218, 2219)] <- names(sort(-table(all$BsmtQual)))[1]
 
 levels(all$BsmtQual) <- c(levels(all$BsmtQual),"None")
 all$BsmtQual[is.na(all$BsmtQual)] <- "None"
 all$BsmtQual<-as.integer(revalue(all$BsmtQual, Qualities))
 
 levels(all$BsmtCond) <- c(levels(all$BsmtCond),"None")
 all$BsmtCond[is.na(all$BsmtCond)] <- "None"
 all$BsmtCond<-as.integer(revalue(all$BsmtCond, Qualities))
 
 levels(all$BsmtExposure) <- c(levels(all$BsmtExposure),"None")
 all$BsmtExposure[is.na(all$BsmtExposure)] <- "None"
 Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
 all$BsmtExposure<-as.integer(revalue(all$BsmtExposure, Exposure))
 
 levels(all$BsmtFinType1) <- c(levels(all$BsmtFinType1),"None")
 all$BsmtFinType1[is.na(all$BsmtFinType1)] <- "None"
 FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
 all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1, FinType))
 
 levels(all$BsmtFinType2) <- c(levels(all$BsmtFinType2),"None")
 all$BsmtFinType2[is.na(all$BsmtFinType2)] <- "None"
 FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
 all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2, FinType))
 
 all$BsmtFullBath[is.na(all$BsmtFullBath)] <-0
 all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <-0
 all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <-0
 all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <-0
 all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <-0
 all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <-0
 
 
 ###Masonry variables
 #check if the 23 houses with veneer area NA are also NA in the veneer type
 length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))
 
 #find the one that should have a MasVnrType
 all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]
 all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2] #taking the 2nd value as the 1st is 'none'
 all[2611, c('MasVnrType', 'MasVnrArea')]
 
 all$MasVnrType[is.na(all$MasVnrType)] <- 'None'
 
 all[!is.na(all$SalePrice),] %>% group_by(MasVnrType) %>% summarise(median = median(SalePrice), counts=n()) %>% arrange(median)
 Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
 all$MasVnrType<-as.integer(revalue(all$MasVnrType, Masonry))
 
 all$MasVnrArea[is.na(all$MasVnrArea)] <-0

 
 ###MS Zoning
 #imputing the mode
 all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]
 all$MSZoning <- as.factor(all$MSZoning)
 table(all$MSZoning)
 
 ###Kitchen variables
 all$KitchenQual[is.na(all$KitchenQual)] <- 'TA' #replace with most common value
 all$KitchenQual<-as.integer(revalue(all$KitchenQual, Qualities))
 
 ###Utilities
 table(all$Utilities)
 kable(all[is.na(all$Utilities) | all$Utilities=='NoSeWa', 1:9])
 all$Utilities <- NULL
 
 ###Home functionality
 #impute mode for the 1 NA
 all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]
 all$Functional <- as.integer(revalue(all$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
 
 
 ###Exterior variables
 #imputing mode
 all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]
 all$Exterior1st <- as.factor(all$Exterior1st)
 
 #imputing mode
 all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]
 all$Exterior2nd <- as.factor(all$Exterior2nd)
 
 all$ExterQual<-as.integer(revalue(all$ExterQual, Qualities))

 all$ExterCond<-as.integer(revalue(all$ExterCond, Qualities))

 
 ###Electrical system
 #imputing mode
 all$Electrical[is.na(all$Electrical)] <- names(sort(-table(all$Electrical)))[1]
 all$Electrical <- as.factor(all$Electrical)
 
 #imputing mode
 all$SaleType[is.na(all$SaleType)] <- names(sort(-table(all$SaleType)))[1]
 all$SaleType <- as.factor(all$SaleType)
 
 all$SaleCondition <- as.factor(all$SaleCondition)

 
 ##Label encoding/factorizing the remaining character variables
 
 Charcol <- names(all[,sapply(all, is.character)])
 Charcol
 cat('There are', length(Charcol), 'remaining columns with character values')
 
 ###Foundation
 all$Foundation <- as.factor(all$Foundation)
 
 ###Heating and airco
 all$Heating <- as.factor(all$Heating)
 all$HeatingQC<-as.integer(revalue(all$HeatingQC, Qualities))
 
 all$CentralAir<-as.integer(revalue(all$CentralAir, c('N'=0, 'Y'=1)))
 
 ###Roof
 all$RoofStyle <- as.factor(all$RoofStyle)
 all$RoofMatl <- as.factor(all$RoofMatl)

 ###Land
 all$LandContour <- as.factor(all$LandContour)
 all$LandSlope<-as.integer(revalue(all$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
 
 ###Dwelling
 all$BldgType <- as.factor(all$BldgType)
 all$HouseStyle <- as.factor(all$HouseStyle) 

 ###Neighborhood and Conditions
 all$Neighborhood <- as.factor(all$Neighborhood)
 all$Condition1 <- as.factor(all$Condition1)
 all$Condition2 <- as.factor(all$Condition2)
 
 ###Pavement of Street & Driveway
 all$Street<-as.integer(revalue(all$Street, c('Grvl'=0, 'Pave'=1)))
 all$PavedDrive<-as.integer(revalue(all$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
 
 
 ##Changing some numeric variables into factors
 
 ###Year and Month Sold
 str(all$YrSold)
 str(all$MoSold)
 all$MoSold <- as.factor(all$MoSold)
 
 ys <- ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(YrSold), y=SalePrice)) +
   geom_bar(stat='summary', fun.y = "median", fill='blue')+
   scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
   geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
   coord_cartesian(ylim = c(0, 200000)) +
   geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

 ms <- ggplot(all[!is.na(all$SalePrice),], aes(x=MoSold, y=SalePrice)) +
   geom_bar(stat='summary', fun.y = "median", fill='blue')+
   scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
   geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
   coord_cartesian(ylim = c(0, 200000)) +
   geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
 
 grid.arrange(ys, ms, widths=c(1,2))

 
 ###MSSubClass
 all$MSSubClass <- as.factor(all$MSSubClass)
 #revalue for better readability
 all$MSSubClass<-revalue(all$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))
 
 
 #Visualization of important variables
 numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
 factorVars <- which(sapply(all, is.factor)) #index vector factor variables
 all_numVar <- all[, numericVars]
 cor_numVar <- cor(all_numVar, use="pairwise.complete.obs")
 #sort on decreasing correlations with SalePrice
 cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
 #select only high corelations
 CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
 cor_numVar <- cor_numVar[CorHigh, CorHigh]
 
 corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
 
 ##Finding variable importance with a quick Random Forest
 set.seed(2018)
 quick_RF <- randomForest(x=all[1:1460,-79], y=all$SalePrice[1:1460], ntree=100,importance=TRUE)
 imp_RF <- importance(quick_RF)
 imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
 imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]
 
 ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) +
   geom_bar(stat = 'identity') + 
   labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + 
   coord_flip() +
   theme(legend.position="none")
 
 s1 <- ggplot(data= all, aes(x=GrLivArea)) +
   geom_density() + labs(x='Square feet living area')
 s2 <- ggplot(data=all, aes(x=as.factor(TotRmsAbvGrd))) +
   geom_histogram(stat='count') + labs(x='Rooms above Ground')
 s3 <- ggplot(data= all, aes(x=X1stFlrSF)) +
   geom_density() + labs(x='Square feet first floor')
 s4 <- ggplot(data= all, aes(x=X2ndFlrSF)) +
   geom_density() + labs(x='Square feet second floor')
 s5 <- ggplot(data= all, aes(x=TotalBsmtSF)) +
   geom_density() + labs(x='Square feet basement')
 s6 <- ggplot(data= all[all$LotArea<100000,], aes(x=LotArea)) +
   geom_density() + labs(x='Square feet lot')
 s7 <- ggplot(data= all, aes(x=LotFrontage)) +
   geom_density() + labs(x='Linear feet lot frontage')
 s8 <- ggplot(data= all, aes(x=LowQualFinSF)) +
   geom_histogram() + labs(x='Low quality square feet 1st & 2nd')
 
 layout <- matrix(c(1,2,5,3,4,8,6,7),4,2,byrow=TRUE)
 multiplot(s1, s2, s3, s4, s5, s6, s7, s8, layout=layout)
 
 n1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=Neighborhood, y=SalePrice)) +
   geom_bar(stat='summary', fun.y = "median", fill='blue') +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
   geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
   geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
 n2 <- ggplot(data=all, aes(x=Neighborhood)) +
   geom_histogram(stat='count')+
   geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 grid.arrange(n1, n2)
 
 q1 <- ggplot(data=all, aes(x=as.factor(OverallQual))) +
   geom_histogram(stat='count')
 q2 <- ggplot(data=all, aes(x=as.factor(ExterQual))) +
   geom_histogram(stat='count')
 q3 <- ggplot(data=all, aes(x=as.factor(BsmtQual))) +
   geom_histogram(stat='count')
 q4 <- ggplot(data=all, aes(x=as.factor(KitchenQual))) +
   geom_histogram(stat='count')
 q5 <- ggplot(data=all, aes(x=as.factor(GarageQual))) +
   geom_histogram(stat='count')
 q6 <- ggplot(data=all, aes(x=as.factor(FireplaceQu))) +
   geom_histogram(stat='count')
 q7 <- ggplot(data=all, aes(x=as.factor(PoolQC))) +
   geom_histogram(stat='count')
 
 layout <- matrix(c(1,2,8,3,4,8,5,6,7),3,3,byrow=TRUE)
 multiplot(q1, q2, q3, q4, q5, q6, q7, layout=layout)
 
 ms1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=MSSubClass, y=SalePrice)) +
   geom_bar(stat='summary', fun.y = "median", fill='blue') +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
   geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
   geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
 ms2 <- ggplot(data=all, aes(x=MSSubClass)) +
   geom_histogram(stat='count')+
   geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 grid.arrange(ms1, ms2)
 
 #correct error
 all$GarageYrBlt[2593] <- 2007 #this must have been a typo. GarageYrBlt=2207, YearBuilt=2006, YearRemodAdd=2007.
 g1 <- ggplot(data=all[all$GarageCars !=0,], aes(x=GarageYrBlt)) +
   geom_histogram()
 g2 <- ggplot(data=all, aes(x=as.factor(GarageCars))) +
   geom_histogram(stat='count')
 g3 <- ggplot(data= all, aes(x=GarageArea)) +
   geom_density()
 g4 <- ggplot(data=all, aes(x=as.factor(GarageCond))) +
   geom_histogram(stat='count')
 g5 <- ggplot(data=all, aes(x=GarageType)) +
   geom_histogram(stat='count')
 g6 <- ggplot(data=all, aes(x=as.factor(GarageQual))) +
   geom_histogram(stat='count')
 g7 <- ggplot(data=all, aes(x=as.factor(GarageFinish))) +
   geom_histogram(stat='count')
 
 layout <- matrix(c(1,5,5,2,3,8,6,4,7),3,3,byrow=TRUE)
 multiplot(g1, g2, g3, g4, g5, g6, g7, layout=layout)
 
 
 b1 <- ggplot(data=all, aes(x=BsmtFinSF1)) +
   geom_histogram() + labs(x='Type 1 finished square feet')
 b2 <- ggplot(data=all, aes(x=BsmtFinSF2)) +
   geom_histogram()+ labs(x='Type 2 finished square feet')
 b3 <- ggplot(data=all, aes(x=BsmtUnfSF)) +
   geom_histogram()+ labs(x='Unfinished square feet')
 b4 <- ggplot(data=all, aes(x=as.factor(BsmtFinType1))) +
   geom_histogram(stat='count')+ labs(x='Rating of Type 1 finished area')
 b5 <- ggplot(data=all, aes(x=as.factor(BsmtFinType2))) +
   geom_histogram(stat='count')+ labs(x='Rating of Type 2 finished area')
 b6 <- ggplot(data=all, aes(x=as.factor(BsmtQual))) +
   geom_histogram(stat='count')+ labs(x='Height of the basement')
 b7 <- ggplot(data=all, aes(x=as.factor(BsmtCond))) +
   geom_histogram(stat='count')+ labs(x='Rating of general condition')
 b8 <- ggplot(data=all, aes(x=as.factor(BsmtExposure))) +
   geom_histogram(stat='count')+ labs(x='Walkout or garden level walls')
 
 layout <- matrix(c(1,2,3,4,5,9,6,7,8),3,3,byrow=TRUE)
 multiplot(b1, b2, b3, b4, b5, b6, b7, b8, layout=layout)
 
 
 #Feature Engineering
 
 all$TotBathrooms <- all$FullBath + (all$HalfBath*0.5) + all$BsmtFullBath + (all$BsmtHalfBath*0.5)
 tb1 <- ggplot(data=all[!is.na(all$SalePrice),], aes(x=as.factor(TotBathrooms), y=SalePrice))+
   geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
   scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
 tb2 <- ggplot(data=all, aes(x=as.factor(TotBathrooms))) +
   geom_histogram(stat='count')
 grid.arrange(tb1, tb2) 
 
 
 all$TotalPorchSF <- all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch
 por1 <- ggplot(data=all, aes(x=TotalPorchSF)) +
   geom_histogram() + labs(x=' Total Porch square feet')
 por2 <- ggplot(data=all, aes(x=WoodDeckSF)) +
   geom_histogram()+ labs(x='Wood Deck square feet')
 grid.arrange(por1, por2, nrow=1)
 
 
 all$Remod <- ifelse(all$YearBuilt==all$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
 all$Age <- as.numeric(all$YrSold)-all$YearRemodAdd
 ggplot(data=all[!is.na(all$SalePrice),], aes(x=Age, y=SalePrice))+
   geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
   scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
 
 ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(Remod), y=SalePrice)) +
   geom_bar(stat='summary', fun.y = "median", fill='blue') +
   geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
   scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
   theme_grey(base_size = 18) +
   geom_hline(yintercept=163000, linetype="dashed") #dashed line is median SalePrice
 
 all$IsNew <- ifelse(all$YrSold==all$YearBuilt, 1, 0)
 all$YrSold <- as.factor(all$YrSold) 

 nb1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=median), y=SalePrice)) +
   geom_bar(stat='summary', fun.y = "median", fill='blue') + labs(x='Neighborhood', y='Median SalePrice') +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
   geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
   geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
 nb2 <- ggplot(all[!is.na(all$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=mean), y=SalePrice)) +
   geom_bar(stat='summary', fun.y = "mean", fill='blue') + labs(x='Neighborhood', y="Mean SalePrice") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
   geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
   geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
 grid.arrange(nb1, nb2) 
 
 all$NeighRich[all$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
 all$NeighRich[!all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
 all$NeighRich[all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0
 
 all$TotalSqFeet <- all$GrLivArea + all$TotalBsmtSF

 #Preparing data for modeling
 
 ##Dropping highly correlated variables
 
 dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')
 all <- all[,!(names(all) %in% dropVars)] 

 ##Removing outliers
 all <- all[-c(524, 1299),] 

 ##PreProcessing predictor variables
 numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] #numericVarNames was created before having done anything
 numericVarNames <- append(numericVarNames, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))
 
 DFnumeric <- all[, names(all) %in% numericVarNames]
 
 DFfactors <- all[, !(names(all) %in% numericVarNames)]
 DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']
 
 cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')
 ##skewness
 for(i in 1:ncol(DFnumeric)){
   if (abs(skew(DFnumeric[,i]))>0.8){
     DFnumeric[,i] <- log(DFnumeric[,i] +1)
   }
 }
 
 **Normalizing the data**
   PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
 print(PreNum)
 DFnorm <- predict(PreNum, DFnumeric)
 dim(DFnorm)
 
 ###One hot encoding the categorical variables
 DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
 dim(DFdummies)
 
 ###Removing levels with few or no observations in train or test
 
 #check if some values are absent in the test set
 ZerocolTest <- which(colSums(DFdummies[1459:2917,])==0)
 colnames(DFdummies[ZerocolTest])
 DFdummies <- DFdummies[,-ZerocolTest] #removing predictors
 
 #check if some values are absent in the train set
 ZerocolTrain <- which(colSums(DFdummies[1:1458,])==0)
 colnames(DFdummies[ZerocolTrain])
 DFdummies <- DFdummies[,-ZerocolTrain] #removing predictor
 
 #Also taking out variables with less than 10 'ones' in the train set.
 fewOnes <- which(colSums(DFdummies[1:1458,])<10)
 colnames(DFdummies[fewOnes])
 DFdummies <- DFdummies[,-fewOnes] #removing predictors
 dim(DFdummies)
 
 
 combined <- cbind(DFnorm, DFdummies) #combining all (now numeric) predictors into one dataframe 

 ##Dealing with skewness of response variable
 skew(all$SalePrice)
 qqnorm(all$SalePrice)
 qqline(all$SalePrice) 
 
 all$SalePrice <- log(all$SalePrice) #default is the natural logarithm, "+1" is not necessary as there are no 0's
 skew(all$SalePrice)
 qqnorm(all$SalePrice)
 qqline(all$SalePrice)
 
 ##Composing train and test sets
 train1 <- combined[1:1458,] #2 outliers taken out of training data
 test1 <- combined[1459:2917,]
 
 #Modeling
 
 ##Lasso regression model
 
 set.seed(1234)
 my_control <-trainControl(method="cv", number=5)
 lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
 
 lasso_mod <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
 lasso_mod$bestTune
 min(lasso_mod$results$RMSE)
 
 LassoPred <- predict(lasso_mod, test1)
 predictions_lasso <- exp(LassoPred) #need to reverse the log to the real values
 head(predictions_lasso)
 
 #write submission files
 sub_lasso <- data.frame(Id = test_labels, SalePrice = predictions_lasso)
 write.csv(sub_lasso, file = 'Lasso_model.csv', row.names = F)
 
 ##XGBoost model
 label_train <- all$SalePrice[!is.na(all$SalePrice)]
 
 # put our testing & training data into two seperates Dmatrixs objects
 dtrain <- xgb.DMatrix(data = as.matrix(train1), label= label_train)
 dtest <- xgb.DMatrix(data = as.matrix(test1))

 
 default_param<-list(
   objective = "reg:linear",
   booster = "gbtree",
   eta=0.1, #default = 0.3
   gamma=0,
   max_depth=6,
   min_child_weight=1,
   subsample=1,
   colsample_bytree=1,
   seed= 2018
 )
 
 xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)
 #train the model using the best iteration found by cross validation
 xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 115)
 XGBpred <- predict(xgb_mod, dtest)
 predictions_XGB <- exp(XGBpred) #need to reverse the log to the real values
 head(predictions_XGB)
 
 #write submission files
 sub_xgb <- data.frame(Id = test_labels, SalePrice = predictions_XGB)
 write.csv(sub_xgb, file = 'XGB_model.csv', row.names = F)
 
 #Linear Regression model
 train2 <- cbind(train1 , SalePrice=all$SalePrice[!is.na(all$SalePrice)])
 regressor = lm(SalePrice ~ . , data = train2)
 predictions_reg = predict.lm(regressor, newdata = test1)
 #write submission files
 sub_reg <- data.frame(Id = test_labels, SalePrice = predictions_reg)
 write.csv(sub_xgb, file = 'regression_model.csv', row.names = F)
 
 
 
 ##Averaging predictions
 sub_avg <- data.frame(Id = test_labels, SalePrice = (predictions_XGB+predictions_lasso)/2)
 write.csv(sub_avg, file = 'average.csv', row.names = F)
 