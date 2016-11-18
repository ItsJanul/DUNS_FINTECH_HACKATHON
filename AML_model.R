#Build 1.0 of DUNS ML algorithm for ANTI-MONEY LAUNDERING
#JH changes on 11/13/16



library(plyr)
library(data.table)
library(rpart)
library(rpart.plot)
library("ROCR")
library("ggplot2")

##CLEANING LANGUAGE FUNCTIONS----------------------------------------------------------------
clean_type <- function(x)
  ifelse (x == "PRIJEM" , "CREDIT",
          ifelse(x == "VYDAJ", "WITHDRAWAL", "NA" ))

clean_operation <- function(x)
  ifelse (x == "VYBER KARTOU" , "CC_WITHDRAWAL",
          ifelse(x == "VKLAD", "CASH_CREDIT", 
                 ifelse(x =="PREVOD Z UCTU", "COLLECT_BANK",
                        ifelse(x== "VYBER", "WITHDRAWAL_CASH", 
                               ifelse(x== "PREVOD NA UCET", "REMMITANCE_TO_BANK", "NA" )))))

clean_k_symbol <- function(x)
  ifelse (x == "POJISTNE" , "INSURACE_PAYMENT",
          ifelse(x == "SLUZBY", "PAYMENT_STATEMENT", 
                 ifelse(x =="UROK", "INTEREST_CREDITED",
                        ifelse(x== "SANKC. UROK", "NEG_BALANCE", 
                               ifelse(x== "SIPO", "HOUSEHOLD", 
                                      ifelse(x== "DUCHOD", "PENSION",
                                             ifelse(x=="UVER", "LOAN_PAYMENT", "NA" )))))))

clean_frequency <- function(x)
  ifelse (x == "POPLATEK MESICNE" , "MONTHLY",
          ifelse(x == "POPLATEK TYDNE", "WEEKLY", 
                 ifelse(x =="POPLATEK PO OBRATU", "TRANSACTION", "NA")))

##FUNCTION FOR COUNTING DAYS SINCE LAST TRANSACTION

days = function (x)
  #this days function works but very slowly, have not found a better vectorized alternative-JH 11-16-16
{
  y= sapply(1:length(x),function(i){ifelse(i==1 || x[i-1]>x[i] , 0, x[i]-x[i-1])})
  return(y)
}

shift <- function(x, lag) 
  #this function shifts any vector ahead by lag
{
  n <- length(x)
  xnew <- rep(NA, n)
  xnew <- as.Date(xnew)
  xnew[(lag+1):n] <- x[1:(n-lag)]
  return(xnew)
}

##FUNCTIONS FOR SCENARIO FLAGGING OF AML-----------------------------------------------------------

naive_SARS<- function (id, amount, last_trans)
  #SARS based on transaction
{
  y= sapply(1:length(id), function(i) {ifelse(amount[i] >=25000, 1, 
                                              ifelse(amount[i] >= 5000, 1,
                                                     ifelse(last_trans[i] <=15 && amount[i]+amount[i-1] >= 5000, 1, 0)))})
  return(y)
}

SARS_type_trans<-function (flag, operation)
  #SARS based on type of transaction
{
  y= sapply(1:length(flag), function(i) {ifelse( flag[i] == 0, 0, 
                                                 ifelse(is.na(operation [i]) || operation[i] == "WITHDRAWAL_CASH", 1,0))})
  return(y)
}

SARS_socio_economic<- function (flag, id, crimes, unemp)#SARS based on socio_economic
{
  y= sapply(1:length(id), function(i) {ifelse( flag[i] == 0, 0, 
                                               ifelse(crimes [i] >= 6132, 1,
                                                      ifelse(unemp[i] >=4.720, 1, 0)))})
  return(y)
}

#Trying to add threat classifiers------------------------------------
#merged_table$threat<- ifelse(merged_table$threat==0, "LOW RISK", 
#                             ifelse(merged_table$threat==1, "MEDIUM RISK", 
#                                    ifelse(merged_table$threat==2, "HIGH RISK", "HIGH-HIGH RISK")))

##CLEANING DATA-------------------------------------------------------------------------------------

#Creating a merged table of relevant variables
setnames(district, old = c("A1", "A11", "A12", "A13", "A14", "A15", "A16"), new = c("district_id", "AVG_SALARY", "UNEMP_95", "UNEMP_96", "ENTR", "CRIMES_95", "CRIMES_96"))
setnames(trans, old= "date", new= "trans_date")
merged_trans_account1= merge(x= trans,y= account[, c("account_id","district_id","frequency")], by = "account_id", all.x = TRUE)
merged_table= merge(merged_trans_account1, district[ , c("district_id", "AVG_SALARY", "UNEMP_95", "UNEMP_96", "ENTR", "CRIMES_95", "CRIMES_96")], by = "district_id", all =TRUE)
dt_all_transactions = merge(dt_all_transactions, customer_names, by = "account_id", all = TRUE)

#Translating our variables to english
merged_table$type <- clean_type(merged_table$type)
merged_table$operation <- clean_operation(merged_table$operation)
merged_table$k_symbol <- clean_k_symbol(merged_table$k_symbol)
merged_table$frequency <- clean_frequency(merged_table$frequency)

#Cleaning Transaction Dates
merged_table$trans_date= paste("19", merged_table$trans_date, sep="")
merged_table$trans_date= as.Date(as.character(merged_table$trans_date), format='%Y%m%d')

#Setting transactions in order to create transaction dates
merged_table <- with(merged_table,merged_table[order(account_id,trans_date),])

#Adding vector for date of last trans and cumulative time since last trans
merged_table$last_trans_date<- shift(merged_table$trans_date, 1)
merged_table$time_between_trans<- merged_table$trans_date-merged_table$last_trans_date
merged_table$time_between_trans<- ifelse(merged_table$time_between_trans < 0, 0, merged_table$time_between_trans)

#layer1 SARS
merged_table$SARS_layer1 <- naive_SARS(merged_table$account_id,merged_table$amount, merged_table$time_between_tran)

#Classifier level 1
merged_table$classifier_level1 <- SARS_type_trans(merged_table$SARS_layer1, merged_table$operation)

#Classifier level 2
merged_table$classifier_level2<- SARS_socio_economic(merged_table$classifier_level1, merged_table$account_id,
                                               merged_table$CRIMES_96, merged_table$UNEMP_96)

#splitting subset by date--------------------------------------------

training_data<-subset(merged_table, trans_date <= as.Date("1998-01-01"))
test_data<-subset(merged_table, trans_date > as.Date("1998-01-01"))
flagged<-subset(test_data, classifier_level2==1)

#Naive model---------------------------------------------------------
naive_model <- rpart(SARS_layer1~ +amount +time_between_trans, data = training_data,  
                  control=list(minsplit=5))
naive_model.plot<- rpart.plot(naive_model)

#Recursive partioning------------------------------------------------
ML_model <- rpart(classifier_level2~ +amount +AVG_SALARY +UNEMP_96 +ENTR +CRIMES_96 +time_between_trans, data = training_data,  
                  control=list(minsplit=5))
ML_model.plot<- rpart.plot(ML_model)

#Trying to create predictions not sure how to work this quite yet JH 11-16-16
pred<- prediction(predict(ML_model,test_data), test_data$classifier_level2)
plot(performance(pred, "tpr", "fpr"))
abline(0, 1, lty = 2)

pred2<- prediction(predict(naive_model,test_data), test_data$classifier_level2)
plot(performance(pred2, "tpr", "fpr"))
abline(0, 1, lty = 2)


#VISUALIZING DATA-------------------------------------------------------

#histogram of transaction amount vs count
plot_amount_hist = ggplot(training_data, aes(x=amount)) + geom_histogram()
sankeytree(ML_model, name="ML Model", childrenName = c("test1", "test2", "test3"), maxLabelLength = 10, nodeHeight = 100)

