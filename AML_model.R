#Build 1.0 of DUNS ML algorithm for ANTI-MONEY LAUNDERING
#JH changes on 11/13/16



library(plyr)
library(data.table)

##CLEANING LANGUAGE FUNCTIONS
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


##Scenario for SARS according to FFIEC for flagging suspicious activity
naive_SARS<- function (id, amount, last_trans)
{
  y= sapply(1:length(id), function(i) {ifelse(amount[i] >= 5000, 1,
                                              ifelse(amount[i] >=25000, 1, 
                                                     ifelse(last_trans[i] <=15 && amount[i]+amount[i-1] >5000, 1, 0)))})
  return(y)
}

SARS_type_trans<-function (flag, operation)
{
  y= sapply(1:length(flag), function(i) {ifelse( flag[i] == 0, 0, 
                                               ifelse(operation [i] == "NA" || operation[i] == "WITHDRAWAL_CASH", 1,0))})
  return(y)
}

SARS_socio_economic<- function (flag, id, crimes, unemp)
{
  y= sapply(1:length(id), function(i) {ifelse( flag[i] == 0, 0, 
                                               ifelse(crimes [i] >= 6132, 1,
                                                      ifelse(unemp[i] >=4.720, 1, 0)))})
  return(y)
}


days = function (x)
{
  y= sapply(1:length(x),function(i){ifelse(i==1 || x[i-1]>x[i] , 0, x[i]-x[i-1])})
  return(y)
}


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

#Setting transactions in order to create transaction #
merged_table <- with(merged_table,merged_table[order(account_id,trans_date),])
merged_table$last_trans <- days(merged_table$trans_date)

#In between cleaning data I merged df to a dt called "dt_all_transactions" 

#layer1 SARS
dt_all_transactions$SARS_layer1 <- naive_SARS(dt_all_transactions$account_id,dt_all_transactions$amount, dt_all_transactions$last_trans)

#layer2 SARS
dt_all_transactions$SARS_layer2 <- SARS_type_trans(dt_all_transactions$SARS_layer1, dt_all_transactions$operation)

#layer3 SARS
dt_all_transactions$SARS_layer3<- SARS_socio_economic(dt_all_transactions$SARS_layer2, dt_all_transactions$account_id,
                                                      dt_all_transactions$CRIMES_96, dt_all_transactions$UNEMP_96)

#VISUALIZING DATA

#histogram of transaction amount vs count
plot_amount_hist = ggplot(dt_all_transactions, aes(x=amount)) + geom_histogram()

#Recursive partioning

library(rpart)
library(rpart.plot)
fit <- rpart(SARS_layer3 ~ + amount +AVG_SALARY +UNEMP_96 +ENTR +CRIMES_96 +last_trans, data = dt_all_transactions, control=list(minsplit=5))
rpart.plot(fit)
text(fit, use.n = TRUE)

fit2 <- rpart(predicted_anomaly ~ + amount +AVG_SALARY +UNEMP_96 +ENTR +CRIMES_96 +last_trans, data = output, control=list(minsplit=5))
rpart.plot(fit2)
text(fit2, use.n = TRUE)

pred<- prediction(predict(fit, type= "prob")[,2],
                  dt_all_transactions$SARS_layer3)
