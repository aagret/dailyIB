
library(XML)
library(methods)
library(httr)
library(data.table)
library(plyr)
#library(TTR)
#library(ggplot2)


#######       ##########################
ibToken   <-  "280265450571377242472700"
#######       ########################## 

source("getIbReport.R")

cash <- getIbReport("334613", ibToken)
cash <- cash[!ClientAccountID == "ClientAccountID", ][, 1:5]
cash <- cash[!CurrencyPrimary == "BASE_SUMMARY",]
cash[, Asset:= "Cash"]
colnames(cash) <- c("Date", "Id", "Alias", "Currency", "Value", "Asset")

Sys.sleep(10)                                
                                                                    
div <- getIbReport("334625", ibToken)
div <- div[!ClientAccountID == "ClientAccountID", ]
div[, Asset:= "Cash"]
div[, Date:= unique(max(cash$Date)), ]
colnames(div) <- c("Id", "Alias", "Currency", "Value", "Asset", "Date")

Sys.sleep(10)                                

int <- getIbReport("334626", ibToken)
int <- int[!ClientAccountID == "ClientAccountID", ]
int <- int[!CurrencyPrimary == "BASE_SUMMARY",]
int[, Asset:= "Cash"]
colnames(int) <- c("Date", "Id", "Alias", "Currency", "Value", "Asset")

Sys.sleep(10)                                

pos <- getIbReport("334611", ibToken)
pos <- pos[!ClientAccountID == "ClientAccountID", ]
colnames(pos) <- c("Date", "Id", "Alias", "Currency", "Asset", "Value", "Fx")

fx <- pos[, unique(Fx), keyby= .(Date, Currency, Id, Alias)]
colnames(fx)[5] <- "Fx"

pos <- pos[,-"Fx"]

db <- setDT(rbind.fill(cash, div, int, pos), 
            key= c("Date", "Currency", "Id", "Alias"))
db <- fx[db]


db[, Date:= as.Date(Date, format="%Y%m%d")]
db[, names(db)[c(5,6)]:= lapply(.SD, 
                              function(x) as.double(unlist(x))),
   .SDcols=names(db)[c(5,6)]]
db[, ValueBase:= Value * Fx]


r <- db[, .(sum(Value), sum(ValueBase)), 
        by= c("Date", "Currency", "Id", "Alias", "Asset")]





p <- db[Alias =="PM" & Date == "2018-11-07", .(Date, Currency, Id, Alias)]




t <- db[AssetClass == "FUT", ':=' (Asset=AssetClass, Currency=CurrencyPrimary, Value= EndingCash - PositionValue,
                Equities= PositionValue)]

t <- t[!AssetClass == "FUT", ':=' (Asset=AssetClass, Currency=CurrencyPrimary, Value= EndingCash,
                                  Equities= PositionValue)]





t <- t[, lapply(.SD, sum, na.rm=TRUE), by= c("Date", "AssetClass", "CurrencyPrimary"),
  .SDcols=c(5:8,10:13)]

colSums(t[, 5:6], na.rm=TRUE)
  
