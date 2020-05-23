library(quantmod)
library(PerformanceAnalytics)
library(data.table)
library(tidyverse)
library(tseries)
library(randomForest)
library(TTR)
library(pdfetch)
library(tidyr)
###simple alpha models ####

# data retrieving
#inputs
#######st.cr.et###
stocks <-c("AAPL","ABBV","ABT","ACN","ADBE","AGN","AIG","ALL","AMGN","AMZN","AXP","BA","BAC","BIIB","BK","BKNG","BLK","BMY","C","CAT","CELG","CHTR","CL","CMCSA","COF","COP","COST","CSCO","CVS","CVX","DD","DHR","DIS","DOW","DUK","EMR","EXC","F","FB","FDX","GD","GE","GILD","GM","GOOG","GOOGL","GS","HD","HON","IBM","INTC","JNJ","JPM","KHC","KMI","KO","LLY","LMT","LOW","MA","MCD","MDLZ","MDT","MET","MMM","MO","MRK","MS","MSFT","NEE","NFLX","NKE","NVDA","ORCL","OXY","PEP","PFE","PG","PYPL","QCOM","RTN","SBUX","SLB","SO","SPG","T","TGT","TXN","UNH","UNP","UPS","USB","UTX","V","VZ","WBA","WFC","WMT","XOM")
####importing crypto.currencies#######
crypto <- c("BTC-USD","XRP-USD","ETH-USD","USDT-USD","LTC-USD","BNB-USD","LINK-USD","EOS-USD","XLM-USD","ADA-USD","TRX-USD")
#"IOT-USD",
########importing etf##########
etfs <- c("SPYX","SPXV","YLCO",'EWW',"QLD","SPXN","HFXJ","ILF","RING","KBWP","IPKW","FBZ","XSD","CATH","PSI","FFTY","PSJ","KIE","EZA")

stocks_baselinee <-c("^GSPC")
period_data<-c(90)
period_prediction<-28
period_count<-6
period_start<-"2018-01-01"
period_end<-"2019-10-01"

#price data + matrix forming
st.cr.et <- c(stocks,crypto,etfs)
getSymbols(c(st.cr.et,stocks_baselinee), from = period_start, to = period_end)
stocks_baselinee <-c("GSPC")
matrix<-data.table(get(stocks_baselinee),keep.rownames = TRUE)

colnames(matrix) <- c("index","Open","Hight","Low","Close","Volume","price_adj")
matrix$ticker<-as.character(stocks_baselinee)


for (i in 1:length(st.cr.et)){
  temp<-data.table(get(st.cr.et[i]),keep.rownames = TRUE)
  colnames(temp) <- c("index","Open","Hight","Low","Close","Volume","price_adj")
  temp$ticker<-st.cr.et[i]
  matrix<-rbind(matrix,temp,fill=TRUE)
}

# adding t+1
temp<-matrix[,c("index","ticker","price_adj")]
colnames(temp)[colnames(temp)=="price_adj"]<-"price_adj_next"
temp$index<-(temp$index-period_prediction)
matrix<-merge(matrix,temp, by = c("index","ticker"), all.x = TRUE)

##CalculateReturns
temp<-matrix[,c("index","ticker","price_adj")]
colnames(temp)[colnames(temp)=="price_adj"]<-"price_adj_prior"
temp$index<-temp$index+period_prediction
matrix<-merge(matrix,temp, by = c("index","ticker"), all.x = TRUE)
matrix<-matrix[order(ticker,index),]
matrix<-matrix%>%fill(price_adj_next,price_adj_prior)
### add fill


### add TTR metrics
st.cr.et <- c(st.cr.et,stocks_baselinee)
tech.indic <- function(x) {
  GET <- data.table(get(st.cr.et[x]),keep.rownames = TRUE)[,c(1,7)]
  GET.index <-  (data.table(get(st.cr.et[x]),keep.rownames = TRUE)[,c(1,7)])[,1]
  GET$ticker <- as.character(st.cr.et[x])
  ma13.GET <- SMA(GET,n=13)
  ma3.GET <- SMA(GET,n=3)
  ema.GET <- EMA(GET)
  sma.GET <- SMA(GET)
  rsi.GET <- RSI(GET)
  macd.GET <- MACD(GET)
  roc.GET <- ROC(GET)
  d.f <- cbind.data.frame(GET.index,GET$ticker,ma13.GET,ma3.GET,ema.GET,sma.GET,rsi.GET,macd.GET,roc.GET)
  colnames(d.f) <- c("index","ticker","MA13","MA3","ema","sma","rsi","macd","signal","roc")
  return(d.f)}
empty <- data.table()

for ( i in 1:length(st.cr.et)){
 ta <- tech.indic(i)
 ta <- as.data.frame(ta)
 empty <- rbind(ta,empty)
}
st.cr.et <- st.cr.et[st.cr.et!="GSPC"]
st.cr.et

#merging T.INDC with matrix 
matrix <- merge(as.data.frame(matrix),as.data.frame(empty), by =c("ticker","index"),all.x = TRUE)
matrix <- merge(matrix,data.table(get(stocks_baselinee),keep.rownames = TRUE)[,c(1,7)],by =c("index"))

#w <- write.csv(matrix, file = "matric.csv")

#macrofactors

factors <- c("T10Y2Y","DFF","USD3MTD156N","WILL5000INDFC","USEPUINDXD","DJIA","DEXCHUS","DCOILBRENTEU","GOLDPMGBD228NLBM","CBBTCUSD","DPCREDIT","NASDAQ100",'CBETHUSD')
data <- as.data.table(pdfetch_FRED(factors))
colnames(data) <- c("index","tenyTr_twoyrTR","ef.ffr" ,"threemonthLibor","willshire5000" ,"ecopolunct","DOW","CHINAUS","crudeoil","goldprice","coinbase.bitcoin","primar.credit.rate","Nasdqa","coinbaseethe")
data <- subset(data, data$index > period_start & data$index <= period_end)

date <- as.POSIXlt(data$index,format="%Y-%m-%d")
data <- subset(data,!(weekdays(as.Date(date)) %in% c('Saturday','Sunday')))
data <- data[-1,]

print(sum(is.na(data)))

matrix <- merge.data.frame(matrix,data,by=c("index"), all.x = TRUE)
is.na(matrix)<-sapply(matrix, is.infinite)
### roc+1
matrix$roct1=(matrix$price_adj_next-matrix$price_adj)/matrix$price_adj
matrix$roct1_<-matrix$roct1
matrix$price_adj_next_<-matrix$price_adj_next
#wr <- write.csv(matrix, file = "matrix.csv")



### predicting models
period_end<-as.Date(period_end)+period_prediction
for(iii in (1:period_count)) {
  

  period_end<-as.Date(period_end)-period_prediction
  
temp<-matrix[matrix$index>=as.Date(period_start)&matrix$index<=as.Date(period_end),]
temp$price_adj_next[temp$index>as.Date(period_end)-period_prediction]<-NA
temp$roct1[temp$index>as.Date(period_end)-period_prediction]<-NA

###Alfa
summary_alfa<-data.table(ticker=character(),alfa=numeric())
for (i in 1:length(st.cr.et)){
  temp1<-temp[,c("index","ticker","roc")]
  temp1<-temp1[temp$ticker==st.cr.et[i]|temp$ticker==stocks_baselinee,]
  temp1<-spread(temp1,ticker,roc)
    temp1<-na.omit(temp1)
  y<-temp1[,st.cr.et[i]]
  x<-temp1[,stocks_baselinee]
  model_alfa<-lm(y~x)
  summary_alfa<-rbind(summary_alfa,data.table(ticker=st.cr.et[i],alfa=coef(model_alfa)["(Intercept)"]))
}


###kpss
#kpss.test(matrix$roct1[matrix$ticker==st.cr.et[3]]) 
#kpss.test(diff(matrix$price_adj[matrix$ticker==st.cr.et[1]]))
###arima 

###lm 

summary_lm<-data.frame(ticker=character(),model=character(),value=character())

for  (j in 1:length(period_data)){
  for  (i in 1:length(st.cr.et)){
    temp1<-temp[temp$ticker==st.cr.et[i]&temp$index>max(temp$index)-period_data[j],]
    #set factors
    outcome<-c("roct1")
    variables<-c("index","GSPC.Adjusted","tenyTr_twoyrTR","threemonthLibor","willshire5000","ecopolunct","DOW","CHINAUS","crudeoil","goldprice","coinbase.bitcoin","Nasdqa","coinbaseethe")
    temp1[,variables[3]]
   
    f <- as.formula(
      paste(outcome, 
            paste(variables, collapse = " + "), 
            sep = " ~ "))
    temp1<-temp1[,c(outcome,variables)]
    temp2<-temp1[,c(variables)]
    temp1<-na.omit(temp1)
    temp2<-na.omit(temp2)
    
    model_lm<-lm(f,data=temp1)
    model_lm<- step(model_lm,k=log(nrow(temp1)))  ## BIC step-wise 
    
    #summary(model_lm)
    #cor(temp1[,c(-2)],temp1[,c(-2)]) 
    temp1<-temp2[temp2$index==max(temp2$index),]
    model_predict<-predict(model_lm,temp1)
    summary_lm<-rbind(summary_lm,data.frame(ticker=st.cr.et[i],model=paste("lm",period_data[j]),value=predict(model_lm,temp1)))
    summary_lm<-rbind(summary_lm,data.frame(ticker=st.cr.et[i],model=paste("sigma lm",period_data[j]),value=sigma(model_lm)))
    }
}
summary_lm<-spread(summary_lm,model,value)

###random forrest
#summary_rf<-data.frame(ticker=character(),model=character(),value=character())

#for  (j in 1:length(period_data)){
#   for  (i in 1:length(st.cr.et)){
#     temp1<-temp[temp$ticker==st.cr.et[i]&temp$index>max(temp$index)-period_data[j],]
#     #set foctors
#     outcome<-c("roct1")
#     variables<-c("index","ema","sma","rsi","macd","signal","roc","GSPC.Adjusted","tenyTr_twoyrTR","threemonthLibor","willshire5000","ecopolunct","DOW","CHINAUS","crudeoil","goldprice","coinbase.bitcoin","Nasdqa","coinbaseethe")
#     temp1<-na.omit[,c(outcome,variables)]
#     temp1<-na.omit(temp1)
#     model_rf<-randomForest(formula = roct1 ~ ., data = temp1, ntree = 500,      mtry = 6, importance = TRUE)
#     temp1<-temp1[temp1$index==max(temp1$index),]
#     model_predict_rf<-predict(model_rf,temp1)
#     summary_rf<-rbind(summary_rf,data.frame(ticker=st.cr.et[i],model=paste("rf",period_data[j]),value=predict(model_rf,temp1)))
#      }
# }
#summary_rf<-spread(summary_rf,model,value)
#varImpPlot(model_rf,type=2)


###  Measuring Performance
summary_mp<-data.frame(ticker=character(),metric=character(),value=numeric())
  for  (i in 1:length(st.cr.et)){
    temp1<-temp[temp$ticker==st.cr.et[i]&temp$index>max(temp$index)-period_data[1],]
    #set foctors
    summary_mp<-rbind(summary_mp,data.frame(ticker=st.cr.et[i],metric="StdDev",value=StdDev(temp1$roc)))
    summary_mp<-rbind(summary_mp,data.frame(ticker=st.cr.et[i],metric="VaR",value=VaR(temp1$roc, p=.95, method="historical")))
    summary_mp<-rbind(summary_mp,data.frame(ticker=st.cr.et[i],metric="Sharpe",value=SharpeRatio( as.ts(temp1[,16]), Rf=.005/12, FUN="StdDev")))
     }

summary_mp<-spread(summary_mp,metric,value)

### date alignment
temp_crop<-as.data.table(temp)[, max(index), by=ticker]
colnames(temp_crop)<-c("ticker","index")
temp_crop<-merge(as.data.frame(temp_crop),temp,by=c("ticker","index"),all.x = TRUE)
temp_crop$index<-period_end
###summary table
if(exists("summary_table")) {
  
  summary_table_1<-temp_crop
  summary_table_1<-merge(summary_table_1,summary_alfa,by="ticker")
  summary_table_1<-merge(summary_table_1,summary_lm,by="ticker")
#  summary_table_1<-merge(summary_table_1,summary_rf,by="ticker")
  summary_table_1<-merge(summary_table_1,summary_mp,by="ticker")
  summary_table<-rbind(summary_table,summary_table_1)  
} else {
  
summary_table<-temp_crop
summary_table<-merge(summary_table,summary_alfa,by="ticker")
summary_table<-merge(summary_table,summary_lm,by="ticker")
#summary_table<-merge(summary_table,summary_rf,by="ticker")
summary_table<-merge(summary_table,summary_mp,by="ticker")
}


}
### 

## trading log + simulation

summary_table$type[summary_table$ticker %in% etfs]<-"etfs"
summary_table$type[summary_table$ticker %in% stocks]<-"stocks"
summary_table$type[summary_table$ticker %in% crypto]<-"crypto"

write.csv(summary_table, file = "metrics.csv")

write.csv(matrix, file = "matrix.csv")

