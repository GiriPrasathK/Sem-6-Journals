df<-read.csv("Month_Value_1 - Month_Value_1.csv")
df
df<-na.omit(df)
df

ts_data<-ts(df$Revenue,start=c(2015,1),frequency=12)
ts_data
png("tsdata.png",width=800,height=600)
plot(ts_data)
dev.off()

de<-decompose(ts_data)
png("myplot2.png",width=800,height=600)
plot(de)
dev.off()

library(tseries)
adf<-adf.test(ts_data)
adf

d_data<-diff(ts_data)
d_data
png("diffplot.png",width=800,height=600)
plot(d_data)
dev.off()

de1<-decompose(d_data)
png("myplot3.png",width=800,height=600)
plot(de1)
dev.off()

acf(d_data,lag.max=6)
pacf(d_data,lag.max=6)

install.packages("forecast")
library(forecast)

