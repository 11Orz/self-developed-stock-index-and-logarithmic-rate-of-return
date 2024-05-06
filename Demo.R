#
# 安装并加载需要的库
install.packages("readxl")
install.packages("data.table")
library(readxl)
library(data.table)

# 读取Excel文件，假设文件路径为'Combined_Stock_Data_Fixed.xlsx'
data <- fread("Combined_Stock_Data_Fixed.xlsx")

#Combined_Stock_Data_Fixed <- read_excel("Combined_Stock_Data_Fixed.xlsx")
data <- Combined_Stock_Data_Fixed
# 计算每只股票的每日收益率
#data[, 收益率 := ((收盘 - 开盘) / 开盘) * 100]
View(data)
# 按日期计算所有股票收益率的平均值
#average_daily_return <- data[, .(平均收益率 = mean(收益率)), by = 日期]

# 打印结果查看
#print(head(average_daily_return))

#当天的算数平均数的股票指数：
stocks <- data
library(dplyr)
install.packages("writexl")
library(writexl)

index_daily <- stocks %>%
  group_by(日期) %>%
  summarise(Daily_Index = mean(收盘,na.rm = TRUE))

print(index_daily)
#输出excel
write_xlsx(index_daily,"name")
#绘图
index_daily_time_series <- ts(index_daily,start =2015 ,frequency = 365)
plot(index_daily_time_series)

#对数收益率
library(dplyr)
library(lubridate)
stocks$对数收益率 <- diff(log(data$收盘),lag = 1)
data <-data %>% 
  group_by(股票代码) %>% 
  mutate(Log_Return = log(收盘/lag(收盘,1))) 

index_log_daily <- data %>%
  group_by(日期) %>%
  summarise(Daily_Log_Index = mean(Log_Return,na.rm = TRUE))

write.csv(data,"name")
write.csv(index_log_daily,"name")
#算术平均对数收益率
stocks_log <- ts(index_log_daily$Daily_Log_Index,start = 2015,frequency = 365)
plot(stocks_log)

#判断是不是科技股：
library(stringr)
data <- data %>% 
  mutate(Iskechuang = 
           if_else(str_sub(股票代码,1,3)=="688",1,0))

data <- data %>%
  mutate(BaseWeight =
           if_else(Iskechuang == 1,1.5,1))

write.csv(data,"name")
#归一化权重
daily_weights <- data %>%
  group_by(日期) %>%
  mutate(
    Weight = BaseWeight / sum(BaseWeight)
  )

#计算归一化权重之后的指数
index_values <- daily_weights %>%
  group_by(日期) %>%
  summarise(
    IndexClose = sum(收盘*Weight)
  )

#找到在对数收益率中最高的前100只股票，对这些股票进行加权处理

top_returns <-data %>%
  filter(!is.na(Log_Return)) %>%
  group_by(股票代码,股票名称) %>%
  summarise(MaxLogReturn=
              max(Log_Return),.groups = 'drop') %>%
  arrange(desc(MaxLogReturn)) %>%
  slice_head(n = 100)
print(top_returns)
write.csv(top_returns,"name",row.names = FALSE)

#对排名前100的也进行加权
top100_data <- data %>%
  semi_join(top_returns,by = "股票代码")

top_data <- top100_data %>%
  mutate(BaseWeight = 1+
           if_else(Iskechuang==1,1.5,0))
write.csv(top_data,"name")

unique_data <- top_data %>%
  select(股票代码,股票名称) %>%
  distinct()

View(unique_data)  

write_xlsx(unique_data,"name")
#筛选出所有被加权过的成分股
weight_stocks <- top_data %>%
  filter(Iskechuang !=0 | BaseWeight != 0)
View(weight_stocks)
write.csv(weight_stocks,"name")
#计算新的加权过后的对数收益率
daily_avg_log <- weight_stocks %>%
  group_by(日期) %>%
  summarise(Avg_log_return = mean(Log_Return,na.rm = TRUE))

View(daily_avg_log)

index_log_daily_time_series <- ts(daily_avg_log$Avg_log_return,start =2015 ,frequency = 365)
plot.ts(stocks_log,
        type = 'l', col = 'blue', 
        xlab = "Date", ylab = "Log Return",
        main = "Daily Log Returns of All Stocks")
lines(index_log_daily_time_series,
      col = 'red')
legend("topright", legend=c("所有股票", "加权后的股票"), 
       col=c("blue", "red"), lty=1, cex=0.8)


#对数收益率GARCH模型

#绘制自拟指数的对数收益率：
plot(index_log_daily_time_series)

#离群值检测
#install.packages("PerformanceAnalytics")
#library(PerformanceAnalytics)
#clean.boudt()

#对原始数据进行检验：
library(aTSA)
#平稳非白噪声序列检验：
adf.test(daily_avg_log$Avg_log_return)
for(k in 1:3) print(Box.test(daily_avg_log$Avg_log_return,lag = 6*k,type = "Ljung-Box"))
#自相关图和偏自相关图：
daily_avg_log <- na.omit(daily_avg_log)
acf(daily_avg_log$Avg_log_return)
pacf(daily_avg_log$Avg_log_return)

#从MA(1)开始拟合


#使用自然定阶
install.packages("forecast")
library(forecast)

auto.arima(daily_avg_log$Avg_log_return)
fit_model <- arima(daily_avg_log$Avg_log_return ,order = c(3,0,3))
ts.diag(fit_model)

#GARCH波动性预测
install.packages("tseries")
library(tseries)
library(ggplot2)
#差分
data_1 <- daily_avg_log
data_1$diff_log <- c(NA,diff(data_1$Avg_log_return))
diff_daily_avg_log <- na.omit(data_1)
#对数收益率的garch
install.packages("rugarch")
library(rugarch)
#garch(1,1)
spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(3, 3), include.mean = TRUE),
                   distribution.model = "std")
fit <- ugarchfit(spec = spec, data = daily_avg_log$Avg_log_return)
print(fit)
summary(fit)
forecasts <- ugarchforecast(fit, n.ahead = 10)
print(forecasts)

#
# 滚动标准差（以30天为窗口）
library(zoo)
rolling_sd <- rollapply(daily_avg_log$Avg_log_return, width = 30, FUN = sd, by.column = TRUE, align = 'right')

# 绘制滚动标准差
plot(rolling_sd, type = 'l', main = "30天滚动标准差", ylab = "Standard Deviation", xlab = "Time")

