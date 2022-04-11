#analysis window
start_dt <- "2019-10-01"
end_dt <- "2020-03-23"


#read stock data
stock_close_data <- read.csv("stock_close.csv")
#convert date column to date-time format
stock_close_data$Date <- as.Date(stock_close_data$Date)
#extract subset of data
stock_close_data <- stock_close_data[stock_close_data$Date >= start_dt & stock_close_data$Date <= end_dt, ] 
#remove rows with na values
stock_close_data <- na.omit(stock_close_data)

#plot closing index of Nifty 50 index
plt_dta <- data.frame(apply(stock_close_data, 2, unclass))
ggplot(data=plt_dta, aes(x=Date, y=X50, group=1)) +
  geom_line(lwd=1)+
  theme_classic()+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  scale_y_discrete(guide = guide_axis(check.overlap = TRUE))+
  theme(
    axis.title = element_text(size=10, face='bold'),
    axis.text = element_text(size=10, color='black')
  )
