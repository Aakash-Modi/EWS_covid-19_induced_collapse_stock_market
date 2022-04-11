##data(stock_close_data)
#based on the stock index
st_dt = "2019-10-01"  #analysis window start date 
ct_dt = "2020-02-19"  #date just prior to critical transition

##parameters
win_sz = 50
bndwth = 5
smoothing = 'gaussian'


##column number for different stock index
#  2 --> Nifty 50 or X50 stock index
#  3 --> Nifty Auto index
#  4 --> Nifty Bank index
#  5 --> Nifty Consumer Durables index
#  6 --> Nifty Financial Services index
#  7 --> Nifty FMCG index
#  8 --> Nifty Healthcare index
#  9 --> Nifty IT index
# 10 --> Nifty Media index
# 11 --> Nifty Metal index
# 12 --> Nifty Oil & Gas index
# 13 --> Nifty Pharma index
# 14 --> Nifty Realty index
stk_idx = 2


##########
#EWS generation
st_in <- tail(which(stock_close_data$Date<=st_dt, arr.ind=TRUE), n=1)  #start date index
tr_in <- tail(which(stock_close_data$Date<=ct_dt, arr.ind=TRUE), n=1)  #transition date index
out=generic_ews(as.numeric(as.character(stock_close_data[st_in:tr_in,stk_idx])),winsize=win_sz, detrending=smoothing,
                bandwidth=bndwth,logtransform=FALSE,interpolate=FALSE)  #early warning signal


##########
##whole time series in analysis window
timeseries1 <- as.matrix(stock_close_data[,stk_idx])
Y1 = timeseries1
timeindex1 = 0:(dim(timeseries1)[1]-1)
bw1 <- round(length(Y1) * 5/100)
#smoothing
smYY1 <- ksmooth(timeindex1, Y1, kernel = "normal", bandwidth = bw1, range.x = range(timeindex1), x.points = timeindex1)
nsmY1 <- Y1 - smYY1$y
smY1 <- smYY1$y

##time series upto critical transition
timeseries <- as.matrix(stock_close_data[st_in:tr_in,stk_idx])
Y = timeseries
timeindex = 0:(dim(timeseries)[1]-1)
bw <- round(length(Y) * 5/100)
#smoothing
smYY <- ksmooth(timeindex, Y, kernel = "normal", bandwidth = bw, range.x = range(timeindex), x.points = timeindex)
nsmY <- Y - smYY$y
smY <- smYY$y


##########
##rearrange data for indicator calculation
mw <- round(length(Y) * 50/100)
omw <- length(nsmY) - mw + 1  ##number of moving windows
low <- 2
high <- mw
nMR <- matrix(data = NA, nrow = mw, ncol = omw)
x1 <- 1:mw
for (i in 1:omw) {
  Ytw <- nsmY[i:(i + mw - 1)]
  nMR[, i] <- Ytw
}

nARR <- out[,2]  #lag-1 auto-regression
nACF <- out[,9]  #lag-1 auto-correlation function
nSD <- out[,3]  #standard deviation
nSK <- out[,4]  #skewness
nKURT <- out[,5]  #kurtosis
nRETURNRATE <- out[,7]  #return rate
timevec <- seq(1, length(nARR))
KtAR <- cor.test(timevec, nARR, alternative = c("two.sided"), method = c("kendall"),  #kendall tau value of lag-1 auto-regression
                 conf.level = 0.95)
KtACF <- cor.test(timevec, nACF, alternative = c("two.sided"), method = c("kendall"),  #kendall tau value of lag-1 auto-correlation
                  conf.level = 0.95)
KtSD <- cor.test(timevec, nSD, alternative = c("two.sided"), method = c("kendall"),  #kendall tau value of standard deviation
                 conf.level = 0.95)
KtSK <- cor.test(timevec, nSK, alternative = c("two.sided"), method = c("kendall"),  #kendall tau value of skewness
                 conf.level = 0.95)
KtKU <- cor.test(timevec, nKURT, alternative = c("two.sided"), method = c("kendall"),  #kendall tau value of kurtosis
                 conf.level = 0.95)
KtRETURNRATE <- cor.test(timevec, nRETURNRATE, alternative = c("two.sided"),  #kendall tau value of return rate
                         method = c("kendall"), conf.level = 0.95)

##########
dev.new()
#par(mar = (c(0, 2, 0, 1) + 0), oma = c(7, 2, 3, 1), mfrow = c(5, 1), cex = 1.5)
#par(mar = (c(0.2,5,0.2,0)+0), mfrow = c(3,1), cex = 0.95, oma = c(3,2,1,1))
par(mar = (c(0.2,5,0.2,0.6)+0.1), oma = c(3, 0, 0, 0), mfrow=c(6,1), cex = 1, xaxs = "i")


##########
##original time series plot
plot(timeindex1, Y1, type = "l", ylab = "", xlab = "", lwd=2, xaxt = "n", las = 1, xlim = c(timeindex1[1],
                                                                                     timeindex1[length(timeindex1)]))
title(ylab="CI", line=4, cex.lab=1.2)
lines(timeindex, smY, type = "l", ylab = "", xlab = "", lwd=1, xaxt = "n", col = 2,
      las = 1, xlim = c(timeindex1[1], timeindex1[length(timeindex1)]))
#abline(v = c(mw,length(smY)), col="blue", lwd=3, lty=2)
abline(v = c(length(smY)-1), col="blue", lwd=1.5, lty=1)
abline(v = c(mw-1), col="blue", lwd=1.5, lty=2)
arrows(x0=0, x1=mw-1, y0=(min(Y1)+1000), length=0.125, code=3, lwd = 1.5)
legend("topright", paste0("(i)"),bty = "n")


##########
##lag-1 auto correlation
plot(timeindex[mw:length(nsmY)], nACF, ylab = "", xlab = "", type = "l", lwd=2, xaxt = "n",
     las = 1, xlim = c(timeindex1[1], timeindex1[length(timeindex1)]))  #4
title(ylab="AC", line=4, cex.lab=1.2)
abline(v = c(length(smY)-1), col="blue", lwd=1.5, lty=1)
abline(v = c(mw-1), col="blue", lwd=1.5, lty=2)
legend("left", paste("Trend = ",round(KtACF$estimate, digits = 3)),
       bty = "n")
legend("topright", paste0("(ii)"),bty = "n")


##########
##standard deviation
# plot(timeindex[mw:length(nsmY)], nSD, ylab = "", xlab = "", type = "l", lwd=2, cex.lab = 1,
#      las = 1, xlim = c(timeindex1[1], timeindex1[length(timeindex1)]))

plot(timeindex[mw:length(nsmY)], nSD, ylab = "", xlab = "", type = "l", lwd=2, xaxt = "n",
     las = 1, xlim = c(timeindex1[1], timeindex1[length(timeindex1)]))
title(ylab="SD", line=4, cex.lab=1.2)
abline(v = c(length(smY)-1), col="blue", lwd=1.5, lty=1)
abline(v = c(mw-1), col="blue", lwd=1.5, lty=2)
legend("left", paste("Trend = ",round(KtSD$estimate, digits = 3)),
       bty = "n")
legend("topright", paste0("(iii)"),bty = "n")


##########
##return rate
plot(timeindex[mw:length(nsmY)], nRETURNRATE, ylab = "", xlab = "", type = "l", lwd=2, xaxt = "n", 
     las = 1, xlim = c(timeindex1[1], timeindex1[length(timeindex1)]))  #4
title(ylab="RR", line=4, cex.lab=1.2)
abline(v = c(length(smY)-1), col="blue", lwd=1.5, lty=1)
abline(v = c(mw-1), col="blue", lwd=1.5, lty=2)
legend("left", paste("Trend = ",round(KtRETURNRATE$estimate, digits = 3)), 
       bty = "n")
legend("topright", paste0("(iv)"),bty = "n")


##########
##skewness
plot(timeindex[mw:length(nsmY)], nSK, type = "l", ylab = "", xlab = "", las = 1, lwd=2, xaxt ="n",
     xlim = c(timeindex1[1], timeindex1[length(timeindex1)]))
title(ylab="SKEW", line=4, cex.lab=1.2)
abline(v = c(length(smY)-1), col="blue", lwd=1.5, lty=1)
abline(v = c(mw-1), col="blue", lwd=1.5, lty=2)
legend("left", paste("Trend = ",round(KtSK$estimate, digits = 3)), 
       bty = "n")
legend("topright", paste0("(v)"),bty = "n")


##########
##kurtosis
plot(timeindex[mw:length(nsmY)], nKURT, type = "l", ylab = "", xlab = "", las = 1, lwd=2,
     cex.lab = 1, xlim = c(timeindex1[1], timeindex1[length(timeindex1)]))
title(ylab="KURT", line=4, cex.lab=1.2)
abline(v = c(length(smY)-1), col="blue", lwd=1.5, lty=1)
abline(v = c(mw-1), col="blue", lwd=1.5, lty=2)
legend("left", paste("Trend = ",round(KtKU$estimate, digits = 3)), 
       bty = "n")
legend("topright", paste0("(vi)"),bty = "n")


mtext("t (Days)", side = 1, line = 2, cex = 1.2)

