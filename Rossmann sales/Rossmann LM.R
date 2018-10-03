# Read and process
path = "/Users/garethalex/Desktop/Year 4/ST4240/Assignment 1/Q1_rossmann"
setwd(path)
dat = read.csv("./dataset/train.csv", header = T)
dim(dat)
dat = dat[dat$Open==1,] # Remove close data
dat[dat$Sales!=0,]
dim(dat)
n = ncol(dat)

# A plot of Sales vs weekdays
jpeg('dayboxplot.jpg')
boxplot(Sales~DayOfWeek, data = dat, 
        ylab = "Sales", xlab = "Day of the Week")
dev.off()


boxplot(Sales~cbind(StateHoliday), data = dat)

boxplot(Sales~WeeksOfDay_num, data = dat,
        ylab = "Sales", xlab = "Week of the Year")

# Create new variable of weeks of the day
WeeksOfDay_num = strftime(dat$Date, format = "%V")
WeeksOfDay = rep("Week 1-50", length(WeeksOfDay_num))


# Treat last 2 weeks as a whole
WeeksOfDay[ WeeksOfDay_num==51 | 
             WeeksOfDay_num==52] = "Week 51-52"
WeeksOfDay = as.factor(WeeksOfDay)

newDat = data.frame(cbind(dat$Sales, WeeksOfDay))
colnames(newDat) <- c("Sales", "WeeksOfDay")
head(newDat)

# Exploration
mean(dat$Sales[WeeksOfDay==1]) # 9809.844
mean(dat$Sales[WeeksOfDay==0]) # 6883.875
boxplot(Sales~WeeksOfDay, data = newDat,
        ylab = "Sales", xlab = "Week of the Year")

sum(dat$StateHoliday=='c') #71
sum(dat$StateHoliday=='b') #145
sum(dat$StateHoliday=='a') #694
sum(dat$SchoolHoliday==1) #163457

# Train-Test split
dat_train = dat[round(n*0.8),]
dat_test = dat[n,]

nstore = max(dat$Store)
fit_total = numeric(nstore)
error_hist = numeric(nstore)

pacf(Sales$V1)


# Fit a linear model
for (i in 1:nstore) {
  store_dat = read.csv(paste("./store_data/", i,".csv",sep=""), header = T)
  store_dat = store_dat[store_dat$Open==1,]
  store_dat = store_dat[store_dat$Sales!=0,]
  
  attach(store_dat)
  
  n = nrow(store_dat)
  DaysOfWeek = numeric(n)
  DaysOfWeek[DayOfWeek==1] = 1
  DaysOfWeek[DayOfWeek==6] = 2
  DaysOfWeek[DayOfWeek==7] = 3
  DaysOfWeek = as.factor(DaysOfWeek)
  
  WeeksOfDay = as.factor(strftime(Date, format = "%V"))
  Promo = as.factor(Promo)
  
  # Covariates: DaysOfWeek, WeeksOfDay, Customers, Promo
  X = cbind(DaysOfWeek, WeeksOfDay, Customers, Promo)
  y = Sales
  train_idx = 1:round(n*0.7)
  data <- data.frame(cbind(y=y,X))
  train <- data[train_idx, ]
  val <- data[-train_idx, ]
  
  # fit = glm(y ~ ., data = train, family=poisson)
  fit = lm(y ~ ., data = train)
  y_predict = predict(fit, newdata=val[,-1],type='response')
  y_test = val[,1]
  error_rate = sqrt(mean(abs(y_predict-y_test)/y_test)**2)
  fit_total[i] = fit
  error_hist[i] = error_rate
  
  detach(store_dat)
  remove(Promo)
}


mean(error_hist[error_hist != Inf]) # 0.05873835


# Treat week variable as a 0/1 categorical variable, repeat the process
nstore = max(dat$Store)
fit_total = numeric(nstore)
error_hist = numeric(nstore)
for (i in 1:nstore) {
  store_dat = read.csv(paste("./store_data/", i,".csv",sep=""), header = T)
  store_dat = store_dat[store_dat$Open==1,]
  attach(store_dat)
  
  n = nrow(store_dat)
  DaysOfWeek = numeric(n)
  DaysOfWeek[DayOfWeek==1] = 1
  DaysOfWeek[DayOfWeek==6] = 2
  DaysOfWeek[DayOfWeek==7] = 3
  DaysOfWeek = as.factor(DaysOfWeek)
  
  WeeksOfDay_num = strftime(Date, format = "%V")
  WeeksOfDay = rep(0, length(WeeksOfDay_num))
  WeeksOfDay[ WeeksOfDay_num==51 | 
                WeeksOfDay_num==52] = 1
  WeeksOfDay = as.factor(WeeksOfDay)
  
  Promo = as.factor(Promo)
  SchoolHoliday = as.factor(SchoolHoliday)
  
  # Covariates: DaysOfWeek, WeeksOfDay, Customers, Promo, SchoolHoliday
  X = cbind(DaysOfWeek, WeeksOfDay, Customers, Promo, SchoolHoliday)
  y = Sales
  train_idx = 1:round(n*0.7)
  data <- data.frame(cbind(y=y,X))
  train <- data[train_idx, ]
  val <- data[-train_idx, ]
  
  # fit = glm(y ~ ., data = train, family=poisson)
  fit = lm(y ~ ., data = train)
  y_predict = predict(fit, newdata=val[,-1],type='response')
  y_test = val[,1]
  error_rate = sqrt(mean(abs(y_predict-y_test)/y_test)**2)
  fit_total[i] = fit
  error_hist[i] = error_rate
  
  detach(store_dat)
  remove(Promo)
  remove(SchoolHoliday)
}

mean(error_hist[error_hist != Inf]) # 0.05756397











# Add SchoolHoliday
nstore = max(dat$Store)
fit_total = numeric(nstore)
error_hist = numeric(nstore)
for (i in 1:nstore) {
  store_dat = read.csv(paste("./store_data/", i,".csv",sep=""), header = T)
  store_dat = store_dat[store_dat$Open==1,]
  attach(store_dat)
  
  n = nrow(store_dat)
  DaysOfWeek = numeric(n)
  DaysOfWeek[DayOfWeek==1] = 1
  DaysOfWeek[DayOfWeek==6] = 2
  DaysOfWeek[DayOfWeek==7] = 3
  DaysOfWeek = as.factor(DaysOfWeek)
  
  WeeksOfDay = as.factor(strftime(Date, format = "%V"))
  Promo = as.factor(Promo)
  SchoolHoliday = as.factor(SchoolHoliday)
  
  # Covariates: DaysOfWeek, WeeksOfDay, Customers, Promo, SchoolHoliday
  X = cbind(DaysOfWeek, WeeksOfDay, Customers, Promo, SchoolHoliday)
  y = Sales
  train_idx = 1:round(n*0.7)
  data <- data.frame(cbind(y=y,X))
  train <- data[train_idx, ]
  val <- data[-train_idx, ]
  
  # fit = glm(y ~ ., data = train, family=poisson)
  fit = lm(y ~ ., data = train)
  y_predict = predict(fit, newdata=val[,-1],type='response')
  y_test = val[,1]
  error_rate = sqrt(mean(abs(y_predict-y_test)/y_test)**2)
  fit_total[i] = fit
  error_hist[i] = error_rate
  
  detach(store_dat)
  remove(Promo)
  remove(SchoolHoliday)
}

mean(error_hist[error_hist != Inf]) # 0.05889106


jpeg('1115.jpg')
plot(y_test, xlab = "DateIndex", ylab = "Sales",
     type="b",col="blue", lwd=1.5, lty=1,pch=20)
lines(y_predict, type="b",col="green", lwd=1.5,lty=1,pch=18)
title("Predict vs Actual Sales", "An example store")
legend(0, 12500, c("Actual","Predict"), col = c("blue","green"),
       lty=c(1,1), cex=0.8)
dev.off()





