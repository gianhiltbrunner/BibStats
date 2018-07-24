library(readr)
library(jsonlite)
library(readr)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

download.file("http://192.168.1.224/visitors.json", destfile = "./visitors.json", method="curl")
json <- read_file("./visitors.json")

if (substrRight(json,1) == ","){
  json <- substr(json, 1, nchar(json)-1)
}

write("[\n",file="visitors-mod.json")
write(json,file="visitors-mod.json",append=TRUE)
write("]",file="visitors-mod.json",append=TRUE)

visitors <- fromJSON("visitors-mod.json")

#visitors <- read_csv("visitors.csv")

visitors$day <- weekdays(as.POSIXct(visitors$ts))
t.str <- strptime(visitors$ts, "%Y-%m-%dT%H:%M:%S")
visitors$hour <- as.numeric(format(t.str, "%H"))# + as.numeric(format(t.str, "%M"))/60
visitors$hour <- factor(visitors$hour)

visitors<-visitors[!(visitors$library_is_closed=="True"),]

fit <- glm(overall/overall_max~hour*day,family=binomial,data = visitors)
summary(fit)

newDat <- data.frame(hour="9",day="Monday")
pred <- predict(fit, newdata = newDat)
summary(pred)[4]*visitors$overall_max[1]

#################
setDay <- "Tuesday"
x <- numeric(length = 18-8)
for (i in (8:18)){
  newDat <- data.frame(hour=as.character(i),day=setDay)
  pred <- predict(fit, newdata = newDat)
  x[i] <- summary(pred)[4]*visitors$overall_max[1]
}
plot(x,xlim = c(8,18),ylim = c(0,150),main=paste("Visits of Infozentrun on", setDay),xlab = "Time",ylab = "Visitors")
lines(x)
