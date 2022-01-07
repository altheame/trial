## Reading and processing the data
power_consumption <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

## Formatting date to Type Date
power_consumption$Date <- as.Date(power_consumption$Date, "%d/%m/%Y")
## Filtering data set from: 2007-02-01 and 2007-02-02
power_consumption <- subset(power_consumption, Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
## Removing observations with missing values
power_consumption <- power_consumption[complete.cases(power_consumption),]
## Combining Date and Time column
dateTime <- paste(power_consumption$Date, power_consumption$Time)
## Naming the vector
dateTime <- setNames(dateTime, "DateTime")
## Removing Date and Time column
power_consumption <- power_consumption[ ,!(names(power_consumption) %in% c("Date","Time"))]
## Addng DateTime column
power_consumption <- cbind(dateTime, power_consumption)
## Formatting dateTime Column
power_consumption$dateTime <- as.POSIXct(dateTime)

## Plot 1
hist(power_consumption$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")

## Plot 2
plot(power_consumption$Global_active_power~power_consumption$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

## Plot 3
with(power_consumption, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

## Plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(power_consumption, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})