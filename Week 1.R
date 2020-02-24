# Dataset: Electric power consumption
# Reading data

setwd("~/coursera/Project/Exploratory Data Analysis")
power <- read.table("household_power_consumption.txt", header=T, sep=";")

# Cleaning data

power$Date <- as.Date(power$Date, format="%d/%m/%Y")
df <- power[(power$Date=="2007-02-01") | (power$Date=="2007-02-02"),]
df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
df$Global_reactive_power <- as.numeric(as.character(df$Global_reactive_power))
df$Voltage <- as.numeric(as.character(df$Voltage))
df <- transform(df, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")

df$Sub_metering_1 <- as.numeric(as.character(df$Sub_metering_1))
df$Sub_metering_2 <- as.numeric(as.character(df$Sub_metering_2))
df$Sub_metering_3 <- as.numeric(as.character(df$Sub_metering_3))

#Plot 1
plot1 <- function() {
    hist(df$Global_active_power, main = paste("Global Active Power"), col="red", xlab="Global Active Power (kilowatts)")
    dev.copy(png, file="plot1.png", width=480, height=480)
    dev.off()
}

# Plot 2
plot2 <- function() {
    plot(df$timestamp,df$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
    dev.copy(png, file="plot2.png", width=480, height=480)
    dev.off()
}

# Plot 3
plot3 <- function() {
    plot(df$timestamp,df$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
    lines(df$timestamp,df$Sub_metering_2,col="red")
    lines(df$timestamp,df$Sub_metering_3,col="blue")
    legend("topright", col=c("black","red","blue"), c("Sub_metering_1 ","Sub_metering_2 ", "Sub_metering_3 "),lty=c(1,1), lwd=c(1,1))
    dev.copy(png, file="plot3.png", width=480, height=480)
    dev.off()
}

# Plot 4
plot4 <- function() {
    par(mfrow=c(2,2))
    
    ## PLOT 1
    plot(df$timestamp,df$Global_active_power, type="l", xlab="", ylab="Global Active Power")
    ## PLOT 2
    plot(df$timestamp,df$Voltage, type="l", xlab="datetime", ylab="Voltage")
    
    ## PLOT 3
    plot(df$timestamp,df$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
    lines(df$timestamp,df$Sub_metering_2,col="red")
    lines(df$timestamp,df$Sub_metering_3,col="blue")
    legend("topright", col=c("black","red","blue"), c("Sub_metering_1 ","Sub_metering_2 ", "Sub_metering_3 "),lty=c(1,1), bty="n", cex=.5)
    
    ## PLOT 4
    plot(df$timestamp,df$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
    
    ## OUTPUT
    dev.copy(png, file="plot4.png", width=480, height=480)
    dev.off()
}

