# plot4
### Global Active Power, Voltage, Energy Sub Metering and Global Reactive Power Charts
#
# summary: output a (480x480) png file in the current working directory 
#          containing four charts displaying the Global Active Power, 
#	   Voltage, Energy sub Metering and Global Reactive Power measures
#          against Time for the effective dates of Feb 1st and 2nd of 2007.
#
# input: Individual household electric power consumption Data Set as
#        provided by the UC Irvine Machine Learning Repository
#
#   url: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
#
# output: PNG file containing the four charts as described above against
#         Time named "plot4.png" in the current working directory.
#
# measurments: 
#    Global Active Power: in kilowatts
#    Voltage: in kilowatts
#    Energy sub metering: in watt-hours
#      Sub_metering_1: Kitchen (dishwasher, oven, microwave)
#      Sub_metering_2: Laundry (washing-machine, tumble-drier, refrigerator, light
#      Sub_metering_3: water-heater and air-conditioner
#    Global reactive power: in kilowatts
#
# requirements: "RCurl" library
#
library(RCurl)

plot4 <- function() {
  data <- fetch_data()

  png("plot4.png", bg = "transparent") # default is 480 X 480

  par(mfrow = c(2,2))
  with(data, {									   # PLOTS
    plot(Time, Global_active_power, type="l", xlab="", ylab="Global Active Power") # Global Active Power
    plot(Time, Voltage, type="l", xlab="datetime", ylab="Voltage")		   # Voltage
    plot(Time, Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")	   # Energy Sub Metering
      points(Time, Sub_metering_2, type="l", col="red")				     # Sub metering 2
      points(Time, Sub_metering_3, type="l", col="blue")			     # Sub metering 3
      legend("topright", box.lwd = 0, box.col = "white", bg = "white",
             legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
             lty=c(1,1), lwd=c(2.5,2.5), col=c("black","red","blue") )
										   # Global Reactive Power
    plot(Time, Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
    })

    dev.off()
}

## fetch_data()
#
# returns a data.frame object with the following columns:
#
#   Date - general days lookup - formatted as a Date
#   Time - combines Date column with itself into a more specific POSIXlt object
#   Global_active_power
#   Global_reactive_power
#   Voltage
#   Global_intensity
#   Sub_metering_1
#   Sub_metering_2
#   Sub_metering_3
#
#   Note: this data.frame only contains data derived from Feb 1st and 2nd of 2007
fetch_data <- function () {
  url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
  
  # Auto-Magically create a date object as we read in the file
  setClass("myDate") # avoid harmelss warnings...
  setAs("character","myDate",function(from) as.Date(from, format("%d/%m/%Y")))
  
  # open a temporary file
  tmp <- tempfile()
  download.file(url, tmp, method="curl")
  con <- unz(tmp, 'household_power_consumption.txt') # unzip the file
  data <- read.table(con, header=TRUE, sep=";", comment.char="",
                     colClasses=c('myDate','character','numeric','numeric','numeric','numeric','numeric',
                                  'numeric','numeric'), na.strings="?")

  unlink(tmp) # remove the temporary file
  
  # extract only the dates: "2007-02-01" and "2007-02-02"
  data <- data[grepl("2007-02-0[12]", strftime(data$Date, "%Y-%m-%d")),]
  row.names(data) <- NULL
  
  # create a valid time object by combining the date/time into the Time col 
  data$Time <- with(data, strptime(paste(strftime(data$Date, "%Y-%m-%d"),data$Time, sep=""), "%Y-%m-%d %H:%M:%S"))

  data
}
