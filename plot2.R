## plot2
### Global Active Power over Time
#
# summary: output a (480x480) png file in the current working directory 
#          containing the Global Active Power in kilowatts against Time 
#          for the effective dates of Feb 1st and 2nd of 2007.
#
# input: Individual household electric power consumption Data Set as
#        provided by the UC Irvine Machine Learning Repository
#
#   url: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
#
# output: PNG file containing the Global Active Power in kilowatts over
#         Time named "plot2.png" in the current working directory.
#
# requirements: "RCurl" library
#
library(RCurl)

plot2 <- function() {
  data <- fetch_data()

  png("plot2.png", bg = "transparent") # default is 480 X 480

  plot(data$Time, data$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")

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
