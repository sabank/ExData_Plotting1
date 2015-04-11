### This program describes the sequence of actions undertaken to create multiple plots
### on the same graphic device.
### It takes 3 arguments, start date, end date, variable columns number.

plot4 <- function(x="2007-02-01",y="2007-02-02",z=1:9){
    ## defines path to and reads txt file and replace "?" by NA
    path <- file.path(getwd(),"03_data/household_power_consumption.txt")
    file <- read.table(path,header=TRUE,sep=";", na.strings ="?")
    
    ## coerce chr to date in variable 'Date' (i.e column 1)
    file$Date <- as.Date(file$Date, "%e/%m/%Y")
    
    ## subset rows between dates 'x' and 'y'
    data <- file[file$Date >= x & file$Date <= y,]
    
    ## convert chr to time in variable 'Time' (i.e column 2)
    data$Time <- as.POSIXct(paste(data$Date, as.character(data$Time)))
    
    ## prepare header for plot's title and labels
    # replace "_" by " " in columns name
    colnames(data)<-gsub("_"," ",colnames(data))
    # split header name when reaching " ". Result is a 'list' of items composed of words.
    headername <- strsplit(colnames(data), " ")
    # casefolding to uppercase the 1st letter of each word in each item of the list
    for (i in 1:length(headername)){
        # take 1st letter of each word
        l1 <- substring(headername[[i]],1,1)
        # take remaining letters of each word
        l2 <- substring(headername[[i]],2)
        # translate all 1st letters in upper case
        l1 <- toupper(l1)
        # concatenate strings l1 and l2 and replace each item of the list
        # paste0(...,collapse) equivalent to paste(...,sep="",collapse)
        headername[[i]] <- paste0(l1, l2, collapse=" ")
    }
    # update columns name
    colnames(data)<-headername
    
    ## open de graphic device
    png("plot4.png", 480, 480, "px")
    
    ## modify template to draw 4 plots over 2 rows and 2 columns in png file
    par(mfcol=c(2,2))
    
    ## plot data 'Global active power' (i.e 3rd column) as scatter plot type line
    plot(data$Time, data[,z[3]], "l", xlab= "", ylab=colnames(data[z[3]]))
    
    ## plot data 'Sub metering 1, 2, 3' (i.e columns 7 to 9) as line
    # create empty plot based on 'Time' and 'Sub metering 1'
    plot(data$Time,data[,z[7]], type = "n", xlab="", ylab= "Energy sub metering")
    # add data 'Sub metering' with a dedicated line color
    linecolor = c("Green","Yellow","Grey","Brown","Purple","Orange","Black","Red","Blue")
    for (i in z[7:9]){
        lines(data$Time, data[,i], col = linecolor[i])
    }
    # add legend
    legend("topright", lty = 1, col = linecolor[z[7:9]], legend = colnames(data[z[7:9]]),
           bty="n", cex = 0.7)
    
    ## plot data 'Voltage'(i.e 5th column) as scatter plot tyoe line
    plot(data$Time, data[,z[5]], "l", xlab= "datetime", ylab=colnames(data[z[5]]))
    
    ## plot data 'Global reactive power' (i.e 4th column) as scatter plot type line
    plot(data$Time, data[,z[4]], "l", xlab= "datetime", ylab=colnames(data[z[4]]))
    
    ## close the graphic device
    dev.off()
}