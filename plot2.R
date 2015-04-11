### This program describes the sequence of actions undertaken to plot data as scatter plot.
### It takes 3 arguments, start date, end date, variable column number.

plot2 <- function(x="2007-02-01",y="2007-02-02",z=3){
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
    
    ## plot data 'Global active power' (i.e 3rd column) as scatter plot type line
    plot(data$Time, data[,z], "l", xlab= "", ylab=paste(colnames(data[z]),"(kilowatts)"),
         font.axis = 1)
    
    ## export plot in png file in working directory
    dev.copy(png,"plot2.png", width = 480, height = 480)
    dev.off()
}