### This program describes the sequence of actions undertaken to plot data as histogram.
### It takes 3 arguments, start date, end date, variable column number.

plot1 <- function(x="2007-02-01",y="2007-02-02",z=3){
    ## defines path to and reads txt file and replace "?" by NA
    path <- file.path(getwd(),"03_data/household_power_consumption.txt")
    file <- read.table(path,header=TRUE,sep=";", na.strings ="?")
    
    ## coerce chr to date in variable 'Date' (i.e column 1)
    file[,1] <- as.Date(file$Date, "%e/%m/%Y")
    
    ## subset rows between dates 'x' and 'y'
    data <- file[file$Date >= x & file$Date <= y,]
    
### this is interesting but not vital
### drop unused levels preserving ordering of remaining levels for variable
### data$Global_active_power[,drop=TRUE]
    
### convert 'factor/levels' to num for variables of interest (i.e columns 3 to 8)
###for (i in 3:8){
###    data[,i]<-as.numeric(levels(data[,i]))[data[,i]]
###}
    
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
    
### give information on 'data' and validate variables are num type
### str(data)
    
    ## plot data 'Global active power' (i.e 3rd column) as histogram
    hist(data[,z], main=colnames(data[z]), xlab=paste(colnames(data[z]),"(kilowatts)"),
         col="Red")
    
    ## export plot in png file in working directory
    dev.copy(png,"plot1.png", width = 480, height = 480)
    dev.off()
    
}