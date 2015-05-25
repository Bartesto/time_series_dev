# Make TS Data
# This function:
#         imports from cloud QA'd csv
#         constructs daily ts to 'regularise' ts
#         compresses ts to monthly observations
#         interpolates (linear) missing values
#         writes new data to csv
#
# by Bart Huntley 18/05/2015

rm(list=ls())



dir="Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\cusum"
csv="test_data_2015_orig.csv"
project="dhi"


mtsd <- function(dir, csv, project) {
        
        is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
        
        load_or_install<-function(package_names)  
        {  
                for(package_name in package_names)  
                {  
                        if(!is_installed(package_name))  
                        {  
                                install.packages(package_name,repos="http://cran.csiro.au/")  
                        }  
                        library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)  
                }  
        }  
        
        load_or_install(c("lubridate","dplyr","zoo"))
        
        setwd(dir)
        df <- read.csv(csv, header = TRUE)
        df$date <- dmy(df$date)#create date here to order by
        df <- df[order(df$date),]#Very Important - order it!
        all <- seq(df[1,1], tail(df[,1], n=1), by = 'days') #daily date sequence for whole period
        d <- rep('NA', length(all)) #dummy data column for data frame
        alldates <- data.frame(date = all, val = d)#make df of complete date seq
        df2 <- left_join(alldates, df, by = "date")#join datasets for whole seq
        df2 <- df2[,-2]#drop dummy column
        df3 <- df2 %>%
                mutate(month = month(date), year = year(date)) %>%
                group_by(year, month) %>%
                summarise_each(funs(mean(., na.rm = TRUE)))
        ##Need to 'create' fictitious start and end dates to give sensible summary dates
        end_day <- ifelse(day(df[1,1])> 28, 28, day(df[1,1]))#handles a > 28day start day for leap years
        start_day <- end_day
        start_date <- ymd(paste0(as.character(year(df[1,1])),
                                 "-", as.character(month(df[1,1])),
                                 "-", as.character(start_day)))
        end_date <- ymd(paste0(as.character(tail(df3[,1], n=1)), 
                               "-", as.character(tail(df3[,2], n=1)), 
                               "-", as.character(end_day)))#end date 
        df3[,3] <- seq(start_date, end_date, by = 'months')#clean date vals to reg day of each mth
        df3 <- df3[,c(-1,-2)]#drop year and mth columns
        df4 <- data.frame( df3[,1], na.approx(df3[,-1], rule=2))
        write.csv(df4, file = paste0(project, "_mtsd.csv"))
        
}