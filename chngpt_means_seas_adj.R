# This function uses the "changepoint" package to create a ggplot object of a 
# time series of cover values showing where breaks in means occur.
# 
# The function uses the data frame "...mtsd.csv" as the starting point.
# This data frame must be created first.
# 
# By Bart Huntley 25/05/2015

rm(list=ls())

dir="Z:\\DEC\\Vegmachine_SharkBay\\Working\\Bernier_Dorre\\Working\\analysis"
csv="Bernier_Dorre_mtsd.csv"

       
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
load_or_install(c("lubridate","ggplot2", "dplyr","tidyr", "grid", "gridExtra",
                  "changepoint"))

##Generic tasks
setwd(dir)
df <- read.csv(csv, header = TRUE)
df <- df[,-1]
df[,1] <- as.Date(df[,1])

sname <- names(df)[-1]


i=1#site 1

df2.i <- df[, c(1, 1+i)]
df2.i$label <- factor(rep("ts", by=length(df2.i[,1])))

##Normal
#Helper function
indexer <- function(){
        ev <- vector(mode="numeric", length=length(cptsNorm))
        for(j in 1:length(cptsNorm)){
                ev[j] <- as.numeric(df2.i[cptsNorm[j],1])
        }
        ev
}
#Changepoint
site.ts <- ts(df2.i[,2], frequency=12, 
              start=c(as.numeric(year(df2.i[1,1])),
                      as.numeric(month(df2.i[1,1]))),
              end=c(as.numeric(year(tail(df2.i[,1], n=1))),
                    as.numeric(month(tail(df2.i[,1], n=1)))))


mvalueNorm <- cpt.mean(site.ts, method="BinSeg")

cptsNorm <- mvalueNorm@cpts

#raw dates of chgpts - mix of start and end dates for periods
date1 <- c(df[1,1], as.Date(indexer()))

cptdfNorm <- data.frame(start=date1[1:length(date1)-1],
                    end=date1[-1], y=mvalueNorm@param.est$mean, label=factor("mean"))
#df for plotting
norm <- data.frame(x=df2.i[,1], y=site.ts)

##seas adjusted
#Helper function
indexer2 <- function(){
        ev <- vector(mode="numeric", length=length(cptsSea))
        for(j in 1:length(cptsSea)){
                ev[j] <- as.numeric(df2.i[cptsSea[j],1])
        }
        ev
}
#Changepoint
site.ts <- ts(df2.i[,2], frequency=12, 
              start=c(as.numeric(year(df2.i[1,1])),
                      as.numeric(month(df2.i[1,1]))),
              end=c(as.numeric(year(tail(df2.i[,1], n=1))),
                    as.numeric(month(tail(df2.i[,1], n=1)))))
site.ts.components <- decompose(site.ts)
site.ts.SA <- site.ts - site.ts.components$seasonal

mvalueSea <- cpt.mean(site.ts.SA, method="BinSeg")

cptsSea <- mvalueSea@cpts

#raw dates of chgpts - mix of start and end dates for periods
date2 <- c(df[1,1], as.Date(indexer2()))

cptdfSea <- data.frame(start=date2[1:length(date2)-1],
                    end=date2[-1], y=mvalueSea@param.est$mean, label=factor("mean"))
#df for plotting
seas <- data.frame(x=df2.i[,1], y=site.ts.SA)

## Trend only
#Changepoint
site.ts <- ts(df2.i[,2], frequency=12, 
              start=c(as.numeric(year(df2.i[1,1])),
                      as.numeric(month(df2.i[1,1]))),
              end=c(as.numeric(year(tail(df2.i[,1], n=1))),
                    as.numeric(month(tail(df2.i[,1], n=1)))))
site.ts.components <- decompose(site.ts)

site.ts.TR <- site.ts.components$trend
site.ts.TR.df <- as.data.frame(site.ts.TR)#put in df to assist in removal of NA's
na.index <- !is.na(site.ts.TR.df)#make index of non-NA values
site.ts.TR.df <- site.ts.TR.df[na.index]#use index to subset trend values
df2.i.TR <- df2.i$date[na.index]#use index to subset dates to match


mvalueTre <- cpt.mean(site.ts.TR.df, method="BinSeg")

cptsTre <- mvalueTre@cpts

indexer3 <- function(){
        ev <- vector(mode="numeric", length=length(cptsTre))
        for(j in 1:length(cptsTre)){
                ev[j] <- as.numeric(df2.i.TR[cptsTre[j]])#different here df of 1 column unlike df2.i
        }
        ev
}

#raw dates of chgpts - mix of start and end dates for periods
date3 <- c(df[1,1], as.Date(indexer3()))

cptdfTre <- data.frame(start=date3[1:length(date3)-1],
                     end=date3[-1], y=mvalueTre@param.est$mean, label=factor("mean"))


#df for plotting
trdf <- data.frame(x=df2.i.TR, y=site.ts.TR.df)

##Plots
#Normal series
ggplot()+
        geom_point(data=norm, aes(x=x, y=y), colour="black")+
        geom_line(data=norm, aes(x=x, y=y), colour="black")+
        geom_segment(data=cptdfNorm, aes(x=start, y=y, xend=end, yend=y), colour="red")+
        coord_cartesian(ylim = c(-10, 80))+
        theme_bw()+
        xlab("")+
        ylab("Vegetation Cover %")+
        ggtitle("Normal")+
        theme(legend.justification=c(0,1), 
              legend.position=c(0,1),
              axis.title.y= element_text(size=15),
              axis.text.y = element_text(angle=90, size=15),
              axis.text.x = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 15)) 
                
#Seasonally adjusted series
ggplot()+
        geom_point(data=seas, aes(x=x, y=y), colour="black")+
        geom_line(data=seas, aes(x=x, y=y), colour="black")+
        geom_segment(data=cptdfSea, aes(x=start, y=y, xend=end, yend=y), colour="red")+
        coord_cartesian(ylim = c(-10, 80))+
        theme_bw()+
        xlab("")+
        ylab("Vegetation Cover %")+
        ggtitle("Seasonally Adjusted")+
        theme(legend.justification=c(0,1), 
              legend.position=c(0,1),
              axis.title.y= element_text(size=15),
              axis.text.y = element_text(angle=90, size=15),
              axis.text.x = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 15)) 

#Trend of series
ggplot()+
        geom_point(data=trdf, aes(x=x, y=y), colour="black")+
        geom_line(data=trdf, aes(x=x, y=y), colour="black")+
        geom_segment(data=cptdfTre, aes(x=start, y=y, xend=end, yend=y), colour="red")+
        coord_cartesian(ylim = c(-10, 80))+
        theme_bw()+
        xlab("")+
        ylab("Vegetation Cover %")+
        ggtitle("Trend")+
        theme(legend.justification=c(0,1), 
              legend.position=c(0,1),
              axis.title.y= element_text(size=15),
              axis.text.y = element_text(angle=90, size=15),
              axis.text.x = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 15)) 

