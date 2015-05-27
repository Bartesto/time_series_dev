# This script creates elbow plots. It takes a cpt.mean function call
# from the changepoint package and computes 1000 iterations of increasing
# penalty values, the number of the corresponding change points detected
# and plots them against each other. The idea behind this is to determine
# at what point the penalty value might represent a reasonable number of 
# change points.
#
# The script uses the data frame "...mtsd.csv" as the starting point.
# This data frame must be created first.
# 
# By Bart Huntley 26/05/2015

dir="Z:\\DEC\\Vegmachine_SharkBay\\Working\\Bernier_Dorre\\Working\\analysis"
csv="Bernier_Dorre_mtsd.csv"
out=".jpeg"

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


setwd(dir)
df <- read.csv(csv, header = TRUE)
df <- df[,-1]
df[,1] <- as.Date(df[,1])

sname <- names(df)[-1]
nfolder <- paste0("ts-chgpt-elbow-plots-", Sys.Date())
if(!file.exists(nfolder)){dir.create(nfolder)}
setwd(paste(dir,nfolder, sep="\\"))

edf2 <- data.frame(matrix(ncol = length(sname), nrow = 10000), stringsAsFactors = FALSE)
colnames(edf2) <- sname

for (i in 1:length(sname)){
        df2.i <- df[, c(1, 1+i)]
        df2.i$label <- factor(rep("ts", by=length(df2.i[,1])))
        
        #Helper function
        indexer <- function(){
                ev <- vector(mode="numeric", length=length(cpts))
                for(j in 1:length(cpts)){
                        ev[j] <- as.numeric(df2.i[cpts[j],1])
                }
                ev
        }
        site.ts <- ts(df2.i[,2], frequency=12, 
                      start=c(as.numeric(year(df2.i[1,1])),
                              as.numeric(month(df2.i[1,1]))),
                      end=c(as.numeric(year(tail(df2.i[,1], n=1))),
                            as.numeric(month(tail(df2.i[,1], n=1)))))
        # Empty vector for 1000 iterations
        elbow <- vector(mode="numeric", length=1000)
        # Binary method Q length must be <= length of ts/2
        for (j in 1:1000){
                ind <- 0+j
                mvalue <- cpt.mean(site.ts, method="BinSeg", penalty = "Manual", 
                                   pen.value = ind, Q=length(site.ts)/2)
                
                elbow[j] <- length(mvalue@cpts)
        }
        edf2[,i] <- elbow
}

edf2$penalty <- 1:1000

for (k in 1:length(sname)){
        p3 <-     ggplot(data=edf2 ,aes_string(x="penalty", y="edf2[,k]"))+
                #geom_point()+
                geom_line()+
                theme_bw()+
                #                 annotate("text", 850, 80, label = paste("est.value = ", min(edf2[,k])),
                #                          size=8, colour = "red")+
                ylab("no. of change pts")+
                xlab("penalty value")
        sname.i<-sname[k]
        filename<-paste0(sname.i, "-elbow-plot", out)
        ggsave(file=filename, p3)
        
        
}

## Run for seasonally adjusted ts


dir="Z:\\DEC\\Vegmachine_SharkBay\\Working\\Bernier_Dorre\\Working\\analysis"
csv="Bernier_Dorre_mtsd.csv"
out=".jpeg"

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


setwd(dir)
df <- read.csv(csv, header = TRUE)
df <- df[,-1]
df[,1] <- as.Date(df[,1])

sname <- names(df)[-1]
nfolder <- paste0("ts-chgpt-graphs-elbow-plots", Sys.Date())
if(!file.exists(nfolder)){dir.create(nfolder)}
setwd(paste(dir,nfolder, sep="\\"))

edf2 <- data.frame(matrix(ncol = length(sname), nrow = 10000), stringsAsFactors = FALSE)
colnames(edf2) <- sname

for (i in 1:length(sname)){
        df2.i <- df[, c(1, 1+i)]
        df2.i$label <- factor(rep("ts", by=length(df2.i[,1])))
        
        #Helper function
        indexer <- function(){
                ev <- vector(mode="numeric", length=length(cpts))
                for(j in 1:length(cpts)){
                        ev[j] <- as.numeric(df2.i[cpts[j],1])
                }
                ev
        }
        site.ts <- ts(df2.i[,2], frequency=12, 
                      start=c(as.numeric(year(df2.i[1,1])),
                              as.numeric(month(df2.i[1,1]))),
                      end=c(as.numeric(year(tail(df2.i[,1], n=1))),
                            as.numeric(month(tail(df2.i[,1], n=1)))))
        # Empty vector for 1000 iterations
        elbow <- vector(mode="numeric", length=1000)
        # Binary method Q length must be <= length of ts/2
        for (j in 1:1000){
                ind <- 0+j
                mvalue <- cpt.mean(site.ts, method="BinSeg", penalty = "Manual", 
                                   pen.value = ind, Q=length(site.ts)/2)
                
                elbow[j] <- length(mvalue@cpts)
        }
        edf2[,i] <- elbow
}

edf2$penalty <- 1:1000

for (k in 1:length(sname)){
        p3 <-     ggplot(data=edf2 ,aes_string(x="penalty", y="edf2[,k]"))+
                #geom_point()+
                geom_line()+
                theme_bw()+
                #                 annotate("text", 850, 80, label = paste("est.value = ", min(edf2[,k])),
                #                          size=8, colour = "red")+
                ylab("no. of change pts")+
                xlab("penalty value")
        sname.i<-sname[k]
        filename<-paste0(sname.i, "-elbow-plot", out)
        ggsave(file=filename, p3)
        
        
}
                