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
sig=0.002
out=".pdf"
survey="1998-09-01"
project="TEC"



ecp_chgpt <- function(dir, csv, sig, survey, out, project){
        
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
                          "ecp"))
                
        ##Generic tasks
        setwd(dir)
        df <- read.csv(csv, header = TRUE)
        df_name <- read.csv(csv, header = TRUE, check.names = FALSE)
        df <- df[,-1]
        df[,1] <- as.Date(df[,1])
        
        sname <- names(df)[-1]
        titles <- names(df_name)[c(-1,-2)]
        folder <- paste0(project, "-ts-ECP-chgpt-graphs-", Sys.Date())
        if(!file.exists(folder)){ dir.create(folder)}
        setwd(paste(dir,folder, sep="\\"))

        for (i in 1:length(sname)){
                
                df2.i <- df[, c(1, 1+i)]
                df2.i$label <- factor(rep("ts", by=length(df2.i[,1])))
                
                ecp.mat <- matrix(df2.i[,2], ncol = 1)
                ecp.out <- e.divisive(ecp.mat, R = 499, sig.lvl = sig, alpha = 1 )
                ecp.est <- ecp.out$estimates[c(-1, -length(ecp.out$estimates))]#drop first and last records
 
                #Helper function to index dates
                indexer <- function(){
                        ev <- vector(mode="numeric", length=length(ecp.est))
                        for(j in 1:length(ecp.est)){
                                ev[j] <- as.numeric(df2.i[ecp.est[j],1])
                        }
                        ev
                }
                
                
                #dates of chgpts
                ecpdf <- data.frame(x=indexer(), label=rep("Break Points", length(indexer())))
                date <- as.character(as.Date(indexer()))
                
                
                #df for vertical line handling
                vertdf <- data.frame(x=as.Date(survey), y=c(-Inf, Inf),
                                     Survey=factor(year(as.Date(survey))))
                
                p <- ggplot()+
                        geom_point(data=df2.i, aes_string(x="date", y=sname[i]), colour="black")+
                        geom_line(data=df2.i, aes_string(x="date", y=sname[i]), colour="black")+
                        geom_vline(data=ecpdf, aes(xintercept=x, linetype="Break \nPoints"),
                                   colour = "red", show_guide=TRUE)+
                        geom_line(aes(x,y), colour='blue', linetype=4, size = 0.5, vertdf)+
                        coord_cartesian(ylim = c(-10, 110))+
                        theme_bw()+
                        xlab("")+
                        ylab("Vegetation Cover %")+
                        ggtitle(paste0("Site ", titles[i]))+
                        theme(legend.title = element_blank(),
                              axis.title.y = element_text(size=15),
                              axis.text.y = element_text(angle=90, size=15),
                              axis.text.x = element_text(size=15),
                              legend.text = element_text(size = 10))
                
                
                titles.i<-titles[i]
                filename<-paste0(titles.i, "-ECP-chgpt-plot",out)

                ggsave(file=filename, p, width = 22.5, height = 13.5, units = "cm")
        }
}

ecp_chgpt(dir, csv, sig, survey, out, project)
