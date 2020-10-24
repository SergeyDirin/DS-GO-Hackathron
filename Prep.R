
#reading the file
getwd()
setwd("C:\\Study\\DS\\Hackathon\\DSGO-Virtual-Oracle-Hackathon-main\\Data")

d <- read.csv("air_status.csv")
str(d)
summary(d)

#setting the factor
is.factor(d$SITE_ID)
d$SITE_ID <- as.factor(d$SITE_ID)

#head(as.POSIXct(d$DATEON, "%Y/%m/%d"))

d$DATEON <- as.POSIXct(d$DATEON, "%Y/%m/%d")

#removing comma
d$COMMENT_CODES <- gsub(","," ",d$COMMENT_CODES)

#making the code searchable
d$COMMENT_CODES <- paste(" ",d$COMMENT_CODES," ",sep="")

#paste("G",1:28,sep="")

#loop through all filter types
for (filter in c("G","N","T","W")) {
  
  #adding dummy columns
  d[,paste(filter,1:28,sep="")] <- 0
  
  #replace codes with 0 in the middle
  for (i in 1:9) {
    d$COMMENT_CODES <- gsub(
      paste(filter,"0",i,sep=""),
      paste(filter,i,sep=""),
      d$COMMENT_CODES
    )
  }
  
  #setting the values of dummy variables
  for (i in 1:28) {
    d[
      grepl( 
        paste(" ",filter,i," ", sep=""), d$COMMENT_CODES, fixed = TRUE
      ),
      paste(filter,i, sep="")
    ] <- 1
  }
  
}

#export the first version
write.csv(d,"air_status_with_dummy_vars.csv")

#removing columns with all zeros
for (filter in c("N","T","W")) {
  for (i in 1:28) {
    if ( nrow(
            d[
              d[paste0(filter,i)] == 1
            ,]
          ) == 0 ) {
      d[,paste0(filter,i)] <- NULL
    }
  }
}

#exporting new version
write.csv(d,"air_status_with_dummy_vars.csv")


install.packages("car")
library(car)
install.packages("ridge")
library (ridge)

d_backup <- d

#filtering out the NAs
d <- na.omit(d)


cor_matrix <- cor(d[,c("TSO4","TNO3","TNH4","Ca","Mg","Na","K","Cl","NSO4","NHNO3","WSO2","TOTAL_SO2","TOTAL_NO3",
              "FLOW_VOLUME","VALID_HOURS","STD2LOCAL_CF","G12","G14","G16","G18","G19","G21","G24","G25","G28","N1",
              "N2","N3","N4","T1","T2","T3","T4","T9","T10","T21","W1","W2","W3","W4","W11")])

?cor
cor_matrix

d_relevant <- d[,-which(names(d) == "COMMENT_CODES" | names(d) == "UPDATE_DATE" | names(d) == "DATEOFF"
                        | names(d) == "NHNO3" | names(d) == "TNO3")]

coefs <- lm(TOTAL_NO3 ~ ., d_relevant)
coefs

PIN414	 Pinnacles NM
SEK430	 Sequoia NP - Ash Mountain
DEV412	 Death Valley NM
SEK402	 Sequoia NP - Lookout Pt
JOT403	 Joshua Tree NP
YOS404	 Yosemite NP - Turtleback Dome
CON186	 Converse Station
LAV410	 Lassen Volcanic NP

d_backup2 <- d
d <- d_backup2




library(ggplot2)
p <- ggplot(data=d)
p + geom_line(aes(x=DATEON,y=TOTAL_NO3, color=SITE_ID), size=0.2)+
  geom_line(aes(x=DATEON,y=TSO4, color=SITE_ID), size=0.2)+
  facet_grid(SITE_ID~.)
p + geom_line(aes(x=DATEON,y=TOTAL_NO3, color=SITE_ID), size=0.2)+
  geom_line(aes(x=DATEON,y=TNH4, color=SITE_ID), size=0.2)+
  facet_grid(SITE_ID~.)
p + geom_line(aes(x=DATEON,y=TOTAL_NO3, color=SITE_ID), size=0.2)+
  geom_line(aes(x=DATEON,y=Ca, color=SITE_ID), size=0.2)+
  facet_grid(SITE_ID~.)
p + geom_line(aes(x=DATEON,y=TOTAL_NO3, color=SITE_ID), size=0.2)+
  geom_line(aes(x=DATEON,y=Mg, color=SITE_ID), size=0.2)+
  facet_grid(SITE_ID~.)
p + geom_line(aes(x=DATEON,y=TOTAL_NO3, color=SITE_ID), size=0.2)+
  geom_line(aes(x=DATEON,y=Na, color=SITE_ID), size=0.2)+
  facet_grid(SITE_ID~.)
p + geom_line(aes(x=DATEON,y=TOTAL_NO3, color=SITE_ID), size=0.2)+
  geom_line(aes(x=DATEON,y=K, color=SITE_ID), size=0.2)+
  facet_grid(SITE_ID~.)
p + geom_line(aes(x=DATEON,y=TOTAL_NO3, color=SITE_ID), size=0.2)+
  geom_line(aes(x=DATEON,y=Cl, color=SITE_ID), size=0.2)+
  facet_grid(SITE_ID~.)
p + geom_line(aes(x=DATEON,y=TOTAL_NO3, color=SITE_ID), size=0.2)+
  geom_line(aes(x=DATEON,y=NSO4, color=SITE_ID), size=0.2)+
  facet_grid(SITE_ID~.)
p + geom_line(aes(x=DATEON,y=TOTAL_NO3, color=SITE_ID), size=0.2)+
  geom_line(aes(x=DATEON,y=WSO2, color=SITE_ID), size=0.2)+
  facet_grid(SITE_ID~.)
p + geom_line(aes(x=DATEON,y=TOTAL_NO3, color=SITE_ID), size=0.2)+
  geom_line(aes(x=DATEON,y=TOTAL_SO2, color=SITE_ID), size=0.2)+
  facet_grid(SITE_ID~.)









