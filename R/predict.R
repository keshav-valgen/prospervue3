#'Predictive Profiling
#'
#'This function indexes one dependent variable from a numeric variable
#'@import RForcecom
#'@import dplyr
#'@export predictor

predictor <- function(access_token, instance_url, object, numfield, catfield, newname){

  instance_u <- paste0(instance_url,'/')
  api <- '36.0'
  myquery <- paste0('Select Id, ', numfield,', ',catfield,' FROM ', object)
  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)
  data1 <- rforcecom.bulkQuery(session, myquery, object)
  data1 <- na.omit(data1)

  categorical <- data1[,3]
  numeric <- data1[,2]

  if(is.numeric(categorical) == TRUE){

    if(length(unique(categorical)) == 2 ){

      categorical<- as.factor(categorical) # If the numeric variable is discreet; ie.. has two levels, 1 and 0, convert them to factor.

    }else{

      categorical <- slider2(categorical, 5)
      categorical[1] <- NULL
      categorical <- categorical[,1]

    }

  }else{

    categorical <- as.factor(categorical)
  }

  data1 <- data.frame(Id = data1$Id, numeric, categorical)
  indexdata <- subset(data1, select = c("numeric", "categorical"))
  indexdata <- index(indexdata)
  indexdata$Rank <- rank(-indexdata$index)
  indexdata <- subset(indexdata, select = c("Categorical", "index")) #Writing the ranks to each individual record
  data2 <- merge(data1, indexdata, by.x = "categorical", by.y = "Categorical")
  data2 <- subset(data2, select = c("Id", "index"))
  colnames(data2) <- c("Id", newname)
  updater(access_token, instance_url, object, data2)
  return(data2)
}
