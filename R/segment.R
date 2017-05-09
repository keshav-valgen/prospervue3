#'Segment
#'
#'This function builds a categorical variable from a numeric variable
#'@import RForcecom
#'@import dplyr
#'@export segment


segment <- function(access_token, instance_url, object, field){


instance_u <- paste0(instance_url,'/')
api <- '36.0'
myobject <- object
myquery <- paste0('Select Id, ', field,' FROM ', myobject)
session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)

data1 <- rforcecom.bulkQuery(session, myquery, myobject)
data1 <- na.omit(data1)

# Data Treatment starts Here
data2 <- subset(data1, select = c(2))
data2 <- slider(data2, 5)
data3 <- data.frame(cat  = unique(datas2$dist))
data3$cat <- as.character(data3$cat)

data3$min <- parse_number(data3$cat)
data3$max <- stri_extract_last_words(data3$cat)
return(data3)

}

