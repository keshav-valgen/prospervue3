#'Industry ranking
#'
#'This function ranks a categorical variable from a numeric variable
#'@import RForcecom
#'@import dplyr
#'@export ranks

ranks <- function(access_token, instance_url, object, depfield, indfield, newname){

  instance_u <- paste0(instance_url,'/')
  api <- '36.0'
  myquery <- paste0('Select Id, ', depfield,', ',indfield,' FROM ', object)
  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)
  data1 <- rforcecom.bulkQuery(session, myquery, object)
  data1 <- na.omit(data1)
  data1 <- ranker(data1)
  data1 <- subset(data1, select = c("qual", "decile"))
  data1 <- data1 %>% group_by(qual, decile)%>% summarise(count = n())
  data1$count <- NULL
  data1$dist[data1$dist == 1 | data1$dist == 2] <- 1
  data1$dist[data1$dist == 3 | data1$dist == 4] <- 2
  data1$dist[data1$dist == 5 | data1$dist == 6] <- 3
  data1$dist[data1$dist == 7 | data1$dist == 8] <- 4
  data1$dist[data1$dist == 9 | data1$dist == 10] <- 5
  data1$dist[data1$dist == 11] <- 6
  return(data1)

}
