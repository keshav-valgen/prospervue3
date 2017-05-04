#'
#'This function takes two variables and converts them into a quad
#'@import RForcecom
#'@import dplyr
#'@export quad
#'

quad <- function(access_token, instance_url, object, field1, field2, newname){

  instance_u <- paste0(instance_url,'/')
  api <- '36.0'
  myquery <- paste0('Select Id, ', field1,', ',field2,' FROM ', object)
  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)
  data1 <- rforcecom.bulkQuery(session, myquery, object)
  data1 <- na.omit(data1)

  var1 <- data1[,2]
  var2 <- data1[,3]
  var1 <- data_clean(var1) # New variable is created
  var2 <- data_clean(var2)

  newdata <- data.frame(Id = data1$Id, var1, var2)
  summary <- newdata %>% group_by(var1, var2) %>%
    summarise(counts = n())
  summary$Rank <- rank(-summary$counts, ties.method = "random")
  newdata <- merge(newdata, summary, all = T)
  newdata <- subset(newdata, select = c('Id', 'Rank'))
  colnames(newdata) <- c("strId", "dist")

  update_job <- rforcecom.createBulkJob(session,
                                        operation ='update', object = object) # Create a new bulkjob for updating Salesforce
  my_data <- data.frame(id = data1$strId, v2 = data1$dist) # Dataframe to be uploaded
  colnames(my_data) <- c("id", newname)

  batches_info <- rforcecom.createBulkBatch(session,
                                            jobId = update_job$id, data = my_data) #Update job

  return(paste0(nrow(data1)," records updated successfully"))
}
