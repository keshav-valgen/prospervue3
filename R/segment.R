#'Segment
#'
#'This function builds a categorical variable from a numeric variable
#'@import RForcecom
#'@import dplyr
#'@export segment


segment <- function(access_token, instance_url, object, field, newname){


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
data1 <- cbind(data1, data2) # Derived values are binded to the original data
data1 <- subset(data1, select = c("Id", "dist"))
colnames(data1) <- c("strId", newname)


# update_job <- rforcecom.createBulkJob(session,
#                                       operation ='update', object = myobject) # Create a new bulkjob for updating Salesforce
# my_data <- data.frame(id = data1$strId, v2 = data1$dist) # Dataframe to be uploaded
# colnames(my_data) <- c("id", newname)
#
# batches_info <- rforcecom.createBulkBatch(session,
#                                           jobId = update_job$id, data = my_data) #Update job
# close_job_info <- rforcecom.closeBulkJob(session, jobId=update_job$id)

return(data1)
}

