#'SegmentFunction
#'
#'This function builds a categorical variable from a numeric variable
#'@import RForcecom
#'@import dplyr
#'@export slider
#'@export data_clean
#'@export ranker
#'@export index
#'@export slider2
#'@export updater


slider <- function(var, iter) # 2 inputs; The numeric variable to convert and Number of cuts
{
  fdata <- var
  fdata$dist <- "NA"

  # For continuous numeric variables, bsed on the specified split, percentiles are used to split the data into equal parts.
  if(iter == 2){

    cutoff <- quantile(fdata[[1]], 1/2)
    fdata$dist[fdata[1] <= cutoff] <- paste0("0 to ",cutoff )
    fdata$dist[fdata[1] > cutoff & fdata[1] <= max(var) ] <- paste0(cutoff," to ", max(var))
    return(fdata)
  }else if(iter == 3){

    cutoff <- quantile(fdata[[1]], c(1/3, 2*(1/3)))
    fdata$dist[fdata[1] <= cutoff[1]] <- paste0("0 to ",cutoff[1] )
    fdata$dist[fdata[1] > cutoff[1] & fdata[1] <= cutoff[2] ] <- paste0(cutoff[1]+1," to ", (cutoff[2]))
    fdata$dist[fdata[1] > cutoff[2] & fdata[1] <= max(var) ] <- paste0(cutoff[2]+1," to ", max(var))
    return(fdata)
  } else if(iter == 4){

    cutoff <- quantile(fdata[[1]], c(1/4, 2*(1/4), 3*(1/4)))
    fdata$dist[fdata[1] <= cutoff[1]] <- paste0("0 to ",cutoff[1] )
    fdata$dist[fdata[1] > cutoff[1] & fdata[1] <= cutoff[2] ] <- paste0(cutoff[1]+1," to ", (cutoff[2]))
    fdata$dist[fdata[1] > cutoff[2] & fdata[1] <= cutoff[3] ] <- paste0(cutoff[2]+1," to ", (cutoff[3]))
    fdata$dist[fdata[1] > cutoff[3] & fdata[1] <= max(var) ] <- paste0(cutoff[3]+1," to ", max(var))
    return(fdata)
  } else if(iter == 5){

    cutoff <- quantile(fdata[[1]], c(1/5, 2*(1/5), 3*(1/5), 4*(1/5)))
    fdata$dist[fdata[1] <= cutoff[1]] <- paste0("0 to ",cutoff[1] )
    fdata$dist[fdata[1] > cutoff[1] & fdata[1] <= cutoff[2] ] <- paste0(round(cutoff[1], 0)+1," to ", round(cutoff[2],0))
    fdata$dist[fdata[1] > cutoff[2] & fdata[1] <= cutoff[3] ] <- paste0(round(cutoff[2], 0)+1," to ", round(cutoff[3],0))
    fdata$dist[fdata[1] > cutoff[3] & fdata[1] <= cutoff[4] ] <- paste0(round(cutoff[3], 0)+1," to ", round(cutoff[4],0))
    fdata$dist[fdata[1] > cutoff[4] & fdata[1] <= max(var) ] <- paste0(round(cutoff[4], 0) +1," to ", round(max(var),0))
    return(fdata)
    # Upto 5 cuts can be done

  }
}

data_clean <- function(var){

  if(is.numeric(var) == TRUE){

    if(length(unique(var)) == 2 ) # If numeric data is only 1 or 0, we convert them directly to discreet variable
    {
      var<- as.factor(var)
      return(var)

    }else{
      # If Continuous, we cut the data to 2 levels using percentile
      fdata <- data.frame(var)
      fdata$dist <- "NA"
      cutoff <- quantile(fdata[[1]], 1/2)
      fdata$dist[fdata[1] <= cutoff] <- paste0("0 to ",cutoff )
      fdata$dist[fdata[1] > cutoff & fdata[1] <= max(var) ] <- paste0(cutoff," to ", max(var))
      fdata[1] <- NULL
      var <- fdata$dist
      return(as.factor(var))
    }
  }else{
    # If categorical, we reduce the number of levels to 2
    cat_name <- names(table(var)[which.max(table(var))])
    var <- as.character(var)
    var[var != cat_name] <- "OTHER"
    var <- factor(var)
    return(var)
  }
}

ranker <- function(dataFrame)
{
  df <- dataFrame

  #Creating a table for use through dplyr
  myorder <-  tbl_df(df)

  colnames(myorder) <- c('Id', 'quant', 'qual') #Colnames generalized for ease of use
  #myorder <- na.omit(myorder)
  myorder$quant[is.na(myorder$quant)] <- 0# Numeric NAs are considered as 0s
  myorder$qual <- factor(myorder$qual)
  myorder <- myorder %>% arrange(desc(qual))

  # Summarization and grouping by independent variable
  summary <- myorder %>% group_by(qual) %>%
    summarise(Records = n(), total = sum(quant))
  summary <- summary %>% arrange(desc(total))

  if(nrow(summary) <= 10){

    summary$decile <- rank(-summary$total)
    final <- merge(myorder, summary, by = 'qual')
    final <- final %>% arrange(decile)
    return(as.data.frame(final))
  }else{

    summary$decile <- as.numeric('NA')

    # Core algorithm; setting up values and creating decile rankings
    totalcount <- nrow(summary)
    order1 <- tail(summary, totalcount %% 10)
    order1$decile <- 11 # Anything not belonging to specified parameters will go to 6
    order2 <- summary[-seq(nrow(summary),nrow(summary)-((totalcount %% 10)-1)),]
    totalcount <- nrow(order2)

    order2$decile <- rep(1:10, each = totalcount/10) #Create Declies

    #Binding the two split tables
    order <- rbind(order2,order1)

    final <- merge(myorder, order, by = 'qual')
    final$Records <- NULL
    final$total <- NULL
    final <- final %>% arrange(decile)
    return(as.data.frame(final)) # Returns the data frame at record levels with decile values from 1: 11 appended
  }
}

index <- function(data1)  # Indexing algorithm; takes in one dataframe with 2 variables, variable 1 is numeric (Dependent)
  # and variable 2 is categorical (Independent)
{
  names(data1) <- c("Numeric","Categorical")

  index1 <- data1 %>% group_by(Categorical) %>%
    summarise(avg = mean(Numeric), count = n(), sum = sum(Numeric)) # The algorithm groups by categorical variable, summarizing by mean, count and sum of numeric variable

  value1 <- sum(index1$sum)/sum(index1$count) #Index is calculated from the summary
  index1$index <- (index1$avg/value1)*100
  return(index1) # The function returns an indexed table with index values.
}

slider2 <- function(var, iter) # 2 inputs; The numeric variable to convert and Number of cuts
{
  if(mean(var) > 100000){
    var <- round((var/100000), -1)    # conditional statement if the mean of the variable is more than 100,000, round it off for ease of use.
  }

  fdata <- data.frame(var)
  fdata$dist <- "NA"

  # For continuous numeric variables, bsed on the specified split, percentiles are used to split the data into equal parts.
  if(iter == 2){

    cutoff <- quantile(fdata[[1]], 1/2)
    fdata$dist[fdata[1] <= cutoff] <- paste0("0 to ",cutoff )
    fdata$dist[fdata[1] > cutoff & fdata[1] <= max(var) ] <- paste0(cutoff," to ", max(var))
    return(fdata)
  }else if(iter == 3){

    cutoff <- quantile(fdata[[1]], c(1/3, 2*(1/3)))
    fdata$dist[fdata[1] <= cutoff[1]] <- paste0("0 to ",cutoff[1] )
    fdata$dist[fdata[1] > cutoff[1] & fdata[1] <= cutoff[2] ] <- paste0(cutoff[1]+1," to ", (cutoff[2]))
    fdata$dist[fdata[1] > cutoff[2] & fdata[1] <= max(var) ] <- paste0(cutoff[2]+1," to ", max(var))
    return(fdata)
  } else if(iter == 4){

    cutoff <- quantile(fdata[[1]], c(1/4, 2*(1/4), 3*(1/4)))
    fdata$dist[fdata[1] <= cutoff[1]] <- paste0("0 to ",cutoff[1] )
    fdata$dist[fdata[1] > cutoff[1] & fdata[1] <= cutoff[2] ] <- paste0(cutoff[1]+1," to ", (cutoff[2]))
    fdata$dist[fdata[1] > cutoff[2] & fdata[1] <= cutoff[3] ] <- paste0(cutoff[2]+1," to ", (cutoff[3]))
    fdata$dist[fdata[1] > cutoff[3] & fdata[1] <= max(var) ] <- paste0(cutoff[3]+1," to ", max(var))
    return(fdata)
  } else if(iter == 5){

    cutoff <- quantile(fdata[[1]], c(1/5, 2*(1/5), 3*(1/5), 4*(1/5)))
    fdata$dist[fdata[1] <= cutoff[1]] <- paste0("0 to ",cutoff[1] )
    fdata$dist[fdata[1] > cutoff[1] & fdata[1] <= cutoff[2] ] <- paste0(cutoff[1]+1," to ", (cutoff[2]))
    fdata$dist[fdata[1] > cutoff[2] & fdata[1] <= cutoff[3] ] <- paste0(cutoff[2]+1," to ", (cutoff[3]))
    fdata$dist[fdata[1] > cutoff[3] & fdata[1] <= cutoff[4] ] <- paste0(cutoff[3]+1," to ", (cutoff[4]))
    fdata$dist[fdata[1] > cutoff[4] & fdata[1] <= max(var) ] <- paste0(cutoff[4]+1," to ", max(var))
    return(fdata)
    # Upto 5 cuts can be done
  }
}

updater <- function(access_token, instance_url, myobject, data){

  instance_u <- paste0(instance_url,'/')
  api <- '36.0'
  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)

  update_job <- rforcecom.createBulkJob(session,
                                        operation ='update', object = myobject) # Create a new bulkjob for updating Salesforce
  my_data <- data # Dataframe to be uploaded
  batches_info <- rforcecom.createBulkBatch(session,
                                            jobId = update_job$id, data = my_data) #Update job
  close_job_info <- rforcecom.closeBulkJob(session, jobId=update_job$id)

}

