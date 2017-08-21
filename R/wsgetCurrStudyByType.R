

#' Loop through the synced export data and return to a list of lists
#'
#' @param login An eloquaLogin object
#' @param currEloquaType Type Include - PageView,WebVisit,FormSubmit,FormSubmitNoRaw,EmailSend,EmailOpen,EmailClickthrough,Subscribe,Unsubscribe,Bounceback
#' @currBulkFilter Filter Used in the Study

# login <- myLogin
# currBulkFilter <- filters
# currEloquaType <- "EmailClickthrough"

wsgetCurrStudyByType <- function(login, currEloquaType, currBulkFilter){


  myExportDef <- defExport(login = myLogin, type = currEloquaType, bulkFilter = currBulkFilter,
                           exportName = "Test123 - EmailOPEN WES STUDY")
  mySync              <- syncExport(login = myLogin, exportDefinition = myExportDef) ## Sync data to the export
  myData              <- getExportData(login = myLogin, exportDefinition = myExportDef)  ## Get data from the export
  myData              <- extractExportData(myData)

  myData              <- as.data.table(myData)

  myData <- myData[,.(ActivityId,ActivityType,ActivityDate,
                      ContactId, EmailAddress,AssetId,AssetName,SubjectLine)]

  myData$ActivityDate_DATE <- substr(myData$ActivityDate,1,19)
  myData$ActivityDate_DATE <- gsub("-","/",myData$ActivityDate_DATE)

  #myData$ActivityDate_ROUND <- substr(myData$ActivityDate_DATE,1,16)
  myData$ActivityDate_DATE <- ymd_hms(myData$ActivityDate_DATE)
  myData$ActivityDate_INT <- as.numeric(myData$ActivityDate_DATE)
  myData$UniqueString <- as.numeric(paste(myData$ContactId,myData$ActivityDate_INT,sep = ""))
  myDataSINGLE <- myData[myData[, .I[which.min(UniqueString)], by=ContactId]$V1]


  studyResults_DT <- as.data.table(myDataSINGLE[,.(ContactId,EmailAddress, ActivityType, ActivityDate) ])
  studyResults_DT$ContactId <- as.numeric(studyResults_DT$ContactId)
  setkey(studyResults_DT,ContactId)

  print("COMPLETED")
  return(studyResults_DT)

}
