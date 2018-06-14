
# HEAT function

# The user specifies the names of the columns in the input dataset which contain the 
# important assessment information

# varArea (optional, default="Area"): Groups the assessment by Sub-division/basin/water-body/station
# varCriteria (required, default="Criteria"): Criteria to which an indicator belongs (e.g.C1,C2,C3)
# varIndicator (required, default="Indicator"): The name of the indicator
# varResponse (required, default="Response"): +ve or -ve
# varTarget (required, default="Target"): Target value for the indicator
# varStatus (required,default="Status"): Status value for the indicator
# summarylevel (required, default=1): Determines the level at which results are aggregated
#                                     1 Results by indicator
#                                     2 Results by Area and Criteria / Quality Element in a "long" table
#                                     3 Results by Area and Criteria in a "wide" table
#                                     4 Summary Results by Area


#===============================================================================
# function HEAT
HEAT<- function(assessmentdata,varArea="Area",varCriteria="Criteria",
                varIndicator="Indicator",varResponse="Response",
                varTarget="Target",varStatus="Status",
                summarylevel=1){
  
  
  requiredcols <- c(varCriteria,varIndicator,varResponse,varTarget,varStatus)
  extracols <- c(varArea)
  
  #Check column names in the imported data
  cnames<-names(assessmentdata)
  nimp = ncol(assessmentdata)
  nreq = length(requiredcols)
  nextra = length(extracols)
  
  ok <- rep(0, nreq)
  okextra <- rep(0, nextra)
  foundresponse=FALSE
  
  for (i in 1:nimp){
    for (j in 1:nreq){
      if(toupper(requiredcols[j])==toupper(cnames[i])){
        names(assessmentdata)[i] <- requiredcols[j]
        ok[j]=1
      }
    }
    for (j in 1:nextra){
      if(toupper(extracols[j])==toupper(cnames[i])){
        names(assessmentdata)[i] <- extracols[j]
        okextra[j]=1
      }
    }
  }
  
  for(j in 1:nextra){
    if(okextra[j]==0){
      assessmentdata[[extracols[j]]]<-1
    }
  }
  
  n<-sum(ok, na.rm = TRUE)
  
  if(n<nreq){
    # The required columns were not found in the input data
    message("Error in HEAT Assessment. Required column(s) were not found in the input data:")
    for (j in 1:nreq){
      if(ok[j]!=1){
        message(paste("    ",requiredcols[j]))
      }
    }
    return(NA)
  }else{
    # The required columns are present - OK to do the assessment
    
    # Change order of matrices factors
    mat1<-data.frame(unique(assessmentdata[,varCriteria]))
    names(mat1)[1] <- varCriteria
    mat1$char<-as.character(mat1[,varCriteria])
    mat1$len<-nchar(mat1$char)
    mat1<-arrange(mat1,len)
    
    assessmentdata[,varCriteria] <- factor(assessmentdata[,varCriteria], levels = mat1$char)
    
    # All combinations of matrices and waterbodies
    # This is used to ensure that a NA is returned where the combinations are missing
    areas<-unique(assessmentdata[,varArea])
    criteria<-unique(assessmentdata[,varCriteria])
    criteria<-expand.grid(areas, criteria)
    names(criteria)[1] <- varArea
    names(criteria)[2] <- varCriteria
    
    assessmentdata$Response1<-ifelse(substr(assessmentdata[,varResponse],1,1)=="-",-1,1)
    assessmentdata$EUT_Ratio<-ifelse(assessmentdata$Response1==1,
                                     assessmentdata[,varStatus]/assessmentdata[,varTarget],
                                     assessmentdata[,varTarget]/assessmentdata[,varStatus])
    
    # QEdata - Calculate the Eutrophication Sum (EUT_SUM) by Quality Element (Criteria)
    QEdata<-ddply(assessmentdata,(as.quoted(c(varArea,varCriteria))),  summarise,EUT_Sum=mean(EUT_Ratio))
    QEspr<-spread_(data=QEdata, key_col=varCriteria , value_col = "EUT_Sum")
    QEdata$QEStatus<-HEATStatus(QEdata$EUT_Sum)
    
    QEdata<-left_join(criteria,QEdata,c(varArea,varCriteria))
    QEdata<-arrange_(QEdata,varArea,varCriteria)
    
    HEATresult<-ddply(QEdata,(as.quoted(c(varArea))), summarise,EUT_Sum=max(EUT_Sum, na.rm = TRUE))
    HEATresult[,varArea]<-NULL
    HEATQE<-inner_join(QEdata, HEATresult, 'EUT_Sum')
    names(HEATQE)[names(HEATQE) == varCriteria] <- "Worst"
    names(HEATQE)[names(HEATQE) == "QEStatus"] <- "Status"
    assessmentdata<-left_join(assessmentdata,QEdata,c(varArea,varCriteria))
    QEspr<-inner_join(QEspr, HEATQE, varArea)
    
    #return results depending on summary level selected
    
    if(summarylevel==2){
      return(QEspr)
    }else if(summarylevel==3){
      return(QEdata)
    }else if(summarylevel==4){
      return(HEATQE)
    }else{
      return(assessmentdata)
    }  
  }
}

#===============================================================================
#Function HEATStatus

# Determines the status classification based on Eutrophication Sum
HEATStatus<-function(EUT_sum){
  status<-ifelse(EUT_sum>0.5, "Good", "High")
  status<-ifelse(EUT_sum>1, "Moderate", status)
  status<-ifelse(EUT_sum>1.5, "Poor", status)
  status<-ifelse(EUT_sum>2, "Bad",status )
  return(status)
}
