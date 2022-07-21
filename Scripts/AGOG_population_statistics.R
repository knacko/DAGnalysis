#' Get population statistics for all the risk factors
#' @param AGOG.formatted The formatted data frame from any of the master scripts
#' @return dataframe with n (%) for all risk factors
getPopStats <- function(AGOG.formatted) {
  
  AGOG.cases <- filter(AGOG.formatted, Cancer.glioma==TRUE)
  AGOG.controls <- filter(AGOG.formatted, Cancer.glioma==FALSE) 
  
  AGOG.stats <- data.frame(stat=character(),cases=character(),control=character())
    
  ### Participants
  AGOG.stats %<>% add.header.row("Demographics") %>%
    add.row("Participants (n)",paste(nrow(AGOG.cases)), paste(nrow(AGOG.controls)))
    
  #### Age  
  AGOG.stats %<>% add.casecontrol.mean.sd("Age","Age (years), mean (SD)")
    
  ### Gender
  AGOG.stats %<>% add.casecontrol.n.perc("Gender","Male, n (%)",1)
  
  ### Ethnicity
  AGOG.stats %<>% add.casecontrol.n.perc("Ethnicity","White European origin, n (%)","Yes")
  
  ### Education
  AGOG.stats %<>% add.header.row("Education level, n (%)") %>%
    add.casecontrol.n.perc("Education","High school or below",1) %>%
    add.casecontrol.n.perc("Education","Bachelor degree or vocational school",2) %>%
    add.casecontrol.n.perc("Education","Advanced degree",3)
  
  ### Income
  AGOG.stats %<>% add.header.row("Yearly Income, n (%)") %>%
    add.casecontrol.n.perc("Income","0 - $50,000",1) %>%
    add.casecontrol.n.perc("Income","$50,000 - $100,000",2) %>%
    add.casecontrol.n.perc("Income","$100,000 - $150,000",3) %>%
    add.casecontrol.n.perc("Income","> $150,000",4)
  
  ### BMI
  AGOG.stats %<>% add.header.row("Body mass index (BMI), n (%)") %>%
    add.casecontrol.n.perc("BMI","Underweight (> 18.5)",1) %>%
    add.casecontrol.n.perc("BMI","Ideal (18.5 - 24.9)",2) %>%
    add.casecontrol.n.perc("BMI","Overweight (25 - 29.9)",3) %>%
    add.casecontrol.n.perc("BMI","Obese (> 30)",4)
  
  
  ### Physical activity
  AGOG.stats %<>% 
    add.header.row("Lifetime Physical activity (MET-h/wk), n (%)") %>%
    add.casecontrol.n.perc("Physical.activity","Low (< 24)",1) %>%
    add.casecontrol.n.perc("Physical.activity","Moderate (24 - 47)",2) %>%
    add.casecontrol.n.perc("Physical.activity","High (> 47)",3)
  
  ### Screen time
  AGOG.stats %<>% 
    add.header.row("Sedentary.activity (h/day), n (%)") %>%
    add.casecontrol.n.perc("Sedentary.activity","Low (< 3)",1) %>%
    add.casecontrol.n.perc("Sedentary.activity","Moderate (3 - 7)",2) %>%
    add.casecontrol.n.perc("Sedentary.activity","High (> 7)",3) 
  
  ### Cellphone usage
  AGOG.stats %<>% 
    add.header.row("Cellphone usage, n (%)") %>%
    add.casecontrol.n.perc("Cellphone","Never",0) %>%
    add.casecontrol.n.perc("Cellphone","< 10 years",1) %>%
    add.casecontrol.n.perc("Cellphone","≥ 10 years",2)
  
  ### Alcohol
  AGOG.stats %<>% 
    add.header.row("Alcohol usage (AU standard drinks/day), n (%)") %>%
    add.casecontrol.n.perc("Alcohol","0 (Abstain)",1) %>%
    add.casecontrol.n.perc("Alcohol","< 1",2) %>%
    add.casecontrol.n.perc("Alcohol","1 - 2",3) %>%
    add.casecontrol.n.perc("Alcohol","> 2",4)
  
  ### Cigarette
  AGOG.stats %<>% 
    add.header.row("Cigarette usage, n (%)") %>%
    add.casecontrol.n.perc("Cigarettes","Never",0) %>%
    add.casecontrol.n.perc("Cigarettes","Former (> 1 ypd)",1) %>%
    add.casecontrol.n.perc("Cigarettes","Current",2)
  
  ### Cannabis
  AGOG.stats %<>% 
    add.header.row("Cannabis usage, n (%)") %>%
    add.casecontrol.n.perc("Cannabis","Never",0) %>%
    add.casecontrol.n.perc("Cannabis","Former (> 1 ypd)",1) %>%
    add.casecontrol.n.perc("Cannabis","Current",2)
  
  ### Caffeine
  AGOG.stats %<>% 
    add.header.row("Caffeine intake (mg/kg/day), n (%)") %>%
    add.casecontrol.n.perc("Caffeine","Low (< 3)",1) %>%
    add.casecontrol.n.perc("Caffeine","Moderate (3 - 6.5)",2) %>%
    add.casecontrol.n.perc("Caffeine","High (> 6.5)",3)
  
  ### Medications
  AGOG.stats %<>% add.header.row("Usage of medications (1 ypd), n (%)") %>%
    add.casecontrol.n.perc("Aspirin","Aspirin (ASA)/Paracetamol (APAP) or similar","Yes") %>%
    add.casecontrol.n.perc("NSAIDs","Nonsteroidal anti-inflammatory drugs (NSAIDs)","Yes") %>%
    add.casecontrol.n.perc("Steroids","Anti-inflammatory steroids","Yes") %>%
    add.casecontrol.n.perc("Statins","Cholesterol lowering medications (statins)","Yes")
  
  
  ### Health conditions
  AGOG.stats %<>% add.header.row("Health Conditions, n (%)")
  
  ### Allergies
  AGOG.stats %<>% add.casecontrol.n.perc("Allergies","Allergies, n (%)",1)
  
  ### Migraines
  AGOG.stats %<>% add.casecontrol.n.perc("Migraines","Migraines, n (%)",1)
  
  ### Non-glioma cancer 
  AGOG.stats %<>% add.casecontrol.n.perc("Cancer.other","Prev. non-glioma cancer incidence, n (%)","Yes")
  
  ### Non-cancer disease
  AGOG.stats %<>% add.casecontrol.n.perc("Disease.other","Prev. neuro. condition incidence, n (%)","Yes")
  
  clipr::write_clip(AGOG.stats)
  
  return(AGOG.stats)

}

### Formatting functions ###############################################
add.row <- function(df,stat,cases,controls) {
  
  temp <- data.frame(stat,cases,controls)
  names(temp) <- names(df)
  
  return(rbind(df,temp))
    
}

add.header.row <- function(df,header) {

  return(add.row(df,header,"",""))
  
}


add.casecontrol.mean.sd <- function(df,col,str,val.dig=1,sd.dig=1) {
  
  case <- format.mean.sd(AGOG.cases[, col],val.dig,sd.dig)
  control <- format.mean.sd(AGOG.controls[,col],val.dig,sd.dig)

  return(add.row(df,str,case,control))
  
}

format.mean.sd <- function(col,val.dig=1,sd.dig=1) {
  
  return(format.stat(mean(col,na.rm=T),sd(col,na.rm=T),val.dig,sd.dig))
  
}


format.median.iqr <- function(col,val.dig=1,iqr.dig=1) {
  
  return(format.stat(median(col,na.rm=T),iqr(col,na.rm=T),val.dig,iqr.dig))
  
}

add.casecontrol.n.perc <- function(df,col,str,val,per.dig=1) {

  case <- format.n.perc(AGOG.cases[, col],val,per.dig)
  control <- format.n.perc(AGOG.controls[,col],val,per.dig)
  
  return(add.row(df,str,case,control))
  
}

format.n.perc <- function(col,val,perc.dig=1) {
  
  return (
    format.stat(
      length(which(col==val)),
      100*length(which(col==val))/length(col),
      val.dig = 0
    )
  )
  
}

format.stat <- function(val,sd,val.dig=1,sd.dig=1) {
  
  return(paste(
    
    formatC(round(val,val.dig), format='f', digits=val.dig),
    " (",
    formatC(round(sd,sd.dig), format='f', digits=sd.dig),
    ")",sep=""))
}





