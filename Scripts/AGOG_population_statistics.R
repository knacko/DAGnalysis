AGOG.cases <- filter(AGOG.imputed, cancer.glioma==TRUE)
AGOG.controls <- filter(AGOG.imputed, cancer.glioma==FALSE) 

AGOG.stats <- data.frame(stat=character(),cases=character(),control=character())
  
### Participants
AGOG.stats %<>% addheaderrow("Demographics") %>%
  addrow("Participants (n)",paste(nrow(AGOG.cases)), paste(nrow(AGOG.controls)))
  
#### Age  
AGOG.stats %<>% add.casecontrol.mean.sd("age","Age (years), mean (SD)")
  
### Gender
AGOG.stats %<>% add.casecontrol.n.perc("gender","Men, n (%)",1)


### Ethnicity
AGOG.stats %<>% add.casecontrol.n.perc("ethnicity","White European origin, n (%)",TRUE)

### Education
AGOG.stats %<>% addheaderrow("Education level, n (%)") %>%
  add.casecontrol.n.perc("education","High school or below",1) %>%
  add.casecontrol.n.perc("education","Bachelor degree or vocational school",2) %>%
  add.casecontrol.n.perc("education","Advanced degree",3)

### Income
AGOG.stats %<>% addheaderrow("Yearly Income, n (%)") %>%
  add.casecontrol.n.perc("income","0 - $50,000",1) %>%
  add.casecontrol.n.perc("income","$50,000 - $100,000",2) %>%
  add.casecontrol.n.perc("income","$100,000 - $150,000",3) %>%
  add.casecontrol.n.perc("income","> $150,000",4)

### BMI
AGOG.stats %<>% addheaderrow("Body mass index (BMI), n (%)") %>%
  add.casecontrol.n.perc("body.size","Underweight (> 18.5)",1) %>%
  add.casecontrol.n.perc("body.size","Ideal (18.5 - 24.9)",2) %>%
  add.casecontrol.n.perc("body.size","Overweight (25 - 29.9)",3) %>%
  add.casecontrol.n.perc("body.size","Obese (> 30)",4)


### Physical activity
AGOG.stats %<>% 
  addheaderrow("Lifetime Physical activity (MET-h/wk), n (%)") %>%
  add.casecontrol.n.perc("physical.activity","Low (< 24)",1) %>%
  add.casecontrol.n.perc("physical.activity","Moderate (24 - 47)",2) %>%
  add.casecontrol.n.perc("physical.activity","High (> 47)",3)

### Screen time
AGOG.stats %<>% add.casecontrol.mean.sd("mental.activity","Mental activity (h/wk), mean (SD)")

### Alcohol
AGOG.stats %<>% 
  addheaderrow("Alcohol usage (AU standard drinks/day), n (%)") %>%
  add.casecontrol.n.perc("vice.alcohol","0 (Abstain)",1) %>%
  add.casecontrol.n.perc("vice.alcohol","< 1",2) %>%
  add.casecontrol.n.perc("vice.alcohol","1 - 2",3) %>%
  add.casecontrol.n.perc("vice.alcohol","> 2",4)

### Cigarette
AGOG.stats %<>% 
  addheaderrow("Cigarette usage status, n (%)") %>%
  add.casecontrol.n.perc("vice.cigarette","Never",0) %>%
  add.casecontrol.n.perc("vice.cigarette","Former (> 1 ypd)",1) %>%
  add.casecontrol.n.perc("vice.cigarette","Current",2)

### Cannabis
AGOG.stats %<>% add.casecontrol.n.perc("vice.cannabis","Cannabis usage, n (%)",TRUE)

### Caffeine
AGOG.stats %<>% add.casecontrol.mean.sd("vice.caffeine","Caffeine intake (mg/d), mean (SD)")

### Medications
AGOG.stats %<>% addheaderrow("Usage of medications (1 ypd), n (%)") %>%
  add.casecontrol.n.perc("drug.aspirin","Aspirin (ASA)/Paracetamol (APAP) or similar",TRUE) %>%
  add.casecontrol.n.perc("drug.nsaid","Nonsteroidal anti-inflammatory drugs (NSAIDs)",TRUE) %>%
  add.casecontrol.n.perc("drug.steroid","Anti-inflammatory steroids",TRUE) %>%
  add.casecontrol.n.perc("drug.statin","Cholesterol lowering medications (statins)",TRUE)

### Allergies
AGOG.stats %<>% add.casecontrol.n.perc("allergies","Allergies, n (%)",TRUE)

### Migraines
AGOG.stats %<>% add.casecontrol.n.perc("migraines","Migraines, n (%)",TRUE)

### Non-glioma cancer 
AGOG.stats %<>% add.casecontrol.n.perc("cancer.other","Previous non-glioma cancer incidence, n (%)",TRUE)

### Non-cancer disease
AGOG.stats %<>% add.casecontrol.n.perc("noncancer.disease","Previous cancer-associated disease incidence, n (%)",TRUE)

### Formatting functions ###############################################
add.row <- function(df,stat,cases,controls) {
  
  temp <- data.frame(stat,cases,controls)
  names(temp) <- names(df)
  
  return(rbind(df,temp))
    
}

add.header.row <- function(df,header) {

  return(addrow(df,header,"",""))
  
}


add.casecontrol.mean.sd <- function(df,col,str,val.dig=1,sd.dig=1) {
  
  case <- format.mean.sd(AGOG.cases[, col],val.dig,sd.dig)
  control <- format.mean.sd(AGOG.controls[,col],val.dig,sd.dig)

  return(add.row(df,str,case,control))
  
}

format.mean.sd <- function(col,val.dig=1,sd.dig=1) {
  
  return(format.stat(mean(col),sd(col),val.dig,sd.dig))
  
}


format.median.iqr <- function(col,val.dig=1,iqr.dig=1) {
  
  return(format.stat(median(col),iqr(col),val.dig,iqr.dig))
  
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





