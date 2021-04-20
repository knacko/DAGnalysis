tidy_dagitty(dag)

exposure <- exp <- "alcohol.usage"
outcome <- out <- "cancer.glioma"
df <- AGOGdata

DAG <- dagitty(paste("dag {", DAGdefault, "}", sep = ""))

fit <- calculate(DAG,df,exp,out)

summary(fit[[1]])
tidy(fit[[1]], exponentiate =  TRUE, conf.int = TRUE)%>% 
  mutate_if(is.numeric, round, digits =4)

conf <- unlist(adjustmentSets(DAG, "drug.statin", "cancer.glioma", effect="total" ), use.names=FALSE)

calculate <- function (dag, df, exposure, outcome) {
  
  adjSets <- adjustmentSets(dag, exposure, outcome, effect="total" )
  
  fitSets <- c()
  
  #adjSet <- adjSets[1]
  
  for (adjSet in adjSets) {
    
    adjSet %<>% unlist(use.names=FALSE)
    
    confounding <- if (length(adjSet) == 0) "" else paste(" +",paste(adjSet,collapse=" + "))
    
    formula <- paste(paste(outcome,"~",exposure,sep = " "),confounding,sep="")
    
    print(formula)
    
    formula %<>% as.formula()
    
    fit <- glm(formula, data = df, family = binomial(link=logit))
    
    fitSets %<>% append(list(fit))
    
  }
  
  return(fitSets)
}

### Model (All) ###################################

tryCatch ({
  
  err = FALSE
  
  #DAG <- read_file()
  
  Model.crude <- AGOG.model(AGOG.dataset)
  Model.adjusted <- AGOG.model(AGOG.dataset, confounders=c("gender","age","ethnicity","state"))
  Model.DAG <- AGOG.model.dags(DAG, AGOG.dataset, confounders=c("gender","age","ethnicity","state"))
  Model.DAG$Confounders <- str_to_title(Model.DAG$Confounders, locale = "en")
  
  ## Create a blank workbook
  wb <- createWorkbook()
  
  addWorksheet(wb, "Crude")
  addWorksheet(wb, "Adjusted")
  addWorksheet(wb, "DAG")
  
  addWorksheet(wb, "Significant Crude")
  addWorksheet(wb, "Significant Adjusted")
  addWorksheet(wb, "Significant DAG")
  
  writeData(wb, "Crude", Model.crude)
  writeData(wb, "Adjusted", Model.adjusted)
  writeData(wb, "DAG", Model.DAG)
  
  writeData(wb, "Significant Crude", filter(Model.crude,Sigificance != " "))
  writeData(wb, "Significant Adjusted", filter(Model.adjusted,Sigificance != " "))
  writeData(wb, "Significant DAG", filter(Model.DAG,Sigificance != " "))
  
  ## Save workbook to working directory
  date <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  
  identifier <- ""
  
  path <- paste0("D:/Documents/School/Internships/CBDRH/Data/Data from CBDRH/Generated/",date,identifier,"/")
  
  },
  error=function(cond) {
    message(cond)
    err <- TRUE
})
  
if(!err) {  
  dir.create(path)
  saveWorkbook(wb, file = paste0(path,date,"_AGOG_OR.xlsx"), overwrite = TRUE)
  save.image(file=paste0(path,date,"_R_image.rda"))
  write.xlsx(AGOG.dataset,paste0(path,date,"_AGOG_dataset.xlsx"))
  write.xlsx(AGOG.raw,paste0(path,date,"_AGOG_raw.xlsx"))
  write(DAG, file = paste0(path,date,"_AGOG_DAG.txt"))
  
  rm(wb,path,date)
}

# Extract the p-value from the summary(lm)
summary.return.pval <- function( object, ... )
{
  smod <- summary( object$glm_res )
  csmod <- smod$coefficients
  csmod[,"Std. Error"] <- sqrt( diag( vcov(object) ))
  csmod[,"z value"] <-  csmod[,"Estimate"] / csmod[,"Std. Error"]
  csmod[,"Pr(>|z|)"] <- stats::pnorm( - abs( csmod[,"z value"] ) )*2
  return(csmod[,4])
}

### Model (DAG) ###################################

AGOG.model.dags <- function(DAG,data,exposure=NULL,confounders=NULL) {
  
  if(is.null(exposure)) exposure <- names(data[[1]])
  exposure <- exposure[!exposure %in% c("cec_upn", "ufn_primary","cancer.glioma",confounders)]
  
  exposure.variables <- data.frame(Variable=character(), OR=numeric(), CI2.5=numeric(), CI97.5=numeric(), "P.value"=numeric(),"Sigificance"=character(),"Confounders"=character())
  
  for(i in 1:length(exposure)) { 
    
    adjSets <- adjustmentSets(DAG, exposure[i], "cancer.glioma", effect="total" )
    
    for(j in 1:length(adjSets)) { 
      
      exposure.variables %<>% rbind(
        AGOG.model(data,exposure[i],
                   x <- union(unlist(adjSets[j], use.names=FALSE),confounders)))
      
    }
  }
  
  exposure.variables$Confounders %<>% strsplit(", ") %>% lapply(function(x) paste(x[!x %in% confounders], collapse=", ")) %>% unlist()
  
  return(exposure.variables)
  
}

### Model (Crude and Adjusted) ##################################

AGOG.model <- function(data,exposure=NULL,confounders=NULL) {
  
  if(is.null(exposure)) exposure <- names(data[[1]])
  
  exposure <- exposure[!exposure %in% c("cec_upn", "ufn_primary","cancer.glioma",confounders)]
  
  exposure.variables <- data.frame(Variable=character(), OR=numeric(), CI2.5=numeric(), CI97.5=numeric(), "P.value"=numeric(),"Sigificance"=character(),"Confounders"=character())
  
  confounders.s <- if (length(confounders) == 0) "" else paste("+",paste(confounders,collapse=" + "))

  model <- lapply(exposure, function(i) {

    form <- as.formula(paste("cancer.glioma ~",i,confounders.s))
    
    mod <- lapply(data, function(d){
      glm.cluster(data=d, formula=form, family=binomial, cluster="ufn_primary")
    })

    cmod <- mitools::MIextract( mod, fun=coef)
    semod <- lapply( mod, FUN=function(mm){
      smm <- summary(mm[[1]])
      smm$coefficients[,"Std. Error"]
    } )
    
    modpool <- miceadds::pool_mi( qhat=cmod, se=semod )
    modpool
  })
  
  for(i in 1:length(model)) { 
    
    glm <- model[[i]]
    
    capture.var <- 2:max(2,(nlevels(data[[1]][,exposure[i]])))
    
    exposure.variable <- cbind(OR = exp(coef(glm)), exp(confint(glm)),
                               "P.value" = summary(glm)$p)[capture.var,]
    
    if (length(capture.var)==1) exposure.variable %<>% as.list()
    
    exposure.variable <- cbind(data.frame("Variable"=names(coef(glm))[capture.var]),data.frame(exposure.variable))
    exposure.variable %<>% rename("CI2.5" = "X2.5..", "CI97.5" = "X97.5..")
    exposure.variable$Sigificance <- stars.pval(exposure.variable$P.value)
    exposure.variable$Confounders <- paste(confounders, collapse=", ")
    
    exposure.variables %<>% rbind(exposure.variable)
    
  }
  
  return (exposure.variables)
  
}

#### Old stuff

logistic.display(glm1)



