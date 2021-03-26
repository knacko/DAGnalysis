options(scipen=999)
library(dagitty)
library(miceadds)
library(broom)
library(dplyr)
library(magrittr)
library(sandwich)
library(gtools)
library(openxlsx)
library(tidyverse)

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
  
  identifier <- "fixed dag"
  
  path <- paste0("D:/Documents/School/Internships/CBDRH/Data/Data from CBDRH/Generated/",date," - ",identifier,"/")
  
  dir.create(path)
  
  saveWorkbook(wb, file = paste0(path,date,"_AGOG_OR.xlsx"), overwrite = TRUE)
  save.image(file=paste0(path,date,"_R_image.rda"))
  write.xlsx(AGOG.dataset,paste0(path,date,"_AGOG_dataset.xlsx"))
  write.xlsx(AGOG.raw,paste0(path,date,"_AGOG_raw.xlsx"))
  write(DAG, file = paste0(path,date,"_AGOG_DAG.txt"))
          
  rm(wb)
  rm(i) 
},
error=function(cond) {
  message(cond)
})




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
  
  if(is.null(exposure)) exposure <- names(data)
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
  
  if(is.null(exposure)) exposure <- names(data)
  
  exposure <- exposure[!exposure %in% c("cec_upn", "ufn_primary","cancer.glioma",confounders)]
  
  exposure.variables <- data.frame(Variable=character(), OR=numeric(), CI2.5=numeric(), CI97.5=numeric(), "P.value"=numeric(),"Sigificance"=character(),"Confounders"=character())
  
  model <- vector(mode = "list", length = length(exposure))
  confounders.s <- if (length(confounders) == 0) "" else paste(" +",paste(confounders,collapse=" + "))
  
  model <- lapply(exposure, function(i) {
      glm.cluster(as.formula(paste("cancer.glioma ~ ",i,confounders.s)),
      data = data,
      family=binomial,
      cluster="ufn_primary")}
  )
  
  for(i in 1:length(model)) { 
    
    glm <- model[[i]]
    
    capture.var <- 2:max(2,(nlevels(data[,exposure[i]])))
    
    exposure.variable <- cbind(OR = exp(coef(glm)), exp(confint(glm)),
                               "P.value" = summary.return.pval(glm))[capture.var,]
       
    if (length(capture.var)==1) exposure.variable %<>% as.list()
    
    exposure.variable <- cbind(data.frame("Variable"=names(coef(glm))[capture.var]),data.frame(exposure.variable))
    exposure.variable %<>% rename("CI2.5" = "X2.5..", "CI97.5" = "X97.5..")
    exposure.variable$Sigificance <- stars.pval(exposure.variable$P.value)
    exposure.variable$Confounders <- paste(confounders, collapse=", ")
    
    exposure.variables %<>% rbind(exposure.variable)
    
  }
  
  return (exposure.variables)
  
}

##############


AGOG.adjusted.data <- AGOG.data

cols <- names(AGOG.adjusted.data)
cols <- cols[! cols %in% c("cec_upn", "ufn_primary","cancer.glioma")]   #,"age","gender"

exposure.variables <- data.frame(Variable=character(), OR=numeric(), CI2.5=numeric(), CI97.5=numeric(), "P.value"=numeric())

AGOG.adjusted.model <- vector(mode = "list", length = length(cols))

AGOG.adjusted.confounders <- c() #"age","gender"
AGOG.adjusted.confounders.s <- if (length(AGOG.adjusted.confounders) == 0) "" else paste(" +",paste(AGOG.adjusted.confounders,collapse=" + "))

AGOG.adjusted.model <- lapply(cols, 
         function(i) {
           glm.cluster(as.formula(paste("cancer.glioma ~ ",i,AGOG.adjusted.confounders.s)),
                      data = AGOG.adjusted.data,
                      family=binomial,
                      cluster="ufn_primary")}
         )

for(i in 1:length(AGOG.adjusted.model)) { 

  AGOG.glm <- AGOG.adjusted.model[[i]]
  
  capture.var <- 2:(length(coef(AGOG.glm))-length(AGOG.adjusted.confounders)) 
  
  if (length(capture.var)==1) {
    exposure.variable <- data.frame(as.list(cbind(OR = exp(coef(AGOG.glm)), exp(confint(AGOG.glm)),
                                                  "P.value" = summary.return.pval(AGOG.glm))[capture.var,]))
  } else {
    exposure.variable <- data.frame(cbind(OR = exp(coef(AGOG.glm)), exp(confint(AGOG.glm)),
                                                  "P.value" = summary.return.pval(AGOG.glm))[capture.var,])
  }
  
  exposure.variable <- cbind(data.frame("Variable"=names(coef(AGOG.glm))[capture.var]),exposure.variable)
  exposure.variable %<>% rename("CI2.5" = "X2.5..", "CI97.5" = "X97.5..")
  
  exposure.variables %<>% rbind(exposure.variable)
  
}

summary(AGOG.adjusted.model)

AGOG.age <- AGOG.crude.model[1]

val <- coef(AGOG.glm)



i <-"fart"

val <- cbind(OR = exp(coef(AGOG.glm)), exp(confint(AGOG.glm)),
             "P.value" = summary.return.pval(AGOG.glm))[2:(length(coef(AGOG.glm))-2),]

val2 <- data.frame(val)



ggplot(AGOG.crude.data, aes(x=cancer.glioma, y=vice.alcohol)) + geom_boxplot() 



lm1 <- lm.cluster(as.formula("cancer.glioma ~ vice.alcohol + age + gender"),
           data = AGOG.crude.data,
           cluster="ufn_primary")
coef(lm1)
vcov(lm1)





rbind(exposure.variables,exposure.variable)


names(exposure.variable)


summary.return.pval(AGOG.glm)






library(epiDisplay)
glm1 <- glm(cancer.glioma~education+age+gender, 
            family=binomial, data=AGOG.adjusted.data)
logistic.display(glm1)





object <- AGOG.glm

### Functions ######################




