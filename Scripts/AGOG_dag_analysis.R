### Model (All) ###################################
# 
# tryCatch ({
#   
#    err = FALSE
#   
#   DAG <- import_dag("D:/Documents/School/Internships/CBDRH/DAGs/currentDag.txt")
#   
#   Model.crude <- AGOG.model(AGOG.dataset)
#   Model.adjusted <- AGOGmo.model(AGOG.dataset, confounders=c("Gender","Age","Ethnicity","State"))
#   Model.DAG <- AGOG.model.dags(DAG, AGOG.dataset, confounders=c("Gender","Age","Ethnicity","State"))
#   Model.DAG$Confounders <- str_to_title(Model.DAG$Confounders, locale = "en")
#   
#   ## Create a blank workbook
#   wb <- createWorkbook()
#   
#   addWorksheet(wb, "Crude")
#   addWorksheet(wb, "Adjusted")
#   addWorksheet(wb, "DAG")
#   
#   addWorksheet(wb, "Significant Crude")
#   addWorksheet(wb, "Significant Adjusted")
#   addWorksheet(wb, "Significant DAG")
#   
#   writeData(wb, "Crude", Model.crude)
#   writeData(wb, "Adjusted", Model.adjusted)
#   writeData(wb, "DAG", Model.DAG)
#   
#   writeData(wb, "Significant Crude", filter(Model.crude,Sigificance != " "))
#   writeData(wb, "Significant Adjusted", filter(Model.adjusted,Sigificance != " "))
#   writeData(wb, "Significant DAG", filter(Model.DAG,Sigificance != " "))
#   
#   ## Save workbook to working directory
#   date <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
#   
#   identifier <- ""
#   
#   path <- paste0("D:/Documents/School/Internships/CBDRH/Data/Data from CBDRH/Generated/",date,identifier,"/")
#   
# #   },
# #   error=function(cond) {
# #     message(cond)
# #     err <- TRUE
# # })
# 
# 
#   dir.create(path)
#   saveWorkbook(wb, file = paste0(path,date,"_AGOG_OR.xlsx"), overwrite = TRUE)
#   save.image(file=paste0(path,date,"_R_image.rda"))
#   write.xlsx(AGOG.dataset,paste0(path,date,"_AGOG_dataset.xlsx"))
#   write.xlsx(AGOG.raw,paste0(path,date,"_AGOG_raw.xlsx"))
#   write(DAG, file = paste0(path,date,"_AGOG_DAG.txt"))
#   
#   rm(wb,path,date)
# 


### RUN EVERYTHING BELOW THIS LINE ############################################

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
  exposure <- exposure[!exposure %in% c("cec_upn", "ufn_primary","Cancer.glioma",confounders)]
  
  exposure.variables <- data.frame(Variable=character(), OR=numeric(), CI2.5=numeric(), CI97.5=numeric(), "P.value"=numeric(),"Sigificance"=character(),"Confounders"=character())
  
  for(i in 1:length(exposure)) { 

    adjSets <- adjustmentSets(DAG, exposure[i], "Cancer.glioma", effect="total" )
    
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
  
  exposure <- exposure[!exposure %in% c("cec_upn", "ufn_primary","Cancer.glioma",confounders)]
  
  exposure.variables <- data.frame(Variable=character(), OR=numeric(), CI2.5=numeric(), CI97.5=numeric(), "P.value"=numeric(),"Sigificance"=character(),"Confounders"=character())
  
  confounders.s <- if (length(confounders) == 0) "" else paste("+",paste(confounders,collapse=" + "))

  model <- lapply(exposure, function(exp) {
    
    form <- as.formula(paste("Cancer.glioma ~",exp,confounders.s))
    
    wgt__ <- NULL
    mod <- lapply(data, function(d){
      glm.cluster(data=d, formula=form, family=binomial, cluster="ufn_primary")
    })

    cmod <- mitools::MIextract( mod, fun=coef)
    semod <- lapply( mod, FUN=function(mm){
      smm <- summary(mm[[1]])
      smm$coefficients[,"Std. Error"]
    } )
    
    modpool <- miceadds::pool_mi( qhat=cmod, se=semod)
    modpool
  })
  
  for(i in 1:length(model)) { 

    glm <- model[[i]]
    
    capture.var <- 2:max(2,(nlevels(data[[1]][,exposure[i]])))
    
   # capture.var <- 2:max(2,(length(factor(data[[1]][,exposure[i]]))))
    
    exposure.variable <- cbind(OR = exp(coef(glm)), exp(confint(glm)),
                               "P.value" = summary(glm)$p)[capture.var,]
    
    if (length(capture.var)==1) exposure.variable %<>% as.list()

    exposure.variable <- cbind(data.frame("Variable"=names(coef(glm))[capture.var]),data.frame(exposure.variable))
    exposure.variable %<>% dplyr::rename("CI2.5" = "X2.5..", "CI97.5" = "X97.5..")
    exposure.variable$Sigificance <- stars.pval(exposure.variable$P.value)
    exposure.variable$Confounders <- paste(confounders, collapse=", ")
    
    exposure.variables %<>% rbind(exposure.variable)
    
  }
  
  return (exposure.variables)
  
}


#' Imports a DAG from the Dagitty R Shiny app
#' @details The Dagitty app contains position information for each node in the graph. This must be removed before the R Dagitty object can be created.
#' @param filepath character; the file location containing the DAG 
#' @return dagitty; The object representing the DAG
import_dag <- function(filepath) {
  
  DAG = readLines(filepath)
  DAG = DAG[sapply(DAG,function(line) !grepl("^bb", line, fixed = TRUE))]
  DAG = paste(DAG,collapse="\n")
  
  
  return(dagitty(DAG))
}  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#### OLD STUFF ##############################################3
# tidy_dagitty(dag)
# 
# exposure <- exp <- "alcohol.usage"
# outcome <- out <- "cancer.glioma"
# df <- AGOGdata
# 
# DAG <- dagitty(paste("dag {", DAGdefault, "}", sep = ""))
# 
# fit <- calculate(DAG,df,exp,out)
# 
# summary(fit[[1]])
# tidy(fit[[1]], exponentiate =  TRUE, conf.int = TRUE)%>% 
#   mutate_if(is.numeric, round, digits =4)
# 
# conf <- unlist(adjustmentSets(DAG, "drug.statin", "cancer.glioma", effect="total" ), use.names=FALSE)
# 
# calculate <- function (dag, df, exposure, outcome) {
#   
#   adjSets <- adjustmentSets(dag, exposure, outcome, effect="total" )
#   
#   fitSets <- c()
#   
#   #adjSet <- adjSets[1]
#   
#   for (adjSet in adjSets) {
#     
#     adjSet %<>% unlist(use.names=FALSE)
#     
#     confounding <- if (length(adjSet) == 0) "" else paste(" +",paste(adjSet,collapse=" + "))
#     
#     formula <- paste(paste(outcome,"~",exposure,sep = " "),confounding,sep="")
#     
#     print(formula)
#     
#     formula %<>% as.formula()
#     
#     fit <- glm(formula, data = df, family = binomial(link=logit))
#     
#     fitSets %<>% append(list(fit))
#     
#   }
#   
#   return(fitSets)
# }

