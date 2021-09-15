
setwd("D:\\Documents\\School\\Internships\\CBDRH\\Data\\Data from CBDRH\\Shortcuts")

files <- list.files (getwd())

#files <- files[!grepl("^.*AGOG_merged.xlsx$", files)]

files <- files[!grepl("^(_|~).*$", files)]

AGOG.raw <- NULL

for (f in files) {
  
  path = readWindowsShortcut(f)
  data <- read.xlsx(path$pathname, colNames=TRUE)
  
  print(paste("Parsing ", path$relativePath))
  
  if (is.null(AGOG.raw)) {
    AGOG.raw <- data
  } else {
    AGOG.raw %<>% merge(data,"cec_upn", all = TRUE)
  }
}

#AGOG.raw[is.na(AGOG.raw)] <- ".x"

AGOG.raw %<>% drop_na(caco_cde) #Remove unassigned cases
AGOG.raw <- AGOG.raw[-which(row_count(AGOG.raw,count=".x",append=FALSE)$rowcount > 100),] #Remove mostly unanswered
AGOG.raw <- AGOG.raw[-which(AGOG.raw$eligible == 0),] #Remove invalid

#write.xlsx(AGOG.raw,paste0(".//_",format(Sys.time(), "%Y-%m-%d_%H-%M"),"_AGOG_raw.xlsx"))

rm(f,files,data,path)

### Remove all useless columns ######################

goodcols <- DAGvars

goodcols <- c("Age", "Gender","Education", "Income","BMI", "Ethnicity",
              "Physical.activity","Sedentary.activity","Alcohol", "Cigarettes",
              "Cannabis", "Caffeine",
              "NSAIDs", "Steroids", "Statins", 
              "Allergies", "Migraines", "Cellphone",
              "Cancer.glioma", "Cancer.other", "Disease.other","State")

AGOG.formatted <- AGOG.raw %>% dplyr::select(c("cec_upn","ufn_primary",goodcols))
AGOG.formatted[apply(AGOG.formatted,c(1,2),function(x) grepl( "^\\.", x))] <- NA

### Get data into the proper formats for imputation/calculation
unord.factors <- c("State","Ethnicity")
ord.factors <- c("Alcohol","Education","Income","Cigarettes","Physical.activity","BMI","Cannabis")
numerics <- c("Caffeine","Sedentary.activity","Age")
booleans <- c("Allergies","Cancer.other","NSAIDs","Gender",
              "Statins","Steroids","Migraines","Disease.other","Cellphone")
binarys <- c("Cancer.glioma")

#AGOG.formatted %<>% mutate_at(unord.factors, funs(as.numeric(as.character(.))))
AGOG.formatted %<>% mutate_at(unord.factors, factor)

AGOG.formatted %<>% mutate_at(ord.factors, funs(as.numeric(as.character(.))))
AGOG.formatted %<>% mutate_at(ord.factors, factor)
#AGOG.formatted %<>% mutate_at(ord.factors, ordered)

AGOG.formatted %<>% mutate_at(numerics, funs(as.numeric(as.character(.))))

AGOG.formatted %<>% mutate_at(booleans, factor)
#AGOG.formatted %<>% mutate_at(booleans, funs(recode(.,"Yes" = TRUE, "No" = FALSE, .default = NA)))
#AGOG.formatted %<>% mutate_at(booleans, funs(as.logical(as.integer(.) - 1L)))

AGOG.formatted %<>% mutate_at(binarys, factor)
AGOG.formatted %<>% mutate_at(binarys, funs(recode(.,"Yes" = TRUE, "No" = FALSE, .default = NA)))

rm(booleans,ord.factors,goodcols,numerics)

m <- 10

AGOG.imputes <- mice(AGOG.formatted, m=m, maxit=10, seed=123, 
                     pred=quickpred(AGOG.formatted, method="spearman",exclude= c('cec_upn', 'cancer.glioma','ufn_primary')))

AGOG.dataset <- lapply(1:m, function(i) complete(AGOG.imputes,i))


DAG <- import_dag("D:/Documents/School/Internships/CBDRH/DAGs/currentDag.txt")

Model.crude <- AGOG.model(AGOG.dataset)
Model.adjusted <- AGOG.model(AGOG.dataset, confounders=c("Gender","Age","Ethnicity","State"))
Model.DAG <- AGOG.model.dags(DAG, AGOG.dataset, confounders=c("Gender","Age","Ethnicity","State"))
Model.DAG$Confounders <- str_to_title(Model.DAG$Confounders, locale = "en")
Model.DAG[,1:6]

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

identifier <- " - Cannabis"

path <- paste0("D:/Documents/School/Internships/CBDRH/Data/Data from CBDRH/Generated/",date,identifier,"/")

#   },
#   error=function(cond) {
#     message(cond)
#     err <- TRUE
# })


dir.create(path)
saveWorkbook(wb, file = paste0(path,date,"_AGOG_OR.xlsx"), overwrite = TRUE)
save.image(file=paste0(path,date,"_R_image.rda"))
write.xlsx(AGOG.dataset,paste0(path,date,"_AGOG_dataset.xlsx"))
write.xlsx(AGOG.raw,paste0(path,date,"_AGOG_raw.xlsx"))
write(DAG, file = paste0(path,date,"_AGOG_DAG.txt"))

rm(wb,path,date)



