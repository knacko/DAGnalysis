#---- Setup the env ----------------------------------------------------------------------------------------------------
setwd("C:\\Users\\geekb\\Documents\\School\\Internships\\CBDRH\\Data\\Data from CBDRH")
list.files(getwd())
getwd()

list.of.packages <- c("magrittr","openxlsx","qdapTools","dplyr","tools","epiDisplay","tidyverse","R.utils",
                      "hablar","stringr","sjmisc","pastecs","dagitty","miceadds","broom","sandwich",
                      "gtools","VIM","compare","Cairo","mice","reactable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

options(scipen=2)

source("D:/Home/School/Internships/CBDRH/DAGnalysis/Scripts/AGOG_dag_analysis.R")

#---- Load the data ----------------------------------------------------------------------------------------------------
setwd("D:\\Home\\School\\Internships\\CBDRH\\Data\\Data from CBDRH\\Shortcuts")

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

#---- Format data ------------------------------------------------------------------------------------------------------
# Get data into the proper formats for imputation/calculation
unord.factors <- c("State","Ethnicity")
ord.factors <- c("Alcohol","Education","Income","Cigarettes","Physical.activity","BMI","Sedentary.activity","Caffeine","Cellphone")
numerics <- c("Age")
booleans <- c("Allergies","Cancer.other","NSAIDs","Gender","Aspirin","Statins","Steroids","Migraines","Disease.other","Cannabis")
binarys <- c("Cancer.glioma")

goodcols <- c(unord.factors,ord.factors,numerics,booleans,binarys)

AGOG.formatted <- AGOG.raw %>% dplyr::select(c("cec_upn","ufn_primary",goodcols))
AGOG.formatted[apply(AGOG.formatted,c(1,2),function(x) grepl( "^\\.", x))] <- NA

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

#AGOG.formatted <- filter(AGOG.formatted, !is.na(Income))
#AGOG.formatted <- na.omit(AGOG.formatted)

#---- Impute -----------------------------------------------------------------------------------------------------------

# Should use m = 35 and maxit = 10 for final results, but m = 10 and maxit = 5 seems suitable for rough calculations
m <- 10
maxit <- 5

AGOG.imputes <- mice(AGOG.formatted, m=m, maxit=maxit, seed=123, 
                     pred=quickpred(AGOG.formatted, method="spearman",exclude= c('cec_upn', 'cancer.glioma','ufn_primary')))

AGOG.dataset <- lapply(1:m, function(i) complete(AGOG.imputes,i))


#---- Model ------------------------------------------------------------------------------------------------------------

DAG <- import_dag("D:/Home/School/Internships/CBDRH/DAGs/currentDag.txt")

Model.crude <- AGOG.model(AGOG.dataset)
Model.adjusted <- AGOG.model(AGOG.dataset, confounders=c("Gender","Age","Ethnicity","State"))
Model.DAG <- AGOG.model.dags(DAG, AGOG.dataset, confounders=c("Gender","Age","Ethnicity","State"))
Model.DAG$Confounders <- stringr::str_to_title(Model.DAG$Confounders, locale = "en")
Model.DAG[,1:6]

#---- Generate plot data -----------------------------------------------------------------------------------------------

Model.sig <- as.data.frame(Model.DAG %>% group_by(Variable) %>% top_n(1, OR))
#Model.sig <- filter(Model.sig,Sigificance != " ")

dict <- read.xlsx("D:/Home/School/Internships/CBDRH/Report/AGOG_master_data_dict.xlsx", colNames=TRUE)

cats <- sprintf("(%s)", paste(unique(dict$Category), collapse = "|"))

dats <- gsub(cats, "|\\1|", Model.sig[,1]) %>% 
  strsplit(., "|", fixed = TRUE) %>%
  do.call(rbind, .)
dats <- as.data.frame(dats[,2:3])
colnames(dats) <- c("Cat","Var")

dats <- cbind(dats,Cat_label = plyr::mapvalues(dats$Cat, from=unique(dict$Category), to=unique(dict$Category_label)))
dats <- cbind(dats,Val_label = plyr::mapvalues(paste0(dats$Cat,dats$Var), from=paste0(dict$Category,dict$Variable), to=dict$Variable_label))

dats$Cat_label[duplicated(dats$Cat_label)] <- NA

#dats$Val_label[which(dats$Val_label=="Caffeine")] <- NA
dats$Val_label[which(dats$Val_label==" ")] <- NA

dats %<>% unite(., col = "Label",  Cat_label, Val_label, remove = FALSE, na.rm=TRUE, sep = "; ")

#Model.sig$Variable <- dats$Val_label
# Model.sig$Category <- dats$Cat_label
# Model.sig$Variable = factor(Model.sig$Variable, levels = Model.sig$Variable)

#---- Generate the plot ------------------------------------------------------------------------------------------------

plotDat <- cbind(Model.sig,dats)

order <- c("Education","Income","BMI","Physical.activity","Sedentary.activity","Cellphone","Alcohol","Cigarettes","Cannabis","Caffeine","Aspirin","NSAIDs","Steroids","Statins","Allergies","Migraines","Disease.other","Cancer.other")

plotDat <- plotDat[order(factor(plotDat$Cat, levels = order)),]

header.idx <- which(duplicated(plotDat$Cat)) - 1
header.idx <- header.idx[data.table::rowid(collapse::seqid(header.idx)) %% 2 == 1]
header.idx <- sort(c(header.idx,header.idx))
header.idx <- header.idx + seq(header.idx) - 1

#header.idx <- sort(append(header.idx,which(plotDat$Cat == "Aspirin")))

insertRow <- function(df, idx) {
  df[seq(idx+1,nrow(df)+1),] <- df[seq(idx,nrow(df)),]
  df[idx,] <- NA
  df
}

for (idx in header.idx) {
  plotDat <- insertRow(plotDat,idx)
}

plotDat <- insertRow(plotDat, which(plotDat$Cat == "Aspirin"))
plotDat <- insertRow(plotDat, which(plotDat$Cat == "Cannabis"))

CairoWin(width=4, height = 4)
forest <- ggplot(data=plotDat, aes(y=Label, x=OR, xmin=CI2.5, xmax=CI97.5))+ 
  geom_point(size=3,shape=15)+ 
  geom_errorbarh(height=.5,size=1)+
  geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5)+
  
  scale_x_continuous(name="Odds Ratio",limits=c(0,4), breaks = c(0:3))+
  scale_y_discrete(limits = rev(plotDat$Label))+#levels(factor(Model.sig$Variable))))+
  
  theme_minimal()+
  theme(text=element_text(family="Times",size=18, color="black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))+
  theme(panel.spacing = unit(1, "lines")) +
  theme(text=element_text(size=16,  family="sans"))
forest

# forest <- ggplot(data=Model.sig, aes(y=Variable, x=OR, xmin=CI2.5, xmax=CI97.5))+ 
#   geom_point(size=3,shape=15)+ 
#   geom_errorbarh(height=.5,size=1)+
#   geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5)+
#   
#   scale_x_continuous(name="Odds Ratio",limits=c(0,4), breaks = c(0:3))+
#   labs(y="Risk Factor")+
#   scale_y_discrete(limits = rev(levels(factor(Model.sig$Variable))))+
#   
#   theme_minimal()+
#   theme(text=element_text(family="Times",size=18, color="black"))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))+
#   theme(panel.spacing = unit(1, "lines")) +
#   theme(text=element_text(size=16,  family="sans"))
# forest

#---- Save -------------------------------------------------------------------------------------------------------------

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

path <- paste0("D:/Home/School/Internships/CBDRH/Data/Data from CBDRH/Generated/",date,identifier,"/")

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
ggsave(filename=paste0(date,"_forest.jpg"),plot=p,path = path,bg="white")




#rm(wb,path,date)



