### Start up stuff #############################################################################

invalids <- data.frame(val=c(".a",".m",".r",".u",".w",".x",".y"),
                       text=c("At least one","Invalid response given","Refused to answer","Don't know","Not asked", "Not answered","Not applicable"))

parseInvalids <- function (d) {
  return(parseData(d, invalids))
}

parseData <- function(d, lookup) {
  
  if (is.null(ncol(d))) return(parseCol(d,lookup))
  
  for (col in 1:ncol(d)) {
    
    d[,col] %<>% parseCol(lookup)
  }
  
  return(d)
}

parseCol <- function(col,lookup) {
  
  for(id in 1:nrow(lookup)){
    col[which(col %in% as.character(lookup$val[id]))] <- as.character(lookup$text[id])
  }
  return(col)
}

### Export data frame to file ####################
write.xlsx(AGOGdata,'.//_AGOGdata.xlsx')

### Import data frame from file ##################
AGOG.data <- read.xlsx('.//_2020-12-30_16_28_AGOG_raw.xlsx', colNames = TRUE,)

### (A) Load all data from shortcuts #############################################################################

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

goodcols <- c("age", "gender","ethnicity", "education", "income","body.size", 
              "physical.activity","mental.activity","vice.alcohol", "vice.cigarette",
              "vice.cannabis", "vice.caffeine",
              "drug.aspirin", "drug.nsaid", "drug.steroid", "drug.statin", 
              "allergies", "migraines",
              "cancer.glioma", "cancer.other", "noncancer.disease","state")

AGOG.formatted <- AGOG.raw %>% dplyr::select(c("cec_upn","ufn_primary",goodcols))

### Get data into the proper formats for imputation/calculation

unord.factors <- c("state")
ord.factors <- c("vice.alcohol","education","gender","income","vice.cigarette","physical.activity","body.size","vice.cannabis")
numerics <- c("vice.caffeine","mental.activity","age")
booleans <- c("allergies","cancer.other","drug.aspirin","drug.nsaid","ethnicity",
              "drug.statin","drug.steroid","migraines","noncancer.disease")
binarys <- c("cancer.glioma")

#AGOG.formatted %<>% mutate_at(unord.factors, funs(as.numeric(as.character(.))))
AGOG.formatted %<>% mutate_at(unord.factors, factor)

AGOG.formatted %<>% mutate_at(ord.factors, funs(as.numeric(as.character(.))))
AGOG.formatted %<>% mutate_at(ord.factors, factor)
AGOG.formatted %<>% mutate_at(ord.factors, ordered)

AGOG.formatted %<>% mutate_at(numerics, funs(as.numeric(as.character(.))))

AGOG.formatted %<>% mutate_at(booleans, factor)
#AGOG.formatted %<>% mutate_at(booleans, funs(recode(.,"Yes" = TRUE, "No" = FALSE, .default = NA)))
#AGOG.formatted %<>% mutate_at(booleans, funs(as.logical(as.integer(.) - 1L)))

AGOG.formatted %<>% mutate_at(binarys, factor)
AGOG.formatted %<>% mutate_at(binarys, funs(recode(.,"Yes" = TRUE, "No" = FALSE, .default = NA)))

rm(booleans,ord.factors,goodcols,numerics)

md.pattern(AGOG.formatted,rotate.names=TRUE)

### Count all .x

count.char <- '.x'
count.char <- NA

row_count(AGOG.formatted,count=count.char,append=FALSE)             ### By row
length(which(row_count(AGOG.formatted,count=count.char,append=FALSE)>0)) ### Rows missing values
sort(t(col_count(AGOG.formatted,count=count.char,append=FALSE)))             ### By col
colSums(row_count(AGOG.formatted,count=count.char,append=FALSE))    ### Entire DF

### Get percentage of .x

head(sort(t(col_count(AGOG.formatted,count=count.char,append=FALSE))[,1],decreasing = TRUE)/nrow(AGOG.formatted)*100)


