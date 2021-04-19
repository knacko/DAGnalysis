### Start up stuff #############################################################################
setwd("C:\\Users\\geekb\\Documents\\School\\Internships\\CBDRH\\Data\\Data from CBDRH")
list.files(getwd())
getwd()

list.of.packages <- c("magrittr","openxlsx","qdapTools","dplyr","tools","epiDisplay","tidyr","R.utils",
                      "hablar","stringr","sjmisc","pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

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

### Merge on cec_upn ################################
setwd("I:\\MED\\CBDRH\\CERU data\\Glioma Study\\Data\\Working Directory")
list.files(getwd())
getwd()

library(openxlsx)

df1 <- read.xlsx("AGOG_hormonal_n0.xlsx", colNames=TRUE)
df2 <- read.xlsx("AGOG_demographics_n0.xlsx", colNames=TRUE)

merged <- merge(df1,df2,"cec_upn")

write.xlsx(merged,"merged.xlsx")

### Export data frame to file ####################

write.xlsx(AGOGdata,'.//_AGOGdata.xlsx')
write.xlsx(AGOG.imputed, './/_AGOGdata.xlsx')
write.xlsx(var, './/_AGOG_DAG_OR.xlsx')

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

factors <- c("vice.alcohol","education","gender","income","vice.cigarette","physical.activity","body.size","vice.cannabis")
numerics <- c("vice.caffeine","mental.activity","age")
booleans <- c("allergies","cancer.other","cancer.glioma","drug.aspirin","drug.nsaid","ethnicity",
              "drug.statin","drug.steroid","migraines","noncancer.disease")

AGOG.formatted$state <- factor(AGOG.formatted$state)

AGOG.formatted %<>% mutate_at(factors, funs(as.numeric(as.character(.))))
AGOG.formatted %<>% mutate_at(factors, factor)

AGOG.formatted %<>% mutate_at(numerics, funs(as.numeric(as.character(.))))

AGOG.formatted %<>% mutate_at(booleans, factor)
AGOG.formatted %<>% mutate_at(booleans, funs(recode(.,"Yes" = TRUE, "No" = FALSE, .default = NA)))
AGOG.formatted %<>% mutate_at(booleans, funs(as.logical(as.integer(.) - 1L)))

rm(booleans,factors,goodcols,numerics)

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




### Remove specific columns





AGOGdata %<>% remove_cols("alcyrs_qty","alc2yrs_qty")
















### From (A), parse alcohol ######################

#check alcyrs_qty
AGOGalch <- AGOGdata %>% dplyr::select(cec_upn,age,alcyrs_qty,alc2yrs_qty,alcstr_age) %>% mutate_at(vars(-("cec_upn")),as.numeric)
AGOGalch$alcyrs_qty <- AGOGalch$alcyrs_qty*-1 

AGOGalch$startAge <- rowSums(AGOGalch %>% dplyr::select(age,alcyrs_qty,))
t(t(stat.desc(AGOGalch$startAge) %>% round(digits = 2)))

AGOGalch %<>%  transform(propLife = abs(100 * alcyrs_qty / age))
AGOGalch %<>%  transform(propHeavy = abs(100 * alc2yrs_qty / alcyrs_qty))

hist(AGOGalch$propLife)
hist(AGOGalch$propHeavy)


### From (A), parse caffeine #####################


AGOGdata$caffeine_usage <- AGOGdata %>% dplyr::select(cola, coffee_inst, coffee, tea, energy) %>% 
  mutate_at(vars(cola:energy), ~ifelse(. == 1, 0, 1)) %>%
  mutate(caffeine = rowSums(., na.rm = TRUE)) %>% mutate_at(vars(caffeine), ~ifelse(. == 0, FALSE, TRUE)) %>% 
  dplyr::select(caffeine) %>% pull(caffeine)
  
#View(AGOGdata$caffeineUsage)

### From (A), parse Proband ######################

AGOGdata$glioma <- as.factor(AGOGdata$caco_cde)
AGOGdata$glioma %<>% recode(Proband = "Yes", Control = "No")

AGOGdata$age <- AGOGdata$dx_age 

### From (A), parse 

AGOGdata$allergies <- AGOGdata$fdalrgy_flg_l

  
### From (A), parse drug usages ##################

AGOGdata$alcohol_usage <- AGOGdata$alchl_flg_l
AGOGdata$cigarette_usage <- AGOGdata$cigevr_flg_l
AGOGdata$cannabis_usage <- AGOGdata$mrjevr_flg_l

AGOGdata$statin_usage <- AGOGdata$statin_evr_flg_l
AGOGdata$nsaids_usage <- AGOGdata$nsaids_reg_flg_l
AGOGdata$steroid_usage <- AGOGdata$steroid_reg_flg_l
AGOGdata$aspirin_usage <- AGOGdata$asprn_flstrg_reg_flg_l

### From (A), parse gender #####################

AGOGdata$gender <- as.factor(AGOGdata$gender_l)


### From (A), parse activity + body size ##################

AGOGdata$physical_activity <- AGOGdata$totalpa_cat




### Binary evaluator #########################3

binary_eval    function(temp_F)
    
    




















### Parse and replace demographics 1 #############################################################################
df <- read.xlsx("AGOG_demographic_n1.xlsx", colNames=TRUE)


### Parse and replace demographics 2 #############################################################################

df <- read.xlsx("AGOG_demographic2_n0.xlsx", colNames=TRUE)
dff <- df

#Replace all the invalids
dff$Ethnic_moth %<>% parseInvalids()
dff$Ethnic_fath %<>% parseInvalids()
dff$Educ %<>% parseInvalids()
dff$Educ_age %<>% parseInvalids()
dff$Income %<>% parseInvalids()


all_zero <- function(x) any(!(x == 0))

fath <- df %>% dplyr::select(cec_upn,ethnic_fath,ethnic_fath_oth,starts_with("ethnic_fath")) %>% filter(ethnic_fath_oth != ".y") %>% select_if(all_zero)
names(fath) <- gsub("ethnic_father", "e~f", names(fath))
fath


moth <- df %>% dplyr::select(cec_upn,ethnic_moth,ethnic_moth_oth,starts_with("ethnic_moth")) %>% filter(ethnic_moth_oth != ".y") %>% select_if(all_zero)
names(moth) <- gsub("ethnic_mother", "e~m", names(moth))
moth

educLookup <- data.frame(val = 1:9,text=c("Primary School","Year 8 or 9","Year 10 or 11","Year 12 or graduated","Vocational Training","University (did not graduate)","University (bachelor)","Postgraduate degree","Other"))
dff$Educ %<>% parseData(educLookup)

incomeLookup <- data.frame(val=1:8,text=c("< $25,000","$25,000 - $50,000","$50,000 - $75,000"," $75,000 - $100,000","$100,000 - $125,000"," $125,000 - $150,000","> $150,000","Prefer not to answer" ))
dff$Income %<>% parseData(incomeLookup)

write.xlsx(dff,"AGOG_demographic2_n0.xlsx")

mother_ethnic <- c("ethnic_mother_cauc_flg","ethnic_mother_jpn_flg","ethnic_mother_chns_flg","ethnic_mother_flpmly_flg","ethnic_mother_krn_flg","ethnic_mother_seasn_flg","ethnic_mother_sthasn_flg",
                   "ethnic_mother_maori_flg","ethnic_mother_mcnsn_flg","ethnic_mother_abrl_flg","ethnic_mother_mlnsn_flg","ethnic_mother_crrbn_flg","ethnic_mother_cntrstham_flg","ethnic_mother_blkafrn_flg",
                   "ethnic_mother_nthafrn_flg","ethnic_mother_mdlesn_flg","ethnic_mother_pcfc_flg","ethnic_mother_unk_flg","ethnic_mother_oth_flg","ethnic_mother_oth_txt")

father_ethnic <- c("ethnic_father_cauc_flg","ethnic_father_jpn_flg","ethnic_father_chns_flg","ethnic_father_flpmly_flg","ethnic_father_krn_flg","ethnic_father_seasn_flg","ethnic_father_sthasn_flg",
                   "ethnic_father_maori_flg","ethnic_father_mcnsn_flg","ethnic_father_abrl_flg","ethnic_father_mlnsn_flg","ethnic_father_crrbn_flg","ethnic_father_cntrstham_flg","ethnic_father_blkafrn_flg",
                   "ethnic_father_nthafrn_flg","ethnic_father_mdlesn_flg","ethnic_father_pcfc_flg","ethnic_father_unk_flg","ethnic_father_oth_flg","ethnic_father_oth_txt")

dff[mother_ethnic] <- 0
dff[father_ethnic] <- 0



ethToCONFIRM <- function() {
  
  
  
  
  
}



  
### Parse and replace the caffeine data  #############################################################################
df <- read.xlsx("Caffeine.xlsx", colNames=TRUE, na.strings = "NA")
df[is.na(df)] <- 0

val <- 0:7
Cola <- c("Unanswered","None","Less than 1 glass per day","2 glasses (or 1 can)","3 glasses","4 glasses","5-7 glasses","More than 7 glasses")
Coffee_inst <- c("Unanswered","None","Less than 1 cup per day","2 cups","3 cups","4 cups","5-7 cups","More than 7 cups")
Coffee <- c("Unanswered","None","Less than 1 cup per day","2 cups","3 cups","4 cups","5-7 cups","More than 7 cups")
Tea <- c("Unanswered","None","Less than 1 cup per day","2 cups","3 cups","4 cups","5-7 cups","More than 7 cups")
Energy <- c("Unanswered","None","Less than 1 can per day","2 cans","3 cans","4 cans","5-7 cans","More than 7 cans")

lookup <- data.frame(val,Cola,Coffee_inst,Coffee,Tea,Energy)

dff <- df
dff$cola_l <- lookup$Cola[match(unlist(dff$Cola), lookup$val)]
dff$coffee_inst_l <- lookup$Coffee_inst[match(unlist(dff$Coffee_inst), lookup$val)]
dff$coffee_l <- lookup$Coffee[match(unlist(dff$Coffee), lookup$val)]
dff$tea_l <- lookup$Tea[match(unlist(dff$Tea), lookup$val)]
dff$energy_l <- lookup$Energy[match(unlist(dff$Energy), lookup$val)]

dff$cola <- lookup$val[match(unlist(dff$cola_l), lookup$Cola)]
dff$coffee_inst <- lookup$val[match(unlist(dff$coffee_inst_l), lookup$Coffee_inst)]
dff$coffee <- lookup$val[match(unlist(dff$coffee_l), lookup$Coffee)]
dff$tea <- lookup$val[match(unlist(dff$tea_l), lookup$Tea)]
dff$energy <- lookup$val[match(unlist(dff$energy_l), lookup$Energy)]


write.xlsx(dff,"AGOG_caffeine_n0_formatted.xlsx")

### Parse and replace for Mobile Data #############################################################################
df <- read.xlsx("AGOG_mobile_n0.xlsx", colNames=TRUE, na.strings = "NA")

dff <- df

dff$mobile %<>% parseInvalids()
dff$mobile_freq %<>% parseInvalids()
dff$mobile_side %<>% parseInvalids()

#Replace the mobile_side
side <- data.frame(val = 0:2,text=c("Left","Right","Both"))
for(id in 1:nrow(side)){
  dff$mobile_side[which(dff$mobile_side %in% as.character(side$val[id]))] <- as.character(side$text[id])
}

write.xlsx(dff,"AGOG_mobile_n0_formatted.xlsx")

### Parse and replace workplace radiation ###############################################
df <- read.xlsx("AGOG_work_radiation_n0.xlsx", colNames=TRUE, na.strings = "NA")
dff <- df

### Parse and replace medications 2 ####################################################
df <- read.xlsx("AGOG_work_radiation_n0.xlsx", colNames=TRUE, na.strings = "NA")
dff <- df


### Parse and replace hormonal ##########################################################
df <- read.xlsx("AGOG_hormonal_n0.xlsx", colNames=TRUE, na.strings = "NA")
dff <- df

dff %<>% parseInvalids()
dff$Menses_start %<>% parseInvalids()


write.xlsx(dff,"AGOG_hormonal_n0.xlsx")

### Parse and replace medications ##########################################################
df <- read.xlsx("AGOG_medications_n1.xlsx", colNames=TRUE, na.strings = "NA")
dff <- df

dff %<>% parseInvalids()

write.xlsx(dff,"AGOG_medications_n1.xlsx")

### Join all by cec_upn ##########################################################
setwd("J:\\CERU data\\Glioma Study\\Data\\Working Directory")
list.files(getwd())
getwd()

dir <- list.files(getwd())

rm(df)

for(id in 1:length(dir)) {
  
  if (file_ext(dir[id]) == "csv") {
   
    newDF <- read.csv(dir[id])#, na.strings = c(".a",".m",".r",".u",".w",".x"))
     
  } else if (file_ext(dir[id]) == "xlsx") {
    
    newDF <- read.xlsx(dir[id], colNames=TRUE)#, na.strings = c(".a",".m",".r",".u",".w",".x"))
  
  }
  
  paste(dir[id],"added")
  
  df <- if (id==1) newDF else df <- merge(df,newDF,by="cec_upn")

}
colSums(is.na(df))
sum(complete.cases(df2))

df2 <- subset(df,caco_cde == "Proband")


write.xlsx(df,"Merged Copy.xlsx")

columns <- c("dx_age","asthm_flg")
uni(df,columns)

uni <- function(df,columns) {

  for (i in 1:length(columns)) {
  
    frm <- as.formula(paste("death_flag ~", paste(columns[1:i],collapse=" + ")))

    results <- glm(formula = frm, family = binomial, data = df, na.action = na.omit) ## works fine
    odds <- exp(coef(results))
    print(odds) 
  }
}

colnames((df))
str(df$death_flag)

str(df$asthm_flg)
df$calc_age %<>% as.numeric()
df$death_flag %<>% factor()


###############  Add label columns
df <- read.xlsx("AGOG_hormonal_n0.xlsx", colNames=TRUE)

rm(cols)



df %<>% select(-cec_upn,-Menses_start,-Menop_age,-Oophrect_age,-hysterect_age,-other_age,-hrt_start,-hrt_stop)

Menses_start,Menop_age,Oophrect_age,hysterect_age,other_age,hrt_start,hrt_stop

colnames(cols)

