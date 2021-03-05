### Setup data ##########################

library(VIM)
library(dplyr)
library(compare)
library(tidyverse)
library(mice)
library(magrittr)
library(openxlsx)
setwd("C:\\Users\\geekb\\Documents\\School\\Internships\\CBDRH\\Data\\Data from CBDRH")

### End setup data ######################

dat <- AGOGdata %>% dplyr::select(cec_upn,age,gender,body.size,education,income,vice.alcohol,physical.activity,mental.activity)

dat %<>% mutate(
  age = as.numeric(age),
  gender = as.factor(gender),
  body.size = as.numeric(body.size),
  education = as.factor(education),
  income = as.factor(income),
  vice.alcohol = as.factor(vice.alcohol),
  physical.activity = as.numeric(physical.activity),
  mental.activity = as.numeric(mental.activity)
)

sapply(dat, function(x) sum(is.na(x)))

original <- dat

init = mice(dat, maxit=0) 
meth = init$method
predM = init$predictorMatrix
predM[, c("cec_upn")]=0

meth[c("age","body.size","physical.activity","mental.activity")]="norm" 
meth[c("gender","vice.alcohol")]="logreg" 
meth[c("education","income")]="polyreg"

set.seed(103)
tempData = mice(dat, method=meth, predictorMatrix=predM, m=25)

tempData <- mice(dat, method="cart", m=5, maxit=10, seed=123, 
            pred=quickpred(dat, method="spearman"))

densityplot(tempData)

completedData <- complete(tempData,4)

original <- stat.desc(na.omit(dat$physical.activity)) %>% round(digits = 2)

imputed <- stat.desc(completedData$physical.activity) %>% round(digits = 2)

cbind(original, imputed)

plot(tempData, c("physical.activity","body.size"))

stripplot(tempData, pch = 20, cex = 1.2)
xyplot(tempData,physical.activity ~ age+gender+body.size+education+income+vice.alcohol+mental.activity,pch=18,cex=1)

modelFit1 <- with(tempData,glm(physical.activity ~ age+gender+body.size+education+income+vice.alcohol+mental.activity))

modelFit1 <- with(tempData,glm(physical.activity ~ body.size))
summary(pool(modelFit1))


### Select file to impute ############################



### Imputing Physical activity ################

file.name <- "20200124_AGOG_PhysicalActivity.xlsx"
df <- read.xlsx(file.name, colNames=TRUE)

df[,-1] %<>% mutate_all(as.numeric)

md.pattern(df,rotate.names=TRUE)

df %<>% mutate(sum = rowSums(.[2:ncol(df)], na.rm = TRUE))

df2 <- AGOG.data %>% dplyr::select(cec_upn,cancer.glioma)

df <- inner_join(df2,df,by="cec_upn")

df.bad <- dplyr::filter(df, sum == 0)
df.subset <- dplyr::filter(df, sum != 0)[1:10]
df.good <- na.omit(df)

df$sum <- NULL

#df.subset <- df.subset[1:10] # %>% dplyr::select(cec_upn,pa_15, pa_19,pa_30,pa_10yr,totalpa)

#numerics <- c("pa_15","pa_19","pa_30","pa_10yr","totalpa")
#factors <- c()
#booleans <- c()

#df.subset %<>% mutate_at(numerics, funs(as.numeric(as.character(.))))

md.pattern(df.subset,rotate.names=TRUE)

df.imputes <- mice(df.subset,m=35,maxit=10,meth='pmm',seed=500,pred=quickpred(df.subset, method="spearman",exclude= c('cec_upn', 'cancer.glioma'))) # 35 = 100 x FIC (fraction of incomplete cases)

#df.imputes <- mice(df.subset, method="cart", m=35, maxit=10, seed=1234, 
#                 pred=quickpred(df.subset, method="spearman",exclude= c('cec_upn', 'cancer.glioma')))

df.imputes.long <- complete(df.imputes, action='long', include=TRUE)

df.imputes.long$totalpa <- (rowSums(df.imputes.long[5:8]*3)+rowSums(df.imputes.long[9:12]*5))/4

df.imputes <- as.mids(df.imputes.long )

md.pattern(df.imputes.long,rotate.names=TRUE)

df.model <- with(df.imputes,glm(cancer.glioma ~ total_pa))#pa_light_15 + pa_mod_19 + pa_light_19 + pa_light_30 + pa_light_10yr + pa_mod_15 + pa_mod_30 + pa_mod_10yr))
summary(pool(df.model))

df.anova <- mi.anova(mi.res=df.imputes,"cancer.glioma ~ totalpa")


densityplot(df.imputes)
stripplot(df.imputes, pch = 20, cex = 1.2)

df.imputed <- complete(df.imputes,4)

df.imputes$data$totalpa <- (rowSums(df.imputed[3:6]*3)+rowSums(df.imputed[7:10]*5))/4
df.imputes$imp$totalpa <- (rowSums(df.imputed[3:6]*3)+rowSums(df.imputed[7:10]*5))/4

summary(df.imputed$totalpa)
summary(df.good$totalpa)

summary(df.updated$totalpa)

### Merge data to the original df ############

df.updated <- update_df(df,df.imputed,"cec_upn")

### Save back to the same file ##############################

write.xlsx(df,paste(Sys.Date(),file.name))
write.xlsx(df.updated,file.name)

### Compare frame function ##################################

df1 <- df.subset
df2 <- df.imputed

comparedf <- function(df1, df2, by.col) {
  
  vec <- vector(length=ncol(df1)) 
  
  for(i in 1:ncol(df1)) { 
  
    vec[i] <- all(df1[i] == df2[i])
  
  }
  
  vec[is.na(vec)] <- FALSE   # All mismatched values from above show as NAs, so replace
  
  df2 <- df2[ which(!vec) ]  # Remove the duplicate columns
  
  df2[[by.col]] <- df1[[by.col]]     # Replaced the pivot column

  df.combined <- left_join(df1,df2,by="cec_upn") # join with unmatched columns
  
  df.combined <- df.combined[,order(colnames(df.combined))] # order the columns
  
  rows.na <- rownames(df1)[!complete.cases(df1)] # find the rows with NAs
  
  df.imp <- df.combined[rows.na,] # exclude rows without NAs
  
}
  
  
anti_join(df1, df2, by="cec_upn")

### Impute master AGOG data (all in proper data types and containing only DAG nodes)

AGOG.imputes <- mice(AGOG.formatted, method="cart", m=5, maxit=10, seed=123, 
                   pred=quickpred(AGOG.formatted, method="spearman",exclude= c('cec_upn', 'cancer.glioma','ufn_primary')))

AGOG.dataset <- complete(AGOG.imputes,2)

#AGOG.dataset$vice.cannabis %<>% as.logical() #Not sure why this is necessary

summary(AGOG.dataset)
summary(AGOG.formatted)

md.pattern(AGOG.dataset,rotate.names=TRUE)


### Functions ######################

update_df <- function(df1,df2,index) {

  df3 = full_join(df1,df2)
  
  index <- as.formula(paste(". ~ `", index,"`",sep=""))
  
  # Create a function na.omit.last 
  na.omit.last = function(x){
    x <- na.omit(x)
    x <- last(x)
  }
  
  # For the columns not in df1 
  dfA = aggregate(index, df3, na.omit,na.action = na.pass)
  dfA = dfA[,-(1:ncol(df1))] 
  dfA = data.frame(lapply(dfA,as.numeric))
  
  dfB = aggregate(index, df3[,1:ncol(df1)], na.omit.last, na.action = na.pass)
  
  # If there are more columns in df2 append dfA
  if (ncol(df2) > ncol(df1)) {
    df3 = cbind(dfB,dfA)
  }  else {
    df3 = dfB
  }
  
  return(df3)
  
}


