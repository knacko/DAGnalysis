
setwd("C:\\Users\\geekb\\Documents\\School\\Internships\\CBDRH\\Data\\Data from CBDRH")
list.files(getwd())
getwd()

list.of.packages <- c("magrittr","openxlsx","qdapTools","dplyr","tools","epiDisplay","tidyverse","R.utils",
                      "hablar","stringr","sjmisc","pastecs","dagitty","miceadds","broom","sandwich",
                      "gtools","VIM","compare","Cairo","mice","reactable","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

options(scipen=2)
