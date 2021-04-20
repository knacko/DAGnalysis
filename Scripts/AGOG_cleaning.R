
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
