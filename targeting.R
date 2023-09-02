
#This data is only collected every 90 days
#enter date under get data + under clean

#install.packages("metatargetr")
#remotes::install_github("favstats/metatargetr")
library(metatargetr)
library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

rm(list=ls())
options(scipen=999)



# 1. Get and translate data -----------------------------------------------

# 1.1 get metatargetr data ----------------------------------------------------------------

###get relevant page ids based on report data
source("Fidentify.R")

#date<-"2023-06-30"

report<-read.csv(paste0("input/targeting/FacebookAdLibraryReport_",date,"_CH_last_90_days_advertisers.csv"),encoding="UTF-8")
report<-report %>% 
  rename("disclaimer"="Disclaimer")
#identify parties
report<-Fidentify(report)
report_all<-report
report<-report %>% 
  filter(!is.na(party))
#remove duplicates
report<-report %>% 
  distinct(Page.ID,.keep_all=T)
#remove pages with spending less than 100 
report<-report %>% 
  filter(Amount.spent..CHF.!="≤100") 


data90<-data.frame(c(NA))
for(i in 1:length(report$Page.ID)) {
  down<-get_targeting(report$Page.ID[i],timeframe = "LAST_90_DAYS")
  if(length(down$ds)==0){next}
  else {
  data90<-plyr::rbind.fill(data90, down)
    }
}

save(data90,file=paste("output/targeting/data90_",date,".rdata"))


# 1.2 Clean -------------------------------------------------------------------

rm(list=ls())

date<-"2023-06-30"

#1) get report data first (see above)
source("Fidentify.R")
options(scipen=999)


report<-read.csv(paste0("input/targeting/FacebookAdLibraryReport_",date,"_CH_last_90_days_advertisers.csv"),encoding="UTF-8")
report<-report %>% 
  rename("disclaimer"="Disclaimer")
#identify parties
report<-Fidentify(report)
report_all<-report
report<-report %>% 
  filter(!is.na(party))
#remove duplicates
report<-report %>% 
  distinct(Page.ID,.keep_all=T)
#remove pages with spending less than 100 
report<-report %>% 
  filter(Amount.spent..CHF.!="≤100") 

#2) data 90 
#for each page you have the information of which percentage of their spending they invested for a particular target group + you get the total spending
load(paste("output/targeting/data90_",date,".rdata"))

data90<-data90 %>% 
  mutate(CHF=gsub("[^[:digit:]]","",total_spend_formatted))
data90<-data90 %>% 
  rename("Page.ID"="internal_id")
data90<-data90 %>% 
  mutate(CHF=as.numeric(CHF))

data90<-merge(data90,report %>% select(Page.ID,Page.name,disclaimer,party),by="Page.ID")

#make data inclusive 
data90<-data90 %>% 
  mutate(date=ymd(ds)-1)

data90$date[1]+90
#select variables wanted for output
data90<-data90 %>% 
    rename("Akteur"="party","Datum"="date","Seite"="Page.name","Zielgruppe"="value",
         "Ausgaben_pct"="total_spend_pct","Kategorie"="type","Anzahl"="num_ads","Ausgeschlossen"="is_exclusion","Kategorie_det"="detailed_type") %>% 
  select("Akteur","Seite","Datum","Anzahl","CHF","Zielgruppe","Ausgaben_pct","Kategorie","Anzahl","Ausgeschlossen","Kategorie_det","custom_audience_type","location_type")

partyorder<-c("SPS","GPS","GLP","EVP","Mitte","FDP","SVP")
data90<-data90 %>% 
  mutate(Akteur=factor(Akteur,levels=c(partyorder,setdiff(unique(data90$Akteur), partyorder)))) 


##add French names 
data90<-data90 %>% 
  mutate(Akteur_fr = case_when(
    Akteur=="SPS" ~ "PS",
    Akteur=="GPS" ~ "PES",
    Akteur=="GLP" ~ "PVL",
    Akteur=="EVP" ~ "PEV",
    Akteur=="Mitte" ~ "Centre",
    Akteur=="FDP" ~ "PLR",
    Akteur=="SVP" ~ "UDC",
    Akteur=="GV" ~ "UAM",
    Akteur=="AV" ~ "UP",
    Akteur=="ES" ~ "ES",
    Akteur=="BV" ~ "UdP",
    Akteur=="GB" ~ "US",
    Akteur=="IGW" ~ "IGW"
  ))

#assure order also for french names
data90<-data90 %>% 
  mutate(Akteur_fr=factor(Akteur_fr,levels=unique(data90$Akteur_fr[order(data90$Akteur)])))


parties <- list("SPS","GPS","GLP", "Mitte","FDP","SVP")


data90<-data90 %>% 
  mutate(Akteur_type=case_when(
    Akteur %in% parties ~ "Parteien",
    TRUE ~ "Interessengruppen"
  ))

data90<-data90 %>% 
  mutate(Akteur_type_fr=recode(Akteur_type,"Parteien"="Partis","Interessengruppen"="Groupes d'intérêt"))

data90<-data90 %>% 
  mutate(Akteur_type=factor(Akteur_type,levels=c("Parteien","Interessengruppen")))

data90<-data90 %>% 
  mutate(Akteur_type_fr=factor(Akteur_type_fr,levels=c("Partis","Groupes d'intérêt")))

data90<-data90 %>% 
  mutate_at("Ausgeschlossen",as.character) %>% 
  mutate(Ausgeschlossen=recode(Ausgeschlossen,"FALSE"="Eingeschlossen","TRUE"="Ausgeschlossen")) %>% 
  mutate(Ausgeschlossen=factor(Ausgeschlossen,levels=c("Eingeschlossen","Ausgeschlossen")))

# 1.3 translate ---------------------------------------------------------------

#load previously translated 
#load("G:/Meine Ablage/Work/R work/Swissdashboard/output/targeting/data90_04042023.Rdata")
load("G:/Meine Ablage/Work/R work/Swissdashboard/output/targeting/detailed/target_det_2023-04-01.Rdata")
translate_help<-target_det %>% 
  select(Zielgruppe, Zielgruppe_de, Zielgruppe_fr) %>% 
  distinct(Zielgruppe, .keep_all = T)

#detect language
# data90$language<-textcat::textcat(data90$Zielgruppe)
# table(data90$language)

translate<-data90 %>%
  filter(Kategorie=="detailed") %>% 
  distinct(Zielgruppe) 

translate_all<-merge(translate,translate_help, by="Zielgruppe",all.x=T)
translate_old<-translate_all %>% 
  filter(!is.na(Zielgruppe_de))
translate<-translate_all %>% 
  filter(is.na(Zielgruppe_de))

#translate
library(deeplr)

my_key<-c("") #get from deepl account
my_key

#check
available_languages2(my_key)
usage2(my_key)

##german (deepl free :deeplr::tranlsate2 vs deeplr::translate)
translate <-translate %>%
  mutate(Zielgruppe_de=translate2(text=Zielgruppe,target_lang="DE",source_lang="EN",auth_key=my_key))

translate_sec<-translate

#shorten long names 
long<-translate %>% 
  filter(nchar(Zielgruppe_de)>25)

# long<-long %>% 
#    mutate(Zielgruppe_short=sub('\\(.*\\)','',Zielgruppe_de)) %>% 
#    select(-Zielgruppe_de)

long<-long %>% 
  mutate(Zielgruppe_short=recode(Zielgruppe,"Anlagenmechaniker für Sanitär-, Heizungs- und Klimatechnik"="Anlagenmechaniker",
                              "ecole polytechnique fédérale de lausanne epfl"="EPFL")) %>% 
  select(-Zielgruppe_de,-Zielgruppe_fr)

translate<-merge(translate,long,by=c("Zielgruppe"),all=T)

translate<-translate %>% 
  mutate(Zielgruppe_de=case_when(
    !is.na(Zielgruppe_short) ~ Zielgruppe_short,
    TRUE ~ Zielgruppe_de
  ))

translate<-translate %>% 
  select(-Zielgruppe_short)

#french
translate_fr <-translate %>%
  mutate(Zielgruppe_fr=translate2(text=Zielgruppe,target_lang="FR",source_lang="EN",auth_key=my_key))

translate_fr_sec<-translate_fr


#shorten long names 
long<-translate_fr %>% 
  filter(nchar(Zielgruppe_fr)>25)

long<-long %>% 
  mutate(Zielgruppe_short=sub('\\(.*\\)','',Zielgruppe_fr)) %>% 
  select(-Zielgruppe_fr,-Zielgruppe_de)

long<-long %>% 
  mutate(Zielgruppe_short=recode(Zielgruppe_short,"Anlagenmechaniker für Sanitär-, Heizungs- und Klimatechnik"="Mécanicien d'installations",
                                 "ecole polytechnique federale de lausanne epfl"="EPFL"))

translate_fr<-merge(translate_fr,long,by=c("Zielgruppe"),all=T)

translate_fr<-translate_fr %>% 
  mutate(Zielgruppe_fr=case_when(
    !is.na(Zielgruppe_short) ~ Zielgruppe_short,
    TRUE ~ Zielgruppe_fr
  ))

translate_fr<-translate_fr %>% 
  select(-Zielgruppe_short)


#merge back together
translate_newold<-rbind(translate_old,translate_fr)
data90<-merge(data90,translate_newold,by=c("Zielgruppe"),all=TRUE)

save(data90,file=paste0("output/targeting/data90_",date,"_translate.rdata"))



# 2. Prepare data ---------------------------------------------------------

# 2.1 further data prep ------------------------------------------------------

date<-"2023-06-30"

##get translated data
load(file=paste0("output/targeting/data90_",date,"_translate.rdata"))


####make sure that every page has at least one observation for each target group if another page of that party has used this target group
data90_full<-data90 %>% 
  group_by(Akteur,Akteur_fr,Akteur_type,Akteur_type_fr) %>% 
  tidyr::complete(tidyr::nesting(Zielgruppe,Zielgruppe_de,Zielgruppe_fr,Ausgeschlossen,Kategorie,Kategorie_det), tidyr::nesting(Seite,CHF,Datum),
                  fill=list(Ausgaben_pct=0)) %>% 
  ungroup()


#check 
table(data90_full$Seite[data90_full$Akteur=="SPS"],data90_full$Kategorie[data90_full$Akteur=="SPS"])
dataCheck<-data90 %>% 
  filter(Seite=="Grünliberale Basel-Stadt"|Seite=="Grünliberale") %>% 
  arrange(Seite,Kategorie)

dataCheck_full<-dataCheck %>% 
  group_by(Akteur,Akteur_fr,Akteur_type,Akteur_type_fr) %>% 
  tidyr::complete(tidyr::nesting(Zielgruppe,Zielgruppe_de,Zielgruppe_fr,Ausgeschlossen,Kategorie,Kategorie_det), tidyr::nesting(Seite,CHF,Datum),
                  fill=list(Ausgaben_pct=0)) %>% 
  ungroup()

dataCheck_full<-dataCheck_full %>% 
  arrange(Seite,Kategorie)

##correct spelling
# table(data90_full$Akteur_type_fr,useNA="always")
# data90_full<-data90_full %>% 
#   mutate(Akteur_type=recode(Akteur_type,"Partei"="Parteien","Interessengruppe"="Interessengruppen"))
# data90_full<-data90_full %>% 
#   mutate(Akteur_type_fr=recode(Akteur_type_fr,"Parti"="Partis","Groupe d'intérêt"="Groupes d'intérêt"))


# 2.2 Detailed targeting ------------------------------------------------------
#Detailed targeing includes a) INTERESTS and b) DEMOGRAPHICS and BEHAVIORS

target_det<-data90_full %>%
  filter(Kategorie=="detailed") 

target_det<-target_det %>% 
  mutate(Ausgaben_abs=Ausgaben_pct*CHF)

target_det<-target_det %>% 
  mutate(Ausgeschlossen_fr=recode(Ausgeschlossen,"Eingeschlossen"="Inclus","Ausgeschlossen"="Exclu"))

# ##add French names (again )
# target_det<-target_det %>% 
#   mutate(Akteur_fr = case_when(
#     Akteur=="SPS" ~ "PS",
#     Akteur=="GPS" ~ "PES",
#     Akteur=="GLP" ~ "PVL",
#     Akteur=="EVP" ~ "PEV",
#     Akteur=="Mitte" ~ "Centre",
#     Akteur=="FDP" ~ "PLR",
#     Akteur=="SVP" ~ "UDC",
#     Akteur=="GV" ~ "UAM",
#     Akteur=="AV" ~ "UP",
#     Akteur=="ES" ~ "ES",
#     Akteur=="BV" ~ "UdP",
#     Akteur=="GB" ~ "US",
#     Akteur=="IGW" ~ "IGW"
#   ))
# 
# #assure order also for french names
# target_det<-target_det %>% 
#   mutate(Akteur_fr=factor(Akteur_fr,levels=unique(target_det$Akteur_fr[order(target_det$Akteur)])))

#cut long names 
#for the top 20 interest groups
x<-target_det %>%
  filter(Kategorie_det=="INTERESTS") %>%
  group_by(Akteur,Zielgruppe,Zielgruppe_de,Zielgruppe_fr,Ausgeschlossen) %>%
  summarise(value=weighted.mean(Ausgaben_pct,CHF))
x<-x %>%
  group_by(Akteur) %>%
  arrange(desc(value)) %>%
  slice(1:20) %>%
  ungroup()
x<-x %>% 
  arrange(-nchar(Zielgruppe_de))
x$n<-nchar(x$Zielgruppe_de)

target_det<-target_det %>% 
  mutate(Zielgruppe_de=recode(Zielgruppe_de,"Single-family detached home"="Einfamilienhaus",
         "Internationale Menschenrechtsvorschriften"="Menschenrechte"))

x<-x %>% 
  arrange(-nchar(Zielgruppe_fr))
x$n<-nchar(x$Zielgruppe_fr)

target_det<-target_det %>% 
  mutate(Zielgruppe_fr=recode(Zielgruppe_fr,"Droit international des droits de l'homme"="Droits de l'homme"))

#for the top 10 dmographics and behaviour groups
x<-target_det %>%
  filter(Kategorie_det=="DEMOGRAPHICS"|Kategorie_det=="BEHAVIORS") %>%
  group_by(Akteur,Zielgruppe,Zielgruppe_de,Zielgruppe_fr,Ausgeschlossen) %>%
  summarise(value=weighted.mean(Ausgaben_pct,CHF))
x<-x %>%
  group_by(Akteur) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  ungroup()
x<-x %>% 
  arrange(-nchar(Zielgruppe_de))
x$n<-nchar(x$Zielgruppe_de)

# target_det<-target_det %>% 
#   mutate(Zielgruppe_de=recode(Zielgruppe_de,"Landwirtschaft, Fischerei und Forstwirtschaft"="Landwirtschaft, Fischerei",
#                               "Mittlere Business-to-Business-Unternehmen "="Mittlere B2B Unternehmen","Eltern mit Kindern im frühen Schulalter "="Eltern (Kinder im Schulalter)",
#                               "Lebens-, Natur- und Sozialwissenschaften"="Natur-, Sozialwissenschaften",
#                               "Krankenschwester in der Intensivpflege"="Krankenschwester",
#                               "Gemeinschaftliche und soziale Dienste"="Soziale Dienste",
#                               "Kunst, Unterhaltung, Sport und Medien"="Kunst, Sport, Medien",
#                               "Entscheidungsträger in der Wirtschaft"="Führungskraft (Wirtschaft)",
#                               "Krankenschwester in der Notaufnahme"="Krankenschwester",
#                               "Chirurgische Krankenschwester"="Krankenschwester"))

x<-x %>% 
  arrange(-nchar(Zielgruppe_fr))
x$n<-nchar(x$Zielgruppe_fr)

target_det<-target_det %>% 
  mutate(Zielgruppe_fr=recode(Zielgruppe_fr,"Service de l'accueil du public"="Service public"))

#correct education
# target_det<-target_det %>% 
#   mutate(Zielgruppe_de=recode(Zielgruppe_de,"Einige Hochschulen"="Hochschulerfahrung",
#                               "Im College"="Im Studium",
#                               "Einiges an der Schule"="Nachdiplomstudiumerfahrung",
#                               "In der Graduiertenschule"="Im Nachdiplomstudium"
#                               ))
# 
# 
# target_det<-target_det %>% 
#   mutate(Zielgruppe_fr=recode(Zielgruppe_fr,"Diplôme d'études supérieures"="Expérience universitaire",
#                               "Au collège"="À l'université",
#                               "Quelques études supérieures"="Expérience d'études postgrades",	
#                               "À l'école doctorale"="En études postgrades",
#                               "Collège"="Université"))

##correct NPO 
# target_det<-target_det %>% 
#   mutate(Zielgruppe_de=case_when(
#     Zielgruppe=="Nonprofit organization" ~ "Non-Profit-Organisation",
#     TRUE ~ Zielgruppe_de ))
# 
# #correct health care
# target_det<-target_det %>% 
#   mutate(Zielgruppe_de=recode(Zielgruppe_de,"Gesundheitspflege"="Gesundheitswesen",
#                               "Wohltätigkeit und Ursachen"="Wohltätigkeit"))
# 
# target_det<-target_det %>% 
#   mutate(Zielgruppe_fr=recode(Zielgruppe_fr,"Charité et causes"="Charité",))


# . check sum and pages ---------------------------------------------------------------

##check sum (compare to website data for the three months)
sum_control_det<-target_det %>% 
  filter(Zielgruppe=="Politics"&Ausgeschlossen=="Eingeschlossen"&Datum==date) %>% 
  group_by(Akteur) %>% 
  summarise(Sum=sum(CHF))

#some diff regarding GV but also present in overall data90
sum_control_90<-data90 %>%
  filter(Kategorie=="gender"&Zielgruppe=="Men") %>%
  group_by(Akteur) %>% 
  summarise(sum=sum(CHF))

##check that all pages are included
FDP_det<-target_det %>% 
  filter(Kategorie_det=="INTERESTS") %>% 
  filter(Akteur=="FDP"&Zielgruppe=="Politics") 
FDP_det_old<-target_det_old %>% 
  filter(Kategorie_det=="INTERESTS") %>% 
  filter(Akteur=="FDP"&Zielgruppe=="Politics") 
length(unique(FDP_det$Seite))
length(unique(data90_full$Seite[data90_full$Akteur=="FDP"]))

table(target_det$Akteur_type,useNA="always")


# . combine databases -------------------------------------------------------

target_det_new<-target_det

##get old version - either the newest old one or by date
#load("output/targeting/detailed/target_det_2023-04-01.rdata")
df_target_det <- file.info(list.files("output/targeting/detailed", full.names = T))
doc_name_target_det<-rownames(df_target_det)[which.max(df_target_det$mtime)]
load(doc_name_target_det)

target_det<-rbind(target_det,target_det_new)

#make sure all combinations that exist at one datapoint exist for all datapoints
target_det<-target_det %>% 
  group_by(Akteur,Akteur_fr,Akteur_type,Akteur_type_fr) %>% 
  tidyr::complete(tidyr::nesting(Zielgruppe,Zielgruppe_de,Zielgruppe_fr,Ausgeschlossen,Ausgeschlossen_fr,Kategorie,Kategorie_det), tidyr::nesting(Seite,CHF,Datum),
                  fill=list(Ausgaben_pct=0)) %>% 
  ungroup()

##check sum (compare to website data for the three months)
sum_control_det_all<-target_det %>% 
  filter(Zielgruppe=="Politics"&Ausgeschlossen=="Eingeschlossen") %>% 
  group_by(Akteur) %>% 
  summarise(Sum=sum(CHF))

##correct solar power vs. solar energy (same german translation)
target_det<-target_det %>% 
  mutate(Zielgruppe_de=ifelse(Zielgruppe=="Solar power","Solarstrom",Zielgruppe_de)) %>% 
  mutate(Zielgruppe_fr=ifelse(Zielgruppe=="Solar power","Courant solaire",Zielgruppe_fr)) 
unique(target_det$Zielgruppe_fr[target_det$Zielgruppe=="Solar power"])
unique(target_det$Zielgruppe_de[target_det$Zielgruppe=="Solar power"])

save(target_det,file=paste0("output/targeting/detailed/target_det_",date,".rdata"))



# #check
#table(target_det$Akteur,target_det$Akteur_fr,useNA = "always")
x<-target_det %>%
  filter(Kategorie_det=="INTERESTS") %>%
  group_by(Akteur,Zielgruppe,Zielgruppe_de,Zielgruppe_fr,Ausgeschlossen) %>%
  summarise(value=weighted.mean(Ausgaben_pct,CHF))

x<-x %>%
  group_by(Akteur) %>%
  arrange(desc(value)) %>%
  slice(1:20) %>%
  ungroup()

x<-x %>% 
  arrange(-nchar(Zielgruppe_de))



# 2.3 demographic -------------------------------------------------------------


# . gender ----------------------------------------------------------------

target_gen<-data90_full %>%
  filter(Kategorie=="gender") %>% 
  filter(Zielgruppe!="All")

target_gen<-target_gen %>% 
  mutate(Ausgaben_abs=Ausgaben_pct*CHF)

target_gen<-target_gen %>% 
  mutate(Zielgruppe=recode(Zielgruppe,"Women"="Frauen","Men"="Männer")) %>% 
  mutate(Zielgruppe=factor(Zielgruppe,levels=c("Frauen","Männer")))

target_gen<-target_gen %>% 
  select(-Ausgeschlossen,-Kategorie_det,-Kategorie,-Zielgruppe_de)

target_gen<-target_gen %>% 
  mutate(Zielgruppe_fr=recode(Zielgruppe,"Frauen"="Femmes","Männer"="Hommes"))

#####combine datasets
target_gen_new<-target_gen

##get previous
df_target_gen <- file.info(list.files("output/targeting/gender", full.names = T))
doc_name_target_gen<-rownames(df_target_gen)[which.max(df_target_gen$mtime)]
load(doc_name_target_gen)

target_gen<-rbind(target_gen,target_gen_new)

save(target_gen,file=paste0("output/targeting/gender/target_gen_",date,".rdata"))


# . age -------------------------------------------------------------------

target_age<-data90_full %>%
  filter(Kategorie=="age")

target_age<-target_age %>% 
  mutate(Zielgruppe=recode(Zielgruppe,"65+"="65")) %>% 
  mutate(Zielgruppe=as.numeric(Zielgruppe)) %>% 
  rename("Zielgruppe_j"="Zielgruppe")

target_age<-target_age %>% 
  mutate(Zielgruppe=case_when(
    Zielgruppe_j<=17 ~ "Alter 13-17",
    Zielgruppe_j>=18&Zielgruppe_j<=24 ~ "Alter 18-24",
    Zielgruppe_j>=25&Zielgruppe_j<=34 ~ "Alter 25-34",
    Zielgruppe_j>=35&Zielgruppe_j<=44 ~ "Alter 35-44",
    Zielgruppe_j>=45&Zielgruppe_j<=54 ~ "Alter 45-54",
    Zielgruppe_j>=55&Zielgruppe_j<=64 ~ "Alter 55-64",
    Zielgruppe_j>=65 ~ "Alter 65+"
  )) %>% 
  select(-Zielgruppe_j)

#filer out below 17
target_age<-target_age %>% 
  filter(Zielgruppe!="Alter 13-17")

target_age<-target_age %>% 
  mutate(Ausgaben_abs=Ausgaben_pct*CHF)

target_age<-target_age %>% 
  select(-Ausgeschlossen,-Kategorie_det,-Kategorie,-custom_audience_type,-location_type,-Zielgruppe_de)

target_age<-target_age %>% 
  mutate(Zielgruppe_fr=recode(Zielgruppe,"Alter 18-24"="Âge 18-24","Alter 25-34"="Âge 25-34", "Alter 35-44"="Âge 35-44",
                              "Alter 45-54"="Âge 45-54","Alter 55-64"="Âge 55-64","Alter 65+"="Âge 65+"))

#####combine datasets
target_age_new<-target_age

##get previous
df_target_age <- file.info(list.files("output/targeting/age", full.names = T))
doc_name_target_age<-rownames(df_target_age)[which.max(df_target_age$mtime)]
load(doc_name_target_age)

target_age<-rbind(target_age,target_age_new)

save(target_age,file=paste0("output/targeting/age/target_age_",date,".rdata"))


# 2.4 custom ------------------------------------------------------------------

target_aud<-data90_full %>% 
  filter(Kategorie=="custom_audience"|Kategorie=="lookalike_audience")

#remove customer list custom audinece - is duplicated in custom audience
target_aud<-target_aud %>% 
  filter(Zielgruppe %in% c("Custom audience","Lookalike audience")|is.na(Zielgruppe))

target_aud<-target_aud %>% 
  mutate(Ausgaben_abs=Ausgaben_pct*CHF)

target_aud<-target_aud %>% 
  mutate(Zielgruppe=paste0(Zielgruppe," (",Ausgeschlossen,")"))

target_aud<-target_aud %>% 
  mutate(Zielgruppe=factor(Zielgruppe,levels=c("Custom audience (Eingeschlossen)","Custom audience (Ausgeschlossen)",
                                                         "Lookalike audience (Eingeschlossen)","Lookalike audience (Ausgeschlossen)")))

target_aud<-target_aud %>% 
  select(-Ausgeschlossen,-Kategorie_det,-Kategorie,-custom_audience_type,-location_type,-Zielgruppe_de)

target_aud<-target_aud %>% 
  mutate(Zielgruppe_fr=recode(Zielgruppe,"Custom audience (Eingeschlossen)"="Custom audience (inclus)",
                              "Custom audience (Ausgeschlossen)"="Custom audience (exclu)",
                              "Lookalike audience (Eingeschlossen)"="Lookalike audience (inclus)",
                              "Lookalike audience (Ausgeschlossen)"="Lookalike audience (exclu)"))


##check sum (compare to website data for the three months and compare to sum_control_det)
sum_control_aud<-target_aud %>% 
  filter(Zielgruppe=="Lookalike audience (Eingeschlossen)")%>% 
  group_by(Akteur) %>% 
  summarise(Sum=sum(CHF))

table(data90_full$Akteur[data90_full$Zielgruppe=="Men"]) #how many pages per Akteur in overall data
table(target_aud$Zielgruppe,target_aud$Akteur) #how many pages in target_aud

##combine datasets
target_aud_new<-target_aud

##get previous
#load("output/targeting/audience/target_aud_2023-04-01.rdata")

df_target_aud <- file.info(list.files("output/targeting/audience", full.names = T))
doc_name_target_aud<-rownames(df_target_aud)[which.max(df_target_aud$mtime)]
load(doc_name_target_aud)

target_aud<-rbind(target_aud,target_aud_new)

save(target_aud,file=paste0("output/targeting/audience/target_aud_",date,".rdata"))



