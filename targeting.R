
#This data is only collected every 90 days - new datasets have to be manually joined together. 

#install.packages("metatargetr")
#remotes::install_github("favstats/metatargetr")
library(metatargetr)
library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)


# get data ----------------------------------------------------------------

###get relevant page ids based on report data
rm(list=ls())
source("Fidentify.R")
options(scipen=999)

report<-read.csv("input/targeting/FacebookAdLibraryReport_2023-04-01_CH_last_90_days_advertisers.csv",encoding="UTF-8")
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

save(data90,file="output/targeting/data90_05042023.rdata")


# Clean -------------------------------------------------------------------

#1) get report data first (see above)
rm(list=ls())
source("Fidentify.R")
options(scipen=999)

report<-read.csv("input/targeting/FacebookAdLibraryReport_2023-04-01_CH_last_90_days_advertisers.csv",encoding="UTF-8")
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
load("output/targeting/data90_05042023.rdata")

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
    Akteur %in% parties ~ "Partei",
    TRUE ~ "Interessengruppe"
  ))

data90<-data90 %>% 
  mutate(Akteur_type_fr=recode(Akteur_type,"Partei"="Parti","Interessengruppe"="Groupe d'intérêt"))

data90<-data90 %>% 
  mutate(Akteur_type=factor(Akteur_type,levels=c("Partei","Interessengruppe")))

data90<-data90 %>% 
  mutate(Akteur_type_fr=factor(Akteur_type_fr,levels=c("Parti","Groupe d'intérêt")))

data90<-data90 %>% 
  mutate_at("Ausgeschlossen",as.character) %>% 
  mutate(Ausgeschlossen=recode(Ausgeschlossen,"FALSE"="Eingeschlossen","TRUE"="Ausgeschlossen")) %>% 
  mutate(Ausgeschlossen=factor(Ausgeschlossen,levels=c("Eingeschlossen","Ausgeschlossen")))

# translate ---------------------------------------------------------------


#detect language
# data90$language<-textcat::textcat(data90$Zielgruppe)
# table(data90$language)

translate<-data90 %>%
  filter(Kategorie=="detailed") %>% 
  distinct(Zielgruppe) 

#translate
library(deeplr)

my_key<-c("e701286f-0d7b-8a39-7243-e01c0a80f30c") #will need to use new key because changed to free api again

#check
# available_languages2(my_key)
# usage2(my_key)

##german
translate <-translate %>%
  mutate(Zielgruppe_de=translate(text=Zielgruppe,target_lang="DE",source_lang="EN",auth_key=my_key))

translate<-translate %>% 
  rename("Zielgruppe_de"="Zielgrupppe_de")

translate_sec<-translate


#shorten long names 
long<-translate %>% 
  filter(nchar(Zielgruppe_de)>25)

long<-long %>% 
  mutate(Zielgruppe_short=sub('\\(.*\\)','',Zielgruppe_de)) %>% 
  select(-Zielgruppe_de)

long<-long %>% 
  mutate(Zielgruppe_short=recode(Zielgruppe_short,"Gesundheitswesen und medizinische Dienstleistungen"="Gesundheitswesen",
                              "Zürcher Hochschule für Angewandte Wissenschaften/ZHAW"="ZHAW"))

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
  mutate(Zielgruppe_fr=translate(text=Zielgruppe,target_lang="FR",source_lang="EN",auth_key=my_key))

translate_fr_sec<-translate_fr


#shorten long names 
long<-translate_fr %>% 
  filter(nchar(Zielgruppe_fr)>25)

long<-long %>% 
  mutate(Zielgruppe_short=sub('\\(.*\\)','',Zielgruppe_fr)) %>% 
  select(-Zielgruppe_fr,-Zielgruppe_de)

long<-long %>% 
  mutate(Zielgruppe_short=recode(Zielgruppe_short,"UNHCR, l'agence des Nations unies pour les réfugiés"="UNHCR",
                                 "Université des sciences appliquées de Zurich/ZHAW"="ZHAW",
                                 "Initiative pour la sécurité énergétique et environnementale"="Initiative pour la sécurité énergétique et env."))

translate_fr<-merge(translate_fr,long,by=c("Zielgruppe"),all=T)

translate_fr<-translate_fr %>% 
  mutate(Zielgruppe_fr=case_when(
    !is.na(Zielgruppe_short) ~ Zielgruppe_short,
    TRUE ~ Zielgruppe_fr
  ))

translate_fr<-translate_fr %>% 
  select(-Zielgruppe_short)


#merge back together
data90<-merge(data90,translate_fr,by=c("Zielgruppe"),all=TRUE)


# Detailed Targeting ------------------------------------------------------

#################Detailliertes Targeting

##interests
target_det<-data90 %>%
  filter(Kategorie=="detailed") 



#within each group create all possible combinations of 
target_det<-target_det %>% 
  group_by(Akteur,Akteur_fr) %>% 
  tidyr::complete(tidyr::nesting(Zielgruppe,Zielgruppe_de,Zielgruppe_fr,Ausgeschlossen,Kategorie_det), tidyr::nesting(Seite,CHF,Datum),
                  fill=list(Ausgaben_pct=0)) %>% 
  ungroup()

target_det<-target_det %>% 
  mutate(Ausgaben_abs=Ausgaben_pct*CHF)


target_det<-target_det %>% 
  mutate(Ausgeschlossen_fr=recode(Ausgeschlossen,"Eingeschlossen"="Inclus","Ausgeschlossen"="Exclu"))


##add French names (again )
target_det<-target_det %>% 
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
target_det<-target_det %>% 
  mutate(Akteur_fr=factor(Akteur_fr,levels=unique(target_det$Akteur_fr[order(target_det$Akteur)])))

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
  mutate(Zielgruppe_de=recode(Zielgruppe_de,"Organisation zur Erhaltung der Gesundheit"="Gesundheitsorganisation",
         "Kleine und mittlere Unternehmen"="KMU","Nichtstaatliche Organisation"="NGO",
         "Camping und Caravaning Club"="Camping Club"))

x<-x %>% 
  arrange(-nchar(Zielgruppe_fr))
x$n<-nchar(x$Zielgruppe_fr)

target_det<-target_det %>% 
  mutate(Zielgruppe_fr=recode(Zielgruppe_fr,"Organisme de gestion de la santé"="Organisation de santé",
                              "Club de camping et de caravaning"="Club de camping","Organisation à but non lucratif"="Org. à but non lucratif",
                              "Politique et questions sociales"="Politique/quest. sociales",
                              "Petites et moyennes entreprises"="PME","Organisation non gouvernementale"="ONG"))

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

target_det<-target_det %>% 
  mutate(Zielgruppe_de=recode(Zielgruppe_de,"Landwirtschaft, Fischerei und Forstwirtschaft"="Landwirtschaft, Fischerei",
                              "Mittlere Business-to-Business-Unternehmen "="Mittlere B2B Unternehmen","Eltern mit Kindern im frühen Schulalter "="Eltern (Kinder im Schulalter)",
                              "Lebens-, Natur- und Sozialwissenschaften"="Natur-, Sozialwissenschaften",
                              "Krankenschwester in der Intensivpflege"="Krankenschwester",
                              "Gemeinschaftliche und soziale Dienste"="Soziale Dienste",
                              "Kunst, Unterhaltung, Sport und Medien"="Kunst, Sport, Medien",
                              "Entscheidungsträger in der Wirtschaft"="Führungskraft (Wirtschaft)",
                              "Krankenschwester in der Notaufnahme"="Krankenschwester",
                              "Chirurgische Krankenschwester"="Krankenschwester"))

x<-x %>% 
  arrange(-nchar(Zielgruppe_fr))
x$n<-nchar(x$Zielgruppe_fr)

target_det<-target_det %>% 
  mutate(Zielgruppe_fr=recode(Zielgruppe_fr,"Sciences de la vie, sciences physiques et sociales"="Sciences physiques et sociales",
                              "Parents d'enfants en âge de fréquenter l'école "="Parents (enfants âge scolaire)","Propriétaires de petites entreprises"="Petits entrepreneurs",
                              "Diplômé de l'enseignement supérieur"="Diplômé universitaire",
                              "Agriculture, pêche et sylviculture"="Agriculture, pêche","Arts, spectacles, sports et médias"="Arts, sports, médias",
                              "Services sociaux et communautaires"="Services sociaux",
                              "Entreprises moyennes interentreprises "="Entreprises moyennes B2B"))

#correct education
target_det<-target_det %>% 
  mutate(Zielgruppe_de=recode(Zielgruppe_de,"Einige Hochschulen"="Hochschulerfahrung",
                              "Im College"="Im Studium",
                              "Einiges an der Schule"="Nachdiplomstudiumerfahrung",
                              "In der Graduiertenschule"="Im Nachdiplomstudium"
                              ))


target_det<-target_det %>% 
  mutate(Zielgruppe_fr=recode(Zielgruppe_fr,"Diplôme d'études supérieures"="Expérience universitaire",
                              "Au collège"="À l'université",
                              "Quelques études supérieures"="Expérience d'études postgrades",	
                              "À l'école doctorale"="En études postgrades",
                              "Collège"="Université"))

##correct NPO 
target_det<-target_det %>% 
  mutate(Zielgruppe_de=case_when(
    Zielgruppe=="Nonprofit organization" ~ "Non-Profit-Organisation",
    TRUE ~ Zielgruppe_de ))

#correct health care
target_det<-target_det %>% 
  mutate(Zielgruppe_de=recode(Zielgruppe_de,"Gesundheitspflege"="Gesundheitswesen",
                              "Wohltätigkeit und Ursachen"="Wohltätigkeit"))

target_det<-target_det %>% 
  mutate(Zielgruppe_fr=recode(Zielgruppe_fr,"Charité et causes"="Charité",))

save(target_det,file=paste0("output/targeting/detailed/target_det_",target_det$Datum[1],".rdata"))


#load("output/targeting/detailed/target_det_2023-04-01.rdata")


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



# demographic -------------------------------------------------------------

##gender

target_gen<-data90 %>%
  filter(Kategorie=="gender") %>% 
  filter(Zielgruppe!="All")

target_gen<-target_gen %>% 
  mutate(Ausgaben_abs=Ausgaben_pct*CHF)

target_gen<-target_gen %>% 
  mutate(Zielgruppe=recode(Zielgruppe,"Women"="Frauen","Men"="Männer")) %>% 
  mutate(Zielgruppe=factor(Zielgruppe,levels=c("Frauen","Männer")))

target_gen<-target_gen %>% 
  select(-Ausgeschlossen,-Kategorie_det,-Kategorie)

target_gen<-target_gen %>% 
  mutate(Zielgruppe_fr=recode(Zielgruppe,"Frauen"="Femmes","Männer"="Hommes"))

save(target_gen,file=paste0("output/targeting/gender/target_gen_",target_gen$Datum[1],".rdata"))

#load("output/targeting/gender/target_gen_2023-04-01.rdata")

####age

target_age<-data90 %>%
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
  select(-Ausgeschlossen,-Kategorie_det,-Kategorie,-custom_audience_type,-location_type)

target_age<-target_age %>% 
  mutate(Zielgruppe_fr=recode(Zielgruppe,"Alter 18-24"="Âge 18-24","Alter 25-34"="Âge 25-34", "Alter 35-44"="Âge 35-44",
                              "Alter 45-54"="Âge 45-54","Alter 55-64"="Âge 55-64","Alter 65+"="Âge 65+"))

save(target_age,file=paste0("output/targeting/age/target_age_",target_age$Datum[1],".rdata"))

#load("output/targeting/age/target_age_2023-04-01.rdata")


# custom ------------------------------------------------------------------

target_aud<-data90 %>% 
  filter(Kategorie=="custom_audience"|Kategorie=="lookalike_audience")

#remove customer list custom audinece - is duplicated in custom audience
target_aud<-target_aud %>% 
  filter(Zielgruppe!="Customer list custom audience")

#create all possible combinations of Seite + Zielgruppe/Ausgeschlossen (nesting: only finds combinations already present in the data)
target_aud<-target_aud %>% 
  group_by(Akteur,Akteur_type,Akteur_fr,Akteur_type_fr) %>% 
  tidyr::complete(tidyr::nesting(Zielgruppe,Ausgeschlossen), tidyr::nesting(Seite,CHF,Datum),
                  fill=list(Ausgaben_pct=0)) %>% 
  ungroup() %>% 
  arrange(Seite)

target_aud<-target_aud %>% 
  mutate(Ausgaben_abs=Ausgaben_pct*CHF)

target_aud<-target_aud %>% 
  mutate(Zielgruppe=paste0(Zielgruppe," (",Ausgeschlossen,")"))

target_aud<-target_aud %>% 
  mutate(Zielgruppe=factor(Zielgruppe,levels=c("Custom audience (Eingeschlossen)","Custom audience (Ausgeschlossen)",
                                                         "Lookalike audience (Eingeschlossen)","Lookalike audience (Ausgeschlossen)")))

target_aud<-target_aud %>% 
  select(-Ausgeschlossen,-Kategorie_det,-Kategorie,-custom_audience_type,-location_type)

target_aud<-target_aud %>% 
  mutate(Zielgruppe_fr=recode(Zielgruppe,"Custom audience (Eingeschlossen)"="Custom audience (inclus)",
                              "Custom audience (Ausgeschlossen)"="Custom audience (exclu)",
                              "Lookalike audience (Eingeschlossen)"="Lookalike audience (inclus)",
                              "Lookalike audience (Ausgeschlossen)"="Lookalike audience (exclu)"))

save(target_aud,file=paste0("output/targeting/audience/target_aud_",target_aud$Datum[1],".rdata"))

#load("output/targeting/audience/target_aud_2023-04-01.rdata")


