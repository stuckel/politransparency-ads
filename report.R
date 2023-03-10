
##report data 

#1. download 7 days report and put in report folder 
#2. check thisweek_all if all party pages have been identified as party - if not change Fidentify.R


library(dplyr)
library(stringr)
library(lubridate)



#########################################################prepare report data

rm(list=ls())
source("Fidentify.R")

#load most recent weekly report data
df <- file.info(list.files("input/report/zip", full.names = T))
rownames(df)[which.max(df$mtime)]
data<-unzip(rownames(df)[which.max(df$mtime)],list=T)$Name
doc_name <- grep('advertisers', unzip(rownames(df)[which.max(df$mtime)],list=T)$Name, 
                 ignore.case=TRUE, value=TRUE)
unzip(rownames(df)[which.max(df$mtime)], files=doc_name,exdir="input/report")
thisweek<-read.csv(paste0("input/report/",doc_name), encoding="UTF-8")


thisweek<-thisweek %>% 
  rename("disclaimer"="Disclaimer")

#identify actors
thisweek<-Fidentify(thisweek)

#add date
thisweek$date<-str_extract(doc_name,"\\d+-\\d+-\\d+")

#copy of all ads
thisweek_all<-thisweek

#filter valid 
thisweek<-thisweek %>% 
  filter(!is.na(party))  #|!is.na(ig)

#filter out smaller than 100 by forcing as.numeric
thisweek<-thisweek %>%
  mutate(amount=as.numeric(Amount.spent..CHF.)) %>% 
  filter(!is.na(amount))


#make sure there is a zero observation for all actors in the database that dont have an observation that week
#first week
if (nrow(file.info(list.files("input/report/zip", full.names = T)))==1) {
  
  party<-c("SPS","GPS","GLP","Mitte","FDP","SVP")
  base<-as.data.frame(party)
  base$date<-thisweek$date[1]
  
  thisweek<-merge(base,thisweek,by=c("party","date"),all.x=TRUE,all.y=T)
  
  
} else {
  
  load(paste0("output/report/parties_weekly_",ymd(thisweek$date[1])-7,".rdata"))
  
  party<-unique(parties_weekly$Akteur)
  base<-as.data.frame(party)
  base$date<-thisweek$date[1]
  
  thisweek<-merge(base,thisweek,by=c("party","date"),all.x=TRUE,all.y=T)
  
}

#in those cases where there was no ads, and amount is NA
thisweek<-thisweek %>% 
  mutate(amount=ifelse(is.na(amount),0,amount))

#assure order of parties, IG order not specified
partyorder<-c("SPS","GPS","GLP","EVP","Mitte","FDP","SVP")
thisweek<-thisweek %>% 
  mutate(party=factor(party,levels=c(partyorder,setdiff(unique(thisweek$party), partyorder)))) 

#transform date
thisweek<-thisweek %>% 
  mutate(date=ymd(date))

#select variables wanted for output
thisweek<-thisweek %>% 
  select(-Page.ID,-Amount.spent..CHF.) %>% 
  rename("Akteur"="party","Datum"="date","Seite"="Page.name","Inserent"="disclaimer",
         "Anzahl"="Number.of.ads.in.Library","CHF"="amount") %>% 
  select("Akteur","Inserent","Seite","Datum","Anzahl","CHF")


##add French names 
thisweek<-thisweek %>% 
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
thisweek<-thisweek %>% 
  mutate(Akteur_fr=factor(Akteur_fr,levels=unique(thisweek$Akteur_fr[order(thisweek$Akteur)])))


##add color 
thisweek<-thisweek %>% 
  mutate(Akteur_col = case_when(
    Akteur=="SPS" ~ "#F0554D",
    Akteur=="GPS" ~ "#83B546",
    Akteur=="GLP" ~ "#C4C43D",
    Akteur=="EVP" ~ "#DEAB28",
    Akteur=="Mitte" ~ "#D6862B",
    Akteur=="FDP" ~ "#356FB3",
    Akteur=="SVP" ~ "#4B8A3E",
    Akteur=="GV" ~ "#94989c",
    Akteur=="AV" ~ "#0262ab",
    Akteur=="ES" ~ "#ef7b0a",
    Akteur=="BV" ~ "#9ea600",
    Akteur=="GB" ~ "#e2001a",
    Akteur=="IGW" ~ "#e4127f"
  ))

#combine with existing dataset (loaded previously) and save 
if (nrow(file.info(list.files("input/report/zip", full.names = T)))==1) {

  parties_weekly<-thisweek
  save(parties_weekly,file=paste0("output/report/parties_weekly_",thisweek$Datum[1],".rdata"))
  
  
} else {
  
  parties_weekly<-rbind(parties_weekly,thisweek)
  save(parties_weekly,file=paste0("output/report/parties_weekly_",thisweek$Datum[1],".rdata"))
  
  
}

