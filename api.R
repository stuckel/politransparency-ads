
####API data

#steps 
#0. restart R 
#1. get facebook token: https://developers.facebook.com/tools/explorer/ and include it below
#3. check quickly again if all relevant actors have been identified (sel.CH.all)
#4. check line 168 - if there are NAs
#5. run rest of code 


# load packages -----------------------------------------------------------

pacman::p_load(plyr,dplyr, tidyr, lubridate, jsonlite, httr,stringr,reshape2,ggplot2,sp,sf)

rm(list=ls())
source("Fidentify.R") # function to identify relevant actors

# Select Pages -----------------------------------------------------------------

#get newest report
df <- file.info(list.files("input/report/zip", full.names = T))
rownames(df)[which.max(df$mtime)]
data<-unzip(rownames(df)[which.max(df$mtime)],list=T)$Name
doc_name <- grep('advertisers', unzip(rownames(df)[which.max(df$mtime)],list=T)$Name, 
                 ignore.case=TRUE, value=TRUE)
unzip(rownames(df)[which.max(df$mtime)], files=doc_name,exdir="input/report")
report.CH<-read.csv(paste0("input/report/",doc_name), encoding="UTF-8")

report.CH<-report.CH %>% 
  rename("disclaimer"="Disclaimer")

#select relevant actors
sel.CH<-Fidentify(report.CH)

#clean data
sel.CH<-sel.CH %>% 
  mutate(number=as.numeric(as.character(Number.of.ads.in.Library))) %>% 
  mutate(amount=as.numeric(as.character(Amount.spent..CHF.))) %>% 
  rename(.,"id"="Page.ID","page"="Page.name") %>% 
  select(-Number.of.ads.in.Library,-Amount.spent..CHF.)

#copy of all ads
sel.CH.all<-sel.CH

#keep valid actors
sel.CH<-sel.CH %>% 
  filter(!is.na(party)&!is.na(amount)) 

#one observation per actor
sel.CH<-sel.CH %>% 
  distinct(id,.keep_all = TRUE)

sel.CH<-sel.CH %>% 
  arrange(desc(party)) 

#identify date range
date<-str_extract(doc_name,"\\d+-\\d+-\\d+")
date<-ymd(date)
date_min<-date-6

# Get KEY -----------------------------------------------------------------

#To access the API one needs to retrieve the access token from https://developers.facebook.com/tools/explorer/, user token
url  <- "https://graph.facebook.com"
path <- "v3.3/ads_archive"
token.f<-"EAASWt05gcD4BACBZAvvlXR8nCSwZB7YcZCvvdXNkDjZAVvatrFU4oXjQuH5ZBWB3y78JFXZBj7mof57RvP0p0XUUOXCp5a18yFXHeD31lpVvpswHoqQD6cgsauwd5XR0NzpWpLMAV3bbdE6YRWUGKGmvh0jVgmVB5CosvNEUKUAom8ZCNO5lHOUw65PL9FYenet4N5eKcbFCDjUbZBnbZAvbGZCzGtmFHAyYhxBes6bZAEVLvETlwdsPHkWacvRsbp13f8ZD"
options(stringsAsFactors = FALSE)
options(scipen=999)


# Get data ----------------------------------------------------------------


#define facebook function
#changes in variables: 
#new: ad_creative_bodies (previous: ad_creative_body)
#new: delivery_by_region (previous: region_distribution)
#new: ad_creative_link_captions (previous: caption)
#new: ad_creative_link_descriptions
#dates are inclusive the day

fb<-function(x) {GET(url=url,path=path,query=list(access_token=token.f,ad_reached_countries="CH",search_page_ids=x,ad_delivery_date_min=date_min,ad_delivery_date_max=date,fields="page_name,ad_delivery_start_time,ad_delivery_stop_time,
                                                  ad_creative_bodies,demographic_distribution,delivery_by_region,impressions,spend,ad_snapshot_url,ad_creative_link_captions,ad_creative_link_descriptions,ad_creative_link_titles,publisher_platforms,currency"))
}

#repeat function for every ID
CNT<-lapply(sel.CH$id,fb)

#prepare result variable
CH<-NULL

#loop over each page ID 
for(i in 1:length(sel.CH$id)) {
  
  #STEP 1: Prepare first 25 ads 
  ads<-rawToChar(CNT[[i]]$content) #Parsing it to JSON
  ADS <- fromJSON(ads) #transform string into a list
  ads.data<-ADS$data #  #extract data contained in list
  
  if(nchar(ads)<=11){next} ##remove ids, for which there is nothing
  
  #Step 2: based on $paging, access and prepare rest of ads 
  while(ADS$paging$'next'!=""){
    
    nextads <- GET(ADS$paging$'next')
    ads <- rawToChar(nextads$content) 
    ADS <- fromJSON(ads)
    
    if (is.null(ADS$paging)) {                #quit loop once paging is null and  add all together into one data frame
      if (is.null(CH)) { 
        CH<-ads.data
      } else {
        CH<-bind_rows(CH,ads.data)
      }
      break}
    
    ads.data2 <- ADS$data
    ads.data<-bind_rows(ads.data,ads.data2)     #add  data to existing data frame and repeat
    
  }}

rm(list=setdiff(ls(), c("CH","sel.CH","sel.CH.all","report.CH","date")))

#remove Ads without info on audiences (those ads  - also have geography = null and an investment <100)
CH<-CH %>% 
    drop_na(demographic_distribution)

#turn lists (demography,  region, platforms) and dataframes (impressions and spend) into normal columns
#impressions
CH$impressions_low<-CH$impressions$lower_bound
CH$impressions_upp<-CH$impressions$upper_bound

#spend
CH$spend_low<-CH$spend$lower_bound
CH$spend_upp<-CH$spend$upper_bound
CH<-CH %>% 
  select(-spend,-impressions)


#demographic_distribution
CH<-CH %>% 
  unnest(c(demographic_distribution)) #keep_empty=T would be an option 
CH$genderAge<-with(CH,paste0(gender,age))
CH$age<-NULL
CH$gender<-NULL
CH<-pivot_wider(CH,names_from=genderAge,values_from=percentage)

#region_distribution
CH<-CH %>% 
  unnest(c(delivery_by_region)) 

CH<-pivot_wider(CH,names_from=region,values_from=percentage)

#platform
CH$platform <- sapply(CH$publisher_platforms, paste, collapse=",")
CH<-CH %>% 
  select(-publisher_platforms)

#create party variable
source("Fidentify.R")
CH<-Fidentify_page(CH)

#check if all pages have been identified
table(CH$party,useNA = "always")
fehler<-CH %>% 
  filter(is.na(party))

CH<-CH %>% 
  rename("date_start"="ad_delivery_start_time","date_stop"="ad_delivery_stop_time")


##get text data
CH<-CH %>% 
  unnest_wider(ad_creative_bodies)  #in case there are multiple variations of an ad, new columns are created
CH<-CH %>% 
  rename("Text"="...1")  #only the first text variant is retained

CH<-CH %>% 
  select(-ad_creative_link_descriptions,-ad_creative_link_titles,-ad_creative_link_captions)

#create spend average
CH<-CH %>% 
  mutate_at(vars(spend_low,spend_upp,impressions_low,impressions_upp),as.numeric)

CH<-CH %>% 
  rowwise() %>% 
  mutate(spend_av=mean(c(spend_low,spend_upp)),impressions_av=mean(c(impressions_low,impressions_upp)))

#create male and female variables
CH<-CH %>% 
  mutate_at(vars(starts_with("female"),starts_with("male")),as.numeric) %>% 
  mutate_at(vars(starts_with("female"),starts_with("male")),funs(ifelse(is.na(.),0,.)))

CH<-CH %>% 
  rowwise() %>% 
  mutate(female=sum(c_across(starts_with("female"))),
         male=sum(c_across(starts_with("male"))))

#control
CH<-CH %>% 
   rowwise() %>% 
   mutate(gender=sum(female,male))
head(CH$gender)

#age variable
CH <- CH %>% 
  mutate_at(vars(ends_with("17")|ends_with("24")|ends_with("34")|ends_with("44")|ends_with("54")|ends_with("64")|ends_with("65+")),as.numeric) %>% 
  mutate_at(vars(ends_with("17")|ends_with("24")|ends_with("34")|ends_with("44")|ends_with("54")|ends_with("64")|ends_with("65+")),funs(ifelse(is.na(.),0,.)))

CH<-CH %>% 
  rowwise() %>% 
  mutate(age18_24a=sum(c_across(ends_with("24"))),
         age25_34a=sum(c_across(ends_with("34"))),
         age35_44a=sum(c_across(ends_with("44"))),
         age45_54a=sum(c_across(ends_with("54"))),
         age55_64a=sum(c_across(ends_with("64"))),
         age65a=sum(c_across(ends_with("65+"))))

#control
CH<-CH %>% 
  rowwise() %>% 
  mutate(ageAll=sum(age18_24a,age25_34a,age35_44a,age45_54a,age55_64a,age65a))
head(CH$ageAll)
CH<-CH %>% 
  select(-ageAll,-gender)

CH<-CH %>% 
  select(-matches("\\d$"),-Unknown)

##geo
CH<-CH %>% 
  rename("Basel-Stadt"="Basel-City","Genève"="Canton of Geneva","Glarus"="Canton of Glarus","Nidwalden"="Canton of Nidwalden","Obwalden"="Canton of Obwalden",
         "Sankt Gallen"="Canton of St. Gallen")

#as numeric 
CH<-CH %>% 
  mutate_at(c("Jura","Obwalden","Zug","Vaud","Valais","Uri","Ticino","Thurgau", "Solothurn","Schwyz","Schaffhausen","Sankt Gallen","Nidwalden","Aargau","Neuchâtel",
              "Luzern","Appenzell Innerrhoden","Graubünden","Glarus","Genève","Fribourg","Bern","Basel-Stadt","Basel-Landschaft","Appenzell Ausserrhoden","Zürich"),as.numeric)

CH<-CH %>% 
  ungroup()

##add French names 
CH<-CH %>% 
  mutate(party_fr = case_when(
    party=="SPS" ~ "PS",
    party=="GPS" ~ "PES",
    party=="GLP" ~ "PVL",
    party=="EVP" ~ "PEV",
    party=="Mitte" ~ "Centre",
    party=="FDP" ~ "PLR",
    party=="SVP" ~ "UDC",
    party=="GV" ~ "UAM",
    party=="AV" ~ "UP",
    party=="ES" ~ "ES",
    party=="BV" ~ "UdP",
    party=="GB" ~ "US",
    party=="IGW" ~ "IGW"
      ))

CH<-CH %>% 
  select(id,party,party_fr,page_name,date_start,date_stop,spend_av,Text, female, male,age18_24a,age25_34a,age35_44a,age45_54a,age55_64a,age65a, "Aargau","Appenzell Ausserrhoden","Appenzell Innerrhoden","Basel-Landschaft","Basel-Stadt" ,"Bern", "Fribourg", "Genève","Glarus","Graubünden", "Jura","Luzern","Neuchâtel","Nidwalden","Obwalden","Sankt Gallen","Schaffhausen","Schwyz",
         "Solothurn","Thurgau","Ticino", "Uri","Valais","Vaud" ,"Zug","Zürich")

CH<-CH %>% 
  rename("Akteur"="party","Acteur"="party_fr","Anfang"="date_start","Ende"="date_stop","Seite"="page_name", "Frauen"="female","Männer"="male","CHF"="spend_av","Alter 18-24"="age18_24a","Alter 25-34"="age25_34a", "Alter 35-44"="age35_44a","Alter 45-54"="age45_54a","Alter 55-64"="age55_64a","Alter 65+"="age65a")

CH<-CH %>% 
  mutate(Ende_table=Ende) #with "" become NAs once variable is turned into a date

CH<-CH %>% 
  mutate(Anfang=ymd(Anfang),
         Ende=ymd(Ende),
         Ende_table=ymd(Ende_table)) 

#if there is no end date (ad still running): set date of report as end date
CH<-CH %>% 
  mutate(Ende=case_when(
    is.na(Ende) ~ date,
    TRUE ~ Ende
  ))


# geo data ----------------------------------------------------------------

#geographic focus
#get name of layer to load
# CH_layers<-st_layers("input/API/map/gadm41_CHE.gpkg")
# CH_layers$name

#select geo variables
api_geo<-CH %>% 
  select(id,Seite,Akteur,Acteur,Anfang,Ende,CHF,
         "Aargau","Appenzell Ausserrhoden","Appenzell Innerrhoden","Basel-Landschaft","Basel-Stadt" ,"Bern", "Fribourg", "Genève","Glarus","Graubünden", "Jura","Luzern","Neuchâtel","Nidwalden","Obwalden","Sankt Gallen","Schaffhausen","Schwyz",
         "Solothurn","Thurgau","Ticino", "Uri","Valais","Vaud" ,"Zug","Zürich")
names(api_geo)

#from wide to long
api_geo<-melt(api_geo, id=c("id","Seite","Akteur","Acteur","Anfang","Ende","CHF"))
api_geo<-api_geo %>% 
  mutate(Kanton=as.character(variable))
api_geo<-api_geo %>% 
  arrange(Kanton)

#replace NAs with 0s
api_geo$value[is.na(api_geo$value)]<-0

#preparing geo data (to do once)
# CH_map<-st_read("input/api/map/gadm41_che.gpkg",layer="ADM_ADM_1")
# 
# CH_map<-CH_map %>%
#   mutate(Kanton=recode(NAME_1,"Lucerne"="Luzern"))
# 
# CH_map<-CH_map %>% 
#   select("Kanton","geom")

# api_map<-CH_map
# save(api_map,file="input/api/map/api_map.rdata")


# geo data table data -----------------------------------------------------

api_geo_table<-CH %>% 
  select(id,Akteur,Acteur,Seite,Anfang,Ende,Ende_table,CHF,Text, "Aargau","Appenzell Ausserrhoden","Appenzell Innerrhoden","Basel-Landschaft","Basel-Stadt" ,"Bern", "Fribourg", "Genève","Glarus","Graubünden", "Jura","Luzern","Neuchâtel","Nidwalden","Obwalden","Sankt Gallen","Schaffhausen","Schwyz",
         "Solothurn","Thurgau","Ticino", "Uri","Valais","Vaud" ,"Zug","Zürich")

# api_geo_table<-api_geo_table %>% 
#   mutate(Anfang=ymd(Anfang),
#          Ende=ymd(Ende)) 

api_geo_table<-api_geo_table %>% 
  mutate(CHF=round(CHF,0))

api_geo_table<-api_geo_table %>% 
  mutate_at(vars("Aargau","Appenzell Ausserrhoden","Appenzell Innerrhoden","Basel-Landschaft","Basel-Stadt" ,"Bern", "Fribourg", "Genève","Glarus","Graubünden", "Jura","Luzern","Neuchâtel","Nidwalden","Obwalden","Sankt Gallen","Schaffhausen","Schwyz",
              "Solothurn","Thurgau","Ticino", "Uri","Valais","Vaud" ,"Zug","Zürich"),funs(ifelse(is.na(.),0,.)))


##preparing geo data for the case that there are no observations (to do once)
# api_geo_zero<-st_read("input/api/map/gadm41_che.gpkg",layer="ADM_ADM_1")
# 
# api_geo_zero<-api_geo_zero %>% 
#   mutate(Kanton=recode(NAME_1,"Lucerne"="Luzern"))
# 
# api_geo_zero<-api_geo_zero %>% 
#   mutate(audience_av=0) %>% 
#   select(Kanton,geom,audience_av) %>% 
#   ungroup()
# 
# save(api_geo_zero,file="input/api/map/api_geo_zero.rdata")


# gender data -----------------------------------------------------

api_gender<-CH %>% 
  select(id,Akteur,Acteur,Seite,Anfang,Ende,CHF,Frauen,Männer)

# long format
api_gender<-melt(api_gender, id=c("id","Akteur","Acteur","Seite","Anfang","Ende","CHF"))


# age ---------------------------------------------------------------------

api_age<-CH %>% 
  select(id,Akteur,Acteur,Seite,Anfang,Ende,CHF,"Alter 18-24","Alter 25-34", "Alter 35-44","Alter 45-54","Alter 55-64","Alter 65+")

# long format
api_age<-melt(api_age, id=c("id","Akteur","Acteur","Seite","Anfang","Ende","CHF"))
api_age$variable<-forcats::fct_rev(api_age$variable)

# demography data table data ---------------------------------------------------------

api_demo_table<-CH %>% 
  select(id,Akteur,Acteur,Seite,Anfang,Ende,Ende_table,CHF,Text,Frauen,Männer,"Alter 18-24","Alter 25-34", "Alter 35-44","Alter 45-54","Alter 55-64","Alter 65+")

# APIdata_demo<-APIdata_demo %>% 
#   mutate(URL=paste0("<a href='",URL,"'>",URL,"</a>"))

api_demo_table<-api_demo_table %>% 
  mutate(CHF=round(CHF,0))


#save
if (nrow(file.info(list.files("input/report/zip", full.names = T)))==1) {
  
  save(api_geo,file=paste0("output/api/geo/api_geo_",date,".rdata"))
  save(api_geo_table,file=paste0("output/api/geo_table/api_geo_table_",date,".rdata"))
  
  save(api_age,file=paste0("output/api/age/api_age_",date,".rdata"))
  save(api_gender,file=paste0("output/api/gender/api_gender_",date,".rdata"))
  save(api_demo_table,file=paste0("output/api/demo_table/api_demo_table_",date,".rdata"))
  
  
} else {
  
  api_geo_thisweek<-api_geo
  load(paste0("output/api/geo/api_geo_",date-7,".rdata"))
  api_geo<-api_geo %>% 
    filter(!id %in% api_geo_thisweek$id)  #remove duplicates from the existing dataset (take the newest version)
  api_geo<-api_geo %>% 
    mutate_at(c("Akteur","Acteur"),as.character)    ##combine it as character then introduce order so that all parties are present
  api_geo<-rbind(api_geo_thisweek,api_geo)
  partyorder<-c("SPS","GPS","GLP","EVP","Mitte","FDP","SVP")  #variable used for all subsequent files
  api_geo<-api_geo %>% 
    mutate(Akteur=factor(Akteur,levels=c(partyorder,setdiff(unique(api_geo$Akteur), partyorder))))
  api_geo<-api_geo %>% 
    mutate(Acteur=factor(Acteur,levels=unique(api_geo$Acteur[order(api_geo$Akteur)])))
  save(api_geo,file=paste0("output/api/geo/api_geo_",date,".rdata"))
  
  api_geo_table_thisweek<-api_geo_table
  load(paste0("output/api/geo_table/api_geo_table_",date-7,".rdata"))
  api_geo_table<-api_geo_table %>% 
    filter(!id %in% api_geo_table_thisweek$id)  
  api_geo_table<-api_geo_table %>% 
    mutate_at(c("Akteur","Acteur"),as.character)    ##combine it as character then introduce order so that all parties are present
  api_geo_table<-rbind.fill(api_geo_table_thisweek,api_geo_table)
  api_geo_table<-api_geo_table %>% 
    mutate(Akteur=factor(Akteur,levels=c(partyorder,setdiff(unique(api_geo_table$Akteur), partyorder))))
  api_geo_table<-api_geo_table %>% 
    mutate(Acteur=factor(Acteur,levels=unique(api_geo_table$Acteur[order(api_geo_table$Akteur)])))
  save(api_geo_table,file=paste0("output/api/geo_table/api_geo_table_",date,".rdata"))
  
  api_age_thisweek<-api_age
  load(paste0("output/api/age/api_age_",date-7,".rdata"))
  api_age<-api_age%>% 
    filter(!id %in% api_age_thisweek$id)  
  api_age<-api_age %>% 
    mutate_at(c("Akteur","Acteur"),as.character)    ##combine it as character then introduce order so that all parties are present
  api_age<-rbind.fill(api_age_thisweek,api_age)
  api_age<-api_age %>% 
    mutate(Akteur=factor(Akteur,levels=c(partyorder,setdiff(unique(api_age$Akteur), partyorder))))
  api_age<-api_age %>% 
    mutate(Acteur=factor(Acteur,levels=unique(api_age$Acteur[order(api_age$Akteur)])))
  save(api_age,file=paste0("output/api/age/api_age_",date,".rdata"))
  
  api_gender_thisweek<-api_gender
  load(paste0("output/api/gender/api_gender_",date-7,".rdata"))
  api_gender<-api_gender %>% 
    filter(!id %in% api_gender_thisweek$id)  
  api_gender<-api_gender %>% 
    mutate_at(c("Akteur","Acteur"),as.character)    ##combine it as character then introduce order so that all parties are present
  api_gender<-rbind.fill(api_gender_thisweek,api_gender)
  api_gender<-api_gender %>% 
    mutate(Akteur=factor(Akteur,levels=c(partyorder,setdiff(unique(api_gender$Akteur), partyorder))))
  api_gender<-api_gender %>% 
    mutate(Acteur=factor(Acteur,levels=unique(api_gender$Acteur[order(api_gender$Akteur)])))
  save(api_gender,file=paste0("output/api/gender/api_gender_",date,".rdata"))
  
  api_demo_table_thisweek<-api_demo_table
  load(paste0("output/api/demo_table/api_demo_table_",date-7,".rdata"))
  api_demo_table<-api_demo_table %>% 
    filter(!id %in% api_demo_table_thisweek$id)  
  api_demo_table<-api_demo_table %>% 
    mutate_at(c("Akteur","Acteur"),as.character)    ##combine it as character then introduce order so that all parties are present
  api_demo_table<-rbind.fill(api_demo_table_thisweek,api_demo_table)
  api_demo_table<-api_demo_table %>% 
    mutate(Akteur=factor(Akteur,levels=c(partyorder,setdiff(unique(api_demo_table$Akteur), partyorder))))
  api_demo_table<-api_demo_table %>% 
    mutate(Acteur=factor(Acteur,levels=unique(api_demo_table$Acteur[order(api_demo_table$Akteur)])))
  save(api_demo_table,file=paste0("output/api/demo_table/api_demo_table_",date,".rdata"))
  
}
  

