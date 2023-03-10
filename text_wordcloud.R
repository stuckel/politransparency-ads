
#text wordcloud

library(dplyr)
library(stringr)
library(textcat)
library(spacyr)
library(future)
library(furrr)
library(quanteda)
library(quanteda.textplots)
library(lubridate)
library(GerNameR) #first names german
library(lexicon) # first names US

# load data ---------------------------------------------------------------

rm(list=ls())
df <- file.info(list.files("input/report/zip", full.names = T))
rownames(df)[which.max(df$mtime)]



data<-unzip(rownames(df)[which.max(df$mtime)],list=T)$Name
doc_name <- grep('advertisers', unzip(rownames(df)[which.max(df$mtime)],list=T)$Name, 
                 ignore.case=TRUE, value=TRUE)
date<-str_extract(doc_name,"\\d+-\\d+-\\d+")

#load ads data
load(paste0("output/api/demo_table/api_demo_table_",date,".rdata"))
APIdata<-api_demo_table

# wordcloud ---------------------------------------------------------------

####################word cloud
#for spacy
# spacy_install()
# 
# spacy_download_langmodel(
#   model = "de_core_news_sm",
#   envname = "spacy_condaenv",
#   conda = "auto"
# )

#select variables of interest
API<-APIdata %>% 
  select(id,Akteur, Text, CHF, Anfang, Ende) 

##add French names 

API<-API %>% 
  mutate(Acteur = case_when(
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
    Akteur=="IGW" ~ "IGW",
  ))


#color info
##add col
API<-API %>% 
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

API$Akteur<-as.character(API$Akteur)

##replace formatted text
# SVP<-APIdata %>% 
#   filter(Seite=="SVP des Kantons Z??rich")
# 
# API$Text[API$id==SVP$id[1]]<-c("Klima-Terroristen an unseren Schulen\nDie Meldung schlug am Dienstag ein wie eine Bombe ????: Klima-Terroristen besetzten die Kantonsschule Enge. Dies Dank der aktiven Mithilfe des SP-Rektors vor Ort. Selbst gew??hlte Politiker der SP und Gr??nen Z??rich schlossen sich der Besetzung an ?????????????????. Die SVP verurteilt dieses illegale und demokratiegef??hrdende Verhalten aufs Sch??rfste ???. Die einseitige politische Beeinflussung an Schulen ist verfassungswidrig. ???????. Der Indoktrinierung unserer Kinder muss Einhalt geboten werden!\n\nWer sich auch immer mehr um unser Schulsystem, unsere Kinder und das politische Klima sorgt hat nur eine Wahl: Am 12. Februar SVP Liste 1! ????????????????\n\n#w??hlen #liste1 #svpzuerich #wahlen2023 #freiundsicher")
# API$Text[API$id==SVP$id[2]]<-c("Nach dem Verkehr nun die Schulen lahmlegen? Wer genug davon hat, w??hlt SVP Liste 1???????\nNach der Strasse besetzen Klimaaktivisten nun Schulen und legen den Unterricht lahm. ???????? So k??rzlich geschehen an der Kantonsschule Enge. Es ist klar: ???????? ???????????????? ???????????? ???????????????? ???????? ???????????????????? ????????????????????????????????????????????! ???? Unsere Demokratie lebt vom offenen Austausch, aber nicht von St??r- und Blockade-Aktionen unserer Gesellschaft. In unseren Bildungsinstituten muss das Vermitteln von Wissen im Vordergrund stehen. ?????????????????????????????? Diese einseitige politische Beeinflussung der Sch??lerinnen und Sch??ler ist klar verfassungswidrig. \n\n?????? Wer genug von solchen gesetzeswidrigen Aktionen hat, w??hlt am Sonntag SVP Liste 1 ?????? \n\n#freiundsicher #w??hlen #Liste1 #wahlen2023 #svpzuerich #svp")
# API$Text[API$id==SVP$id[3]]<-c("Fliessender Verkehr statt Strassenkleber - SVP Liste 1 w??hlen ???????\nLinke und gr??ne Klima-Tr??umer legen durch ihre Strassenklebe-Aktionen und unbewilligten Velo-Demos den Verkehr lahm. Sie schr??nken dadurch die arbeitende Bev??lkerung im Berufsverkehr und die Rettungskr??fte in ihrer Arbeit ein. ????????\nDie SVP will fliessenden Verkehr, harte Bussen und Haftstrafen f??r Klima-Aktivisten und Schadenersatz durch die Chaoten an die betroffenen Betriebe und Berufsleute. ??? \n\n???????? Darum: Am 12. Februar SVP ??? Liste 1 ??? w??hlen! ???????\n\n#w??hlen #liste1 #svpzuerich #wahlen2023 #freiundsicher #WahlenZH")
# API$Text[API$id==SVP$id[4]]<-c("Potenzial der Wasserkraft durch Bev??lkerungswachstum zunichte gemacht ???? ???\n\nDie Zahlen l??gen nicht ????: Laut der offiziellen Studie des Bundesamts f??r Energie (BFE) verf??gt unser Land ??ber ein Ausbaupotenzial der Wasserkraft bis 2050 von 1.43 Terrawattstunden. ?????????? Der Mehrbedarf an Strom durch unser momentanes Bev??lkerungswachstum betr??gt j??hrlich 0.53 Terrawattstunden. \nDas gesamte Ausbaupotenzial der Schweizerischen Wasserkraft deckt also bloss das alleinige Bev??lkerungswachstum von 2.7 (!) Jahren. ??????\n\nWer diesem Wahnsinn Einhalt gebieten m??chte, w??hlt n??chste Woche noch ???????:\n\n- SVP Liste 1 ???\n\n- Natalie Rickli, Ernst Stocker, Silvia Steiner, Peter Gr??nenfelder und Carmen Walker Sp??h in den Regierungsrat ???\n\n???? Wichtig: Stimmabgabe per Post ist nur noch bis Dienstagmorgen m??glich! ???? \n\n#svp #svpzuerich #wahlen23 #wasserkraft #bev??lkerungswachstum")
# API$Text[API$id==SVP$id[5]]<-c("Genug ist genug - jetzt SVP Liste 1 w??hlen Wer auch genug hat von Gender-Wahnsinn und Woke-Diktatur, w??hlt am 12. Februar SVP Liste 1. \n\n#svpw??hlen #liste1 #freiundsicher #svpzuerich #gendergag")
# API$Text[API$id==SVP$id[6]]<-c("F??r Sicherheit und Stabilit??t! ???????????????\nUnsere bisherigen Regierungsr??te Natalie Rickli und Ernst Stocker erkl??ren, wie sie einander im Regierungsrat erlebt haben und wof??r sie sich auch k??nftig im Regierungsrat engagieren werden. ???????? \n\nJetzt w??hlen und mitbestimmen ???????:\n??? Natalie Rickli, Ernst Stocker und die weiteren b??rgerlichen Kandidaten f??r den Regierungsrat. \n??? Liste 1 der SVP f??r den Kantonsrat.\n\n#svp #svpzuerich #liste1 #freiundsicher")
# API$Text[API$id==SVP$id[7]]<-c("??? Treibhausgasaustoss massiv reduziert, doch Zuwanderung verunm??glicht die Erreichung der Klimaziele ??????????\nMit dem Pariser Abkommen hat sich die Schweiz verpflichtet, den Treibhausgasausstoss zu reduzieren und einen Beitrag zum globalen Klimaschutz zu leisten. Die Schweiz hat sich zum Ziel gesetzt, bis 2030 den Treibhausgas-Ausstoss im Vergleich zu 1990 um 50% zu senken. \n\nEin Blick in die Statistiken l??sst aufhorchen ????????: Herr und Frau Schweizer haben ihren Treibhausgasausstoss in der Schweiz um 33% reduziert ???? und h??tten damit die Zwischenziele des Pariser Klimaabkommens bis 2020 mehr als deutlich erreicht??????????? Doch das Bev??lkerungswachstum f??hrte dazu, dass der Gesamtausstoss der Schweiz nur um 15% gesenkt werden konnte. ???? \n\nObwohl wir uns also an das Klimaabkommen halten und unseren Treibhausgasausstoss pro Kopf stark reduziert haben, f??hrt das hohe Bev??lkerungswachstum dazu, dass es f??r die Schweiz immer schwieriger wird, die Pariser Klimaziele zu erreichen. \n\nWollen wir wirklich so weitermachen?????? \n\nWer das nicht will: \n- w??hlt am 12. Februar SVP ??? Liste 1  \n- w??hlt die 5 b??rgerlichen Regierungsratskandidaten  \n\n#SVPw??hlen #liste1 #svpzuerich #wahlen2023 #freiundsicher #zuwanderungbegrenzen")
# API$Text[API$id==SVP$id[8]]<-c("Bevormundung stoppen - SVP w??hlen \nLinke und gr??ne ??ko-Tr??umer wollen uns vorschreiben, wie wir zu leben und zu denken haben. Die SVP will den Gender-Wahn und die Bevormundung stoppen ??? und daf??r sorgen, dass wir wieder denken, sagen und essen k??nnen, was wir wollen. ???\nDarum: SVP Liste 1 am 12. Februar w??hlen! ???????\n#w??hlen #liste1 #svpzuerich #wahlen2023 #freiundsicher")
# API$Text[API$id==SVP$id[9]]<-c("Nach dem Verkehr nun die Schulen lahmlegen? Wer genug davon hat, w??hlt SVP Liste 1???????\nNach der Strasse besetzen Klimaaktivisten nun Schulen und legen den Unterricht lahm. ???????? So k??rzlich geschehen an der Kantonsschule Enge. Es ist klar: ???????? ???????????????? ???????????? ???????????????? ???????? ???????????????????? ????????????????????????????????????????????! ???? Unsere Demokratie lebt vom offenen Austausch, aber nicht von St??r- und Blockade-Aktionen unserer Gesellschaft. In unseren Bildungsinstituten muss das Vermitteln von Wissen im Vordergrund stehen. ?????????????????????????????? Diese einseitige politische Beeinflussung der Sch??lerinnen und Sch??ler ist klar verfassungswidrig. \n\n?????? Wer genug von solchen gesetzeswidrigen Aktionen hat, w??hlt am Sonntag SVP Liste 1 ?????? \n\n#freiundsicher #w??hlen #Liste1 #wahlen2023 #svpzuerich #svp")
# API$Text[API$id==SVP$id[10]]<-c("Die Zeit rennt - jede Stimme z??hlt! ???\nWer w??hlt, bestimmt mit. Dass sich der Einsatz f??r jede Stimme lohnt, beweist unser Parteipr??sident Domenik Ledergerber im Video. ???????? \n\nF??r eine sichere Zukunft in Freiheit heisst es, JETZT noch w??hlen zu gehen: ???????\n??? Liste 1 f??r den Kantonsrat;\n??? Natalie Rickli und Ernst Stocker f??r den Regierungsrat - zusammen mit den weiteren b??rgerlichen Kandidaten von FDP und Die Mitte.\n\n#freiundsicher #w??hlen #Liste1 #wahlen2023 #svpzuerich #sv")
# API$Text[API$id==SVP$id[11]]<-c("JETZT noch w??hlen - f??r einen b??rgerlichen Regierungsrat und Stabilit??t! ???????????\nNur wer w??hlt, bestimmt mit! Die Pr??sidenten und Pr??sidentinnen der b??rgerlichen Parteien empfehlen, folgende Personen auf den Wahlzettel f??r den Regierungsrat zu schreiben ???????:\n??? Natalie Rickli (SVP, bisher)\n??? Ernst Stocker (SVP, bisher)\n??? Silvia Steiner (Die Mitte, bisher)\n??? Carmen Walker Sp??h (FDP, bisher)\n??? Peter Gr??nenfelder (FDP, neu)\n\nSo k??nnen wir alle dazu beitragen, dass der Kanton Z??rich weiterhin eine funktionierende Grundversorgung, gesunde Finanzen, starke KMU und eine innovative Wirtschaft hat. Vielen Dank f??r Ihre Stimme! ???????\n \n#buergerlichesticket #regierungsratswahlen2023 #svpzuerich #freiundsicher")
# API$Text[API$id==SVP$id[12]]<-c("Energieversorgung gew??hrleisten - SVP Liste 1 w??hlen! ???????\nWegen der gescheiterten links-gr??nen Energiepolitik zahlen wir und unsere KMU immer mehr f??r Benzin, W??rme und Strom und wir m??ssen uns vor Blackouts f??rchten. ??? \nDie SVP will eine sichere, bezahlbare Energieversorgung ohne Technologie-Verbote. ??? \n\n???????? Darum: am 12. Februar SVP ??? Liste 1 ??? w??hlen! ???????\n\n#w??hlen #liste1 #svpzuerich #wahlen2023 #freiundsicher")
# API$Text[API$id==SVP$id[14]]<-c("Jetzt Liste 1 w??hlen - f??r eine sichere Zukunft in Freiheit! ???????\nDie Wahlunterlagen sind eingetroffen. F??r eine sichere Zukunft in Freiheit z??hlt nun jede Stimme. ??????????????? Siehe Video ??? so geht???s ??????!\n\n???????? ????????????????????????????????:\n???????? NUR die Liste 1 der SVP ins Kuvert legen.\n???????? Die Stimmrechtskarte unterschreiben (ansonsten ist die Wahl ung??ltig).\n???????? Wahlunterlagen der Familienmitglieder einzeln und nicht im selben Couvert einschicken.\n???????? Bis allersp??testens 7. Februar das Couvert auf die Post bringen. Anschliessend in den Briefkasten der Gemeinde/Stadt werfen oder am Wahlsonntag an die Urne gehen.\n\nHerzlichen Dank f??r die Unterst??tzung! \n\n#svp #svpzuerich #liste1 #freiundsicher")
# API$Text[API$id==SVP$id[15]]<-c("Am 12. Februar einen b??rgerlichen Regierungsrat w??hlen! ???????\nDamit der Kanton Z??rich weiterhin eine hohe Lebensqualit??t, gesunde Finanzen, starke KMU und eine innovative Wirtschaft hat, empfehlen die Pr??sidenten und Pr??sidentinnen der b??rgerlichen Parteien zur Wahl in den Regierungsrat: \n??? Natalie Rickli (SVP, bisher)\n??? Ernst Stocker (SVP, bisher)\n??? Silvia Steiner (Die Mitte, bisher)\n??? Carmen Walker Sp??h (FDP, bisher)\n??? Peter Gr??nenfelder (FDP, neu)\n\nVielen Dank f??r Ihre Stimme! ???????\n\n#buergerlichesticket #regierungsratswahlen2023 #svpzuerich #freiundsicher Natalie Rickli Carmen Walker Sp??h FDP Peter Gr??nenfelder, Regierungsratskandidat Z??rich Die Mitte Kanton Z??rich FDP Kanton Z??rich")
# API$Text[API$id==SVP$id[16]]<-c("Energieversorgung gew??hrleisten - SVP Liste 1 w??hlen???????\nWeniger heizen, Strom sparen und kalt duschen: Der Zustand, wovor die SVP jahrelang gewarnt hat und wof??r sie bel??chelt worden ist, ist Realit??t geworden. Die Versorgungssicherheit der Schweiz ist nicht mehr gew??hrleistet. Die Schweiz ist abh??ngig geworden von Drittstaaten. Schweizerinnen und Schweizer m??ssen Strom sparen und daf??r noch mehr bezahlen. \n\nSo kann es nicht weitergehen! Wer eine sichere, zukunftsf??hige und bezahlbare Energie- und Stromversorgung m??chte, w??hlt am 12. Februar 2023:\n???????? SVP Liste 1 f??r den Kantonsrat.\n???????? Natalie Rickli und Ernst Stocker f??r den Regierungsrat; zusammen mit den weiteren b??rgerlichen Kandidatinnen und Kandidaten von FDP und Die Mitte.\n\nHerzlichen Dank f??r Ihre Unterst??tzung f??r eine sichere Zukunft in Freiheit! ????????\n\n#w??hlen #liste1 #svpzuerich #wahlen2023 #freiundsicher")
# API$Text[API$id==SVP$id[17]]<-c("Am 12. Februar einen b??rgerlichen Regierungsrat w??hlen!???????\nDamit der Kanton Z??rich weiterhin eine hohe Lebensqualit??t, gesunde Finanzen, starke KMU und eine innovative Wirtschaft hat, empfehlen die Pr??sidenten und Pr??sidentinnen der b??rgerlichen Parteien zur Wahl in den Regierungsrat: \n??? Natalie Rickli (SVP, bisher)\n??? Ernst Stocker (SVP, bisher)\n??? Silvia Steiner (Die Mitte, bisher)\n??? Carmen Walker Sp??h (FDP, bisher)\n??? Peter Gr??nenfelder (FDP, neu)\n\nVielen Dank f??r Ihre Stimme! ???????\n\n#buergerlichesticket #regierungsratswahlen2023 #svpzuerich #freiundsicher Natalie Rickli Carmen Walker Sp??h FDP Peter Gr??nenfelder, Regierungsratskandidat Z??rich Die Mitte Kanton Z??rich FDP Kanton Z??rich")
# API$Text[API$id==SVP$id[18]]<-c("F??r Sicherheit und Stabilit??t! ???????????????\nUnsere bisherigen Regierungsr??te Natalie Rickli und Ernst Stocker erkl??ren, wie sie einander im Regierungsrat erlebt haben und wof??r sie sich auch k??nftig im Regierungsrat engagieren werden. ???????? \n\nJetzt w??hlen und mitbestimmen ???????:\n??? Natalie Rickli, Ernst Stocker und die weiteren b??rgerlichen Kandidaten f??r den Regierungsrat. \n??? Liste 1 der SVP f??r den Kantonsrat.\n\n#svp #svpzuerich #liste1 #freiundsicher")
# API$Text[API$id==SVP$id[19]]<-c("Jetzt Liste 1 w??hlen - f??r eine sichere Zukunft in Freiheit! ???????\nDie Wahlunterlagen sind eingetroffen. F??r eine sichere Zukunft in Freiheit z??hlt nun jede Stimme. ??????????????? Siehe Video ??? so geht???s ??????!\n\n???????? ????????????????????????????????:\n???????? NUR die Liste 1 der SVP ins Kuvert legen.\n???????? Die Stimmrechtskarte unterschreiben (ansonsten ist die Wahl ung??ltig).\n???????? Wahlunterlagen der Familienmitglieder einzeln und nicht im selben Couvert einschicken.\n???????? Bis allersp??testens 7. Februar das Couvert auf die Post bringen. Anschliessend in den Briefkasten der Gemeinde/Stadt werfen oder am Wahlsonntag an die Urne gehen.\n\nHerzlichen Dank f??r die Unterst??tzung! \n\n#svp #svpzuerich #liste1 #freiundsicher")
# API$Text[API$id==SVP$id[21]]<-c("Am 12. Februar einen b??rgerlichen Regierungsrat w??hlen! ???????\nDamit der Kanton Z??rich weiterhin eine hohe Lebensqualit??t, gesunde Finanzen, starke KMU und eine innovative Wirtschaft hat, empfehlen die Pr??sidenten und Pr??sidentinnen der b??rgerlichen Parteien zur Wahl in den Regierungsrat: \n??? Natalie Rickli (SVP, bisher)\n??? Ernst Stocker (SVP, bisher)\n??? Silvia Steiner (Die Mitte, bisher)\n??? Carmen Walker Sp??h (FDP, bisher)\n??? Peter Gr??nenfelder (FDP, neu)\n\nVielen Dank f??r Ihre Stimme! ???????\n\n#buergerlichesticket #regierungsratswahlen2023 #svpzuerich #freiundsicher Natalie Rickli Carmen Walker Sp??h FDP Peter Gr??nenfelder, Regierungsratskandidat Z??rich Die Mitte Kanton Z??rich FDP Kanton Z??rich")

##select new ones 
if (nrow(file.info(list.files("input/report/zip", full.names = T)))==1) {
  
  API<-API

} else {
  
  load(file=paste0("output/wordcloud/wordcloud_cp_",as.Date(date)-7,".rdata"))
  API<-API %>% 
    filter(!id %in% names(API_cor_nouns))  #ids are set as docids (see below) and therefore can be accessed by names()
  rm(API_cor_nouns)
  
}

#identify language
API$language<-textcat(API$Text)
# table(API$language)
# check_lang<-API %>% 
#   filter(!language %in% c("german","french"))
# API$language[API$language=="middle_frisian"|API$language=="swedish"]<-c("german")

API_de<-API %>% 
  filter(language=="german") 

API_de_cor<-corpus(API_de,text_field="Text",docid_field = "id")

API_fr<-API %>% 
  filter(language=="french") 

API_fr_cor<-corpus(API_fr,text_field="Text",docid_field = "id")

#German noun identification
spacy_initialize(model = "de_core_news_sm")

corpus_NOUNreduction_de=function (text) {
  txt = text
  spacy_initialize(model="de_core_news_sm")
  selection = spacy_parse(txt, tag = TRUE, pos = TRUE)
  selection = paste(selection$token[(selection$pos=="PROPN" | selection$pos=="NOUN")], collapse=" ")
  return(selection)
}

plan(multisession)
API_list_nouns = API_de_cor %>%
  furrr::future_map(function(x) corpus_NOUNreduction_de(x), .progress=TRUE ) 

API_de_cor_nouns<-API_de_cor

for (i in 1:length(API_de_cor_nouns)){
  API_de_cor_nouns[[i]]=API_list_nouns[[i]]
}

spacy_finalize()


#French noun identification
#spacy_download_langmodel("fr_core_news_sm")
spacy_initialize(model = "fr_core_news_sm")

corpus_NOUNreduction_fr=function (text) {
  txt = text
  spacy_initialize(model="fr_core_news_sm")
  selection = spacy_parse(txt, tag = TRUE, pos = TRUE)
  selection = paste(selection$token[(selection$pos=="PROPN" | selection$pos=="NOUN")], collapse=" ")
  return(selection)
}

plan(multisession)
API_list_nouns = API_fr_cor %>%
  furrr::future_map(function(x) corpus_NOUNreduction_fr(x), .progress=TRUE ) 

API_fr_cor_nouns<-API_fr_cor

for (i in 1:length(API_fr_cor_nouns)){
  API_fr_cor_nouns[[i]]=API_list_nouns[[i]]
}

spacy_finalize()


#combine corpi
if (nrow(file.info(list.files("input/report/zip", full.names = T)))==1) {
  
  API_cor_nouns<-API_de_cor_nouns+API_fr_cor_nouns
  
} else {
  
  load(file=paste0("output/wordcloud/wordcloud_cp_",as.Date(date)-7,".rdata"))  #from last week

    API_cor_nouns<-API_de_cor_nouns+API_fr_cor_nouns+API_cor_nouns
  
}

save(API_cor_nouns,file=paste0("output/wordcloud/wordcloud_cp_",date,".rdata"))

#get first names
#German
names<-as.data.frame(filter.names())
#US
data("freq_first_names")
names_US<-freq_first_names
names_US<-names_US %>% 
  filter(!is.na(Name))
names_all<-c(names$name,names_US$Name)

stops<-unique(c(stopwords("german"),stopwords("german",source="marimo"),stopwords("french"),"e",names_all))

API_toks <- tokens(API_cor_nouns, remove_punct = TRUE,remove_number = TRUE,remove_url = TRUE) %>% 
  tokens_remove(pattern = stops) %>% 
  tokens_remove(pattern="www.*",valuetype=c("regex")) %>% 
  tokens_remove(pattern="@.*",valuetype=c("regex")) %>% 
  tokens_keep(pattern="[[:alpha:]]", valuetype=c("regex")) 

API_dfm <- dfm(API_toks)  

#remove frequent generic politics terms
topfeatures(API_dfm,n=100)
API_dfm<-dfm_remove(API_dfm,pattern=c(".*wahl.*"),valuetype="regex")  ##remove everything related to wahl
API_dfm<-dfm_remove(API_dfm,pattern=c(".*liste.*"),valuetype="regex")  ##remove everything related to Liste

API_dfm<-dfm_remove(API_dfm,pattern=c("kantonsrat","regierungsrat","kanton","politik","politique","landrat","partei","bezirk",
                                      "stimme","w??hle","listenplatz","kreis","kandidatinnen","kandidierenden","kandidaten","regierungsr??tin","kandidatin",
                                      "kandidierende","couvert","urne","video","2x","canton","stadtkreise"))


#remove names of each party for each party
API_dfm_sp<-dfm_subset(API_dfm,Akteur=="SPS")
topfeatures(API_dfm_sp,n=100)
API_dfm_sp<-dfm_remove(API_dfm_sp,pattern=c("sp","ps","spkantonzuerich"))

API_dfm_fdp<-dfm_subset(API_dfm,Akteur=="FDP")
topfeatures(API_dfm_fdp,n=100)
API_dfm_fdp<-dfm_remove(API_dfm_fdp,pattern=c("fdp","plr"))

API_dfm_glp<-dfm_subset(API_dfm,Akteur=="GLP")
topfeatures(API_dfm_glp,n=100)
API_dfm_glp<-dfm_remove(API_dfm_glp,pattern=c("glp","pvl","glpzh","gr??nliberalen"))

API_dfm_mitte<-dfm_subset(API_dfm,Akteur=="Mitte")
topfeatures(API_dfm_mitte,n=100)
API_dfm_mitte<-dfm_remove(API_dfm_mitte,pattern=c("mitte","centre"))

API_dfm_gps<-dfm_subset(API_dfm,Akteur=="GPS")
topfeatures(API_dfm_gps,n=100)
API_dfm_gps<-dfm_remove(API_dfm_gps,pattern=c("gps","gr??ne","gr??nen","vert","pes"))

API_dfm_svp<-dfm_subset(API_dfm,Akteur=="SVP")
topfeatures(API_dfm_svp,n=100)
API_dfm_svp<-dfm_remove(API_dfm_svp,pattern=c("svp","udc","svpzuerich"))

API_dfm_rest<-dfm_subset(API_dfm,Akteur %in% c("EVP","GB","GV","IGW"))

API_dfm_all<-rbind(API_dfm_sp,API_dfm_fdp,API_dfm_glp,API_dfm_mitte,API_dfm_gps,API_dfm_svp,API_dfm_rest)

API_dfm_orig<-API_dfm
API_dfm<-API_dfm_all

#define order of party factor only once old and new datasets have been combined 
partyorder<-rev(c("SVP","SPS","GPS","GLP","EVP","Mitte","FDP"))
API_dfm@docvars$Akteur<-factor(API_dfm@docvars$Akteur,levels=c(partyorder,setdiff(unique(API_dfm@docvars$Akteur), partyorder)))
API_dfm@docvars$Acteur<-factor(API_dfm@docvars$Acteur,levels=unique(API_dfm@docvars$Acteur[order(API_dfm@docvars$Akteur)]))

#transform into df for adding weight
API_w <- convert(API_dfm, to="data.frame")
API_w<-cbind(API_w,weight=API_dfm@docvars$CHF)
API_w<-API_w %>% 
  mutate_if(is.numeric,~ . * weight) %>% 
  select(-weight)

#transform back into dfm 
rownames(API_w)<-API_w$doc_id
API_w<-API_w %>% 
  select(-doc_id)
API_w_dfm<-as.dfm(API_w)
docvars(API_w_dfm, "Akteur") <- API_dfm@docvars$Akteur
docvars(API_w_dfm, "Acteur") <- API_dfm@docvars$Acteur
docvars(API_w_dfm, "Akteur_col") <- API_dfm@docvars$Akteur_col
docvars(API_w_dfm, "Anfang") <- ymd(API_dfm@docvars$Anfang)
docvars(API_w_dfm, "Ende") <- ymd(API_dfm@docvars$Ende)

save(API_w_dfm,file=paste0("output/wordcloud/wordcloud_",date,".rdata"))


