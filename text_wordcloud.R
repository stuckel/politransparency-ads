
#text wordcloud

#check on identified languages (line 136)
#if italian is present adapt line 247

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
#   filter(Seite=="SVP des Kantons Zürich")
# 
# API$Text[API$id==SVP$id[1]]<-c("Klima-Terroristen an unseren Schulen\nDie Meldung schlug am Dienstag ein wie eine Bombe 💣: Klima-Terroristen besetzten die Kantonsschule Enge. Dies Dank der aktiven Mithilfe des SP-Rektors vor Ort. Selbst gewählte Politiker der SP und Grünen Zürich schlossen sich der Besetzung an 🤦🏼‍♂️. Die SVP verurteilt dieses illegale und demokratiegefährdende Verhalten aufs Schärfste ❌. Die einseitige politische Beeinflussung an Schulen ist verfassungswidrig. 🗣️. Der Indoktrinierung unserer Kinder muss Einhalt geboten werden!\n\nWer sich auch immer mehr um unser Schulsystem, unsere Kinder und das politische Klima sorgt hat nur eine Wahl: Am 12. Februar SVP Liste 1! 👊🏼💪🏼\n\n#wählen #liste1 #svpzuerich #wahlen2023 #freiundsicher")
# API$Text[API$id==SVP$id[2]]<-c("Nach dem Verkehr nun die Schulen lahmlegen? Wer genug davon hat, wählt SVP Liste 1🗳️\nNach der Strasse besetzen Klimaaktivisten nun Schulen und legen den Unterricht lahm. 👎🏼 So kürzlich geschehen an der Kantonsschule Enge. Es ist klar: 𝐒𝐨 𝐝𝐚𝐫𝐟 𝐮𝐧𝐝 𝐤𝐚𝐧𝐧 𝐞𝐬 𝐧𝐢𝐜𝐡𝐭 𝐰𝐞𝐢𝐭𝐞𝐫𝐠𝐞𝐡𝐞𝐧! 🚨 Unsere Demokratie lebt vom offenen Austausch, aber nicht von Stör- und Blockade-Aktionen unserer Gesellschaft. In unseren Bildungsinstituten muss das Vermitteln von Wissen im Vordergrund stehen. 👩🏼‍🎓👨🏻‍🎓 Diese einseitige politische Beeinflussung der Schülerinnen und Schüler ist klar verfassungswidrig. \n\n‼️ Wer genug von solchen gesetzeswidrigen Aktionen hat, wählt am Sonntag SVP Liste 1 ‼️ \n\n#freiundsicher #wählen #Liste1 #wahlen2023 #svpzuerich #svp")
# API$Text[API$id==SVP$id[3]]<-c("Fliessender Verkehr statt Strassenkleber - SVP Liste 1 wählen 🗳️\nLinke und grüne Klima-Träumer legen durch ihre Strassenklebe-Aktionen und unbewilligten Velo-Demos den Verkehr lahm. Sie schränken dadurch die arbeitende Bevölkerung im Berufsverkehr und die Rettungskräfte in ihrer Arbeit ein. 👎🏼\nDie SVP will fliessenden Verkehr, harte Bussen und Haftstrafen für Klima-Aktivisten und Schadenersatz durch die Chaoten an die betroffenen Betriebe und Berufsleute. ✅ \n\n👉🏼 Darum: Am 12. Februar SVP – Liste 1 – wählen! 🗳️\n\n#wählen #liste1 #svpzuerich #wahlen2023 #freiundsicher #WahlenZH")
# API$Text[API$id==SVP$id[4]]<-c("Potenzial der Wasserkraft durch Bevölkerungswachstum zunichte gemacht 💧 ❌\n\nDie Zahlen lügen nicht 🔢: Laut der offiziellen Studie des Bundesamts für Energie (BFE) verfügt unser Land über ein Ausbaupotenzial der Wasserkraft bis 2050 von 1.43 Terrawattstunden. 💧⚡️ Der Mehrbedarf an Strom durch unser momentanes Bevölkerungswachstum beträgt jährlich 0.53 Terrawattstunden. \nDas gesamte Ausbaupotenzial der Schweizerischen Wasserkraft deckt also bloss das alleinige Bevölkerungswachstum von 2.7 (!) Jahren. ‼️\n\nWer diesem Wahnsinn Einhalt gebieten möchte, wählt nächste Woche noch 🗳️:\n\n- SVP Liste 1 ✅\n\n- Natalie Rickli, Ernst Stocker, Silvia Steiner, Peter Grünenfelder und Carmen Walker Späh in den Regierungsrat ✅\n\n🚨 Wichtig: Stimmabgabe per Post ist nur noch bis Dienstagmorgen möglich! 🚨 \n\n#svp #svpzuerich #wahlen23 #wasserkraft #bevölkerungswachstum")
# API$Text[API$id==SVP$id[5]]<-c("Genug ist genug - jetzt SVP Liste 1 wählen Wer auch genug hat von Gender-Wahnsinn und Woke-Diktatur, wählt am 12. Februar SVP Liste 1. \n\n#svpwählen #liste1 #freiundsicher #svpzuerich #gendergag")
# API$Text[API$id==SVP$id[6]]<-c("Für Sicherheit und Stabilität! 👌🏼🗳️\nUnsere bisherigen Regierungsräte Natalie Rickli und Ernst Stocker erklären, wie sie einander im Regierungsrat erlebt haben und wofür sie sich auch künftig im Regierungsrat engagieren werden. 💪🏼 \n\nJetzt wählen und mitbestimmen 🗳️:\n✅ Natalie Rickli, Ernst Stocker und die weiteren bürgerlichen Kandidaten für den Regierungsrat. \n✅ Liste 1 der SVP für den Kantonsrat.\n\n#svp #svpzuerich #liste1 #freiundsicher")
# API$Text[API$id==SVP$id[7]]<-c("‼ Treibhausgasaustoss massiv reduziert, doch Zuwanderung verunmöglicht die Erreichung der Klimaziele 🙈‼️\nMit dem Pariser Abkommen hat sich die Schweiz verpflichtet, den Treibhausgasausstoss zu reduzieren und einen Beitrag zum globalen Klimaschutz zu leisten. Die Schweiz hat sich zum Ziel gesetzt, bis 2030 den Treibhausgas-Ausstoss im Vergleich zu 1990 um 50% zu senken. \n\nEin Blick in die Statistiken lässt aufhorchen 👂🏼: Herr und Frau Schweizer haben ihren Treibhausgasausstoss in der Schweiz um 33% reduziert 📉 und hätten damit die Zwischenziele des Pariser Klimaabkommens bis 2020 mehr als deutlich erreicht❗💪🏼 Doch das Bevölkerungswachstum führte dazu, dass der Gesamtausstoss der Schweiz nur um 15% gesenkt werden konnte. 🤯 \n\nObwohl wir uns also an das Klimaabkommen halten und unseren Treibhausgasausstoss pro Kopf stark reduziert haben, führt das hohe Bevölkerungswachstum dazu, dass es für die Schweiz immer schwieriger wird, die Pariser Klimaziele zu erreichen. \n\nWollen wir wirklich so weitermachen⁉️ \n\nWer das nicht will: \n- wählt am 12. Februar SVP – Liste 1  \n- wählt die 5 bürgerlichen Regierungsratskandidaten  \n\n#SVPwählen #liste1 #svpzuerich #wahlen2023 #freiundsicher #zuwanderungbegrenzen")
# API$Text[API$id==SVP$id[8]]<-c("Bevormundung stoppen - SVP wählen \nLinke und grüne Öko-Träumer wollen uns vorschreiben, wie wir zu leben und zu denken haben. Die SVP will den Gender-Wahn und die Bevormundung stoppen ❌ und dafür sorgen, dass wir wieder denken, sagen und essen können, was wir wollen. ✅\nDarum: SVP Liste 1 am 12. Februar wählen! 🗳️\n#wählen #liste1 #svpzuerich #wahlen2023 #freiundsicher")
# API$Text[API$id==SVP$id[9]]<-c("Nach dem Verkehr nun die Schulen lahmlegen? Wer genug davon hat, wählt SVP Liste 1🗳️\nNach der Strasse besetzen Klimaaktivisten nun Schulen und legen den Unterricht lahm. 👎🏼 So kürzlich geschehen an der Kantonsschule Enge. Es ist klar: 𝐒𝐨 𝐝𝐚𝐫𝐟 𝐮𝐧𝐝 𝐤𝐚𝐧𝐧 𝐞𝐬 𝐧𝐢𝐜𝐡𝐭 𝐰𝐞𝐢𝐭𝐞𝐫𝐠𝐞𝐡𝐞𝐧! 🚨 Unsere Demokratie lebt vom offenen Austausch, aber nicht von Stör- und Blockade-Aktionen unserer Gesellschaft. In unseren Bildungsinstituten muss das Vermitteln von Wissen im Vordergrund stehen. 👩🏼‍🎓👨🏻‍🎓 Diese einseitige politische Beeinflussung der Schülerinnen und Schüler ist klar verfassungswidrig. \n\n‼️ Wer genug von solchen gesetzeswidrigen Aktionen hat, wählt am Sonntag SVP Liste 1 ‼️ \n\n#freiundsicher #wählen #Liste1 #wahlen2023 #svpzuerich #svp")
# API$Text[API$id==SVP$id[10]]<-c("Die Zeit rennt - jede Stimme zählt! ⏰\nWer wählt, bestimmt mit. Dass sich der Einsatz für jede Stimme lohnt, beweist unser Parteipräsident Domenik Ledergerber im Video. 💪🏼 \n\nFür eine sichere Zukunft in Freiheit heisst es, JETZT noch wählen zu gehen: 🗳️\n✅ Liste 1 für den Kantonsrat;\n✅ Natalie Rickli und Ernst Stocker für den Regierungsrat - zusammen mit den weiteren bürgerlichen Kandidaten von FDP und Die Mitte.\n\n#freiundsicher #wählen #Liste1 #wahlen2023 #svpzuerich #sv")
# API$Text[API$id==SVP$id[11]]<-c("JETZT noch wählen - für einen bürgerlichen Regierungsrat und Stabilität! 🎯🗳️\nNur wer wählt, bestimmt mit! Die Präsidenten und Präsidentinnen der bürgerlichen Parteien empfehlen, folgende Personen auf den Wahlzettel für den Regierungsrat zu schreiben ✍🏼:\n✅ Natalie Rickli (SVP, bisher)\n✅ Ernst Stocker (SVP, bisher)\n✅ Silvia Steiner (Die Mitte, bisher)\n✅ Carmen Walker Späh (FDP, bisher)\n✅ Peter Grünenfelder (FDP, neu)\n\nSo können wir alle dazu beitragen, dass der Kanton Zürich weiterhin eine funktionierende Grundversorgung, gesunde Finanzen, starke KMU und eine innovative Wirtschaft hat. Vielen Dank für Ihre Stimme! 🗳️\n \n#buergerlichesticket #regierungsratswahlen2023 #svpzuerich #freiundsicher")
# API$Text[API$id==SVP$id[12]]<-c("Energieversorgung gewährleisten - SVP Liste 1 wählen! 🗳️\nWegen der gescheiterten links-grünen Energiepolitik zahlen wir und unsere KMU immer mehr für Benzin, Wärme und Strom und wir müssen uns vor Blackouts fürchten. ⚫ \nDie SVP will eine sichere, bezahlbare Energieversorgung ohne Technologie-Verbote. ✅ \n\n👉🏼 Darum: am 12. Februar SVP – Liste 1 – wählen! 🗳️\n\n#wählen #liste1 #svpzuerich #wahlen2023 #freiundsicher")
# API$Text[API$id==SVP$id[14]]<-c("Jetzt Liste 1 wählen - für eine sichere Zukunft in Freiheit! 🗳️\nDie Wahlunterlagen sind eingetroffen. Für eine sichere Zukunft in Freiheit zählt nun jede Stimme. 🗳️💪🏼 Siehe Video – so geht’s ▶️!\n\n𝐙𝐮 𝐛𝐞𝐚𝐜𝐡𝐭𝐞𝐧:\n👉🏼 NUR die Liste 1 der SVP ins Kuvert legen.\n👉🏼 Die Stimmrechtskarte unterschreiben (ansonsten ist die Wahl ungültig).\n👉🏼 Wahlunterlagen der Familienmitglieder einzeln und nicht im selben Couvert einschicken.\n👉🏼 Bis allerspätestens 7. Februar das Couvert auf die Post bringen. Anschliessend in den Briefkasten der Gemeinde/Stadt werfen oder am Wahlsonntag an die Urne gehen.\n\nHerzlichen Dank für die Unterstützung! \n\n#svp #svpzuerich #liste1 #freiundsicher")
# API$Text[API$id==SVP$id[15]]<-c("Am 12. Februar einen bürgerlichen Regierungsrat wählen! 🗳️\nDamit der Kanton Zürich weiterhin eine hohe Lebensqualität, gesunde Finanzen, starke KMU und eine innovative Wirtschaft hat, empfehlen die Präsidenten und Präsidentinnen der bürgerlichen Parteien zur Wahl in den Regierungsrat: \n✅ Natalie Rickli (SVP, bisher)\n✅ Ernst Stocker (SVP, bisher)\n✅ Silvia Steiner (Die Mitte, bisher)\n✅ Carmen Walker Späh (FDP, bisher)\n✅ Peter Grünenfelder (FDP, neu)\n\nVielen Dank für Ihre Stimme! 🗳️\n\n#buergerlichesticket #regierungsratswahlen2023 #svpzuerich #freiundsicher Natalie Rickli Carmen Walker Späh FDP Peter Grünenfelder, Regierungsratskandidat Zürich Die Mitte Kanton Zürich FDP Kanton Zürich")
# API$Text[API$id==SVP$id[16]]<-c("Energieversorgung gewährleisten - SVP Liste 1 wählen🗳️\nWeniger heizen, Strom sparen und kalt duschen: Der Zustand, wovor die SVP jahrelang gewarnt hat und wofür sie belächelt worden ist, ist Realität geworden. Die Versorgungssicherheit der Schweiz ist nicht mehr gewährleistet. Die Schweiz ist abhängig geworden von Drittstaaten. Schweizerinnen und Schweizer müssen Strom sparen und dafür noch mehr bezahlen. \n\nSo kann es nicht weitergehen! Wer eine sichere, zukunftsfähige und bezahlbare Energie- und Stromversorgung möchte, wählt am 12. Februar 2023:\n👉🏼 SVP Liste 1 für den Kantonsrat.\n👉🏼 Natalie Rickli und Ernst Stocker für den Regierungsrat; zusammen mit den weiteren bürgerlichen Kandidatinnen und Kandidaten von FDP und Die Mitte.\n\nHerzlichen Dank für Ihre Unterstützung für eine sichere Zukunft in Freiheit! 🫶🏼\n\n#wählen #liste1 #svpzuerich #wahlen2023 #freiundsicher")
# API$Text[API$id==SVP$id[17]]<-c("Am 12. Februar einen bürgerlichen Regierungsrat wählen!🗳️\nDamit der Kanton Zürich weiterhin eine hohe Lebensqualität, gesunde Finanzen, starke KMU und eine innovative Wirtschaft hat, empfehlen die Präsidenten und Präsidentinnen der bürgerlichen Parteien zur Wahl in den Regierungsrat: \n✅ Natalie Rickli (SVP, bisher)\n✅ Ernst Stocker (SVP, bisher)\n✅ Silvia Steiner (Die Mitte, bisher)\n✅ Carmen Walker Späh (FDP, bisher)\n✅ Peter Grünenfelder (FDP, neu)\n\nVielen Dank für Ihre Stimme! 🗳️\n\n#buergerlichesticket #regierungsratswahlen2023 #svpzuerich #freiundsicher Natalie Rickli Carmen Walker Späh FDP Peter Grünenfelder, Regierungsratskandidat Zürich Die Mitte Kanton Zürich FDP Kanton Zürich")
# API$Text[API$id==SVP$id[18]]<-c("Für Sicherheit und Stabilität! 👌🏼🗳️\nUnsere bisherigen Regierungsräte Natalie Rickli und Ernst Stocker erklären, wie sie einander im Regierungsrat erlebt haben und wofür sie sich auch künftig im Regierungsrat engagieren werden. 💪🏼 \n\nJetzt wählen und mitbestimmen 🗳️:\n✅ Natalie Rickli, Ernst Stocker und die weiteren bürgerlichen Kandidaten für den Regierungsrat. \n✅ Liste 1 der SVP für den Kantonsrat.\n\n#svp #svpzuerich #liste1 #freiundsicher")
# API$Text[API$id==SVP$id[19]]<-c("Jetzt Liste 1 wählen - für eine sichere Zukunft in Freiheit! 🗳️\nDie Wahlunterlagen sind eingetroffen. Für eine sichere Zukunft in Freiheit zählt nun jede Stimme. 🗳️💪🏼 Siehe Video – so geht’s ▶️!\n\n𝐙𝐮 𝐛𝐞𝐚𝐜𝐡𝐭𝐞𝐧:\n👉🏼 NUR die Liste 1 der SVP ins Kuvert legen.\n👉🏼 Die Stimmrechtskarte unterschreiben (ansonsten ist die Wahl ungültig).\n👉🏼 Wahlunterlagen der Familienmitglieder einzeln und nicht im selben Couvert einschicken.\n👉🏼 Bis allerspätestens 7. Februar das Couvert auf die Post bringen. Anschliessend in den Briefkasten der Gemeinde/Stadt werfen oder am Wahlsonntag an die Urne gehen.\n\nHerzlichen Dank für die Unterstützung! \n\n#svp #svpzuerich #liste1 #freiundsicher")
# API$Text[API$id==SVP$id[21]]<-c("Am 12. Februar einen bürgerlichen Regierungsrat wählen! 🗳️\nDamit der Kanton Zürich weiterhin eine hohe Lebensqualität, gesunde Finanzen, starke KMU und eine innovative Wirtschaft hat, empfehlen die Präsidenten und Präsidentinnen der bürgerlichen Parteien zur Wahl in den Regierungsrat: \n✅ Natalie Rickli (SVP, bisher)\n✅ Ernst Stocker (SVP, bisher)\n✅ Silvia Steiner (Die Mitte, bisher)\n✅ Carmen Walker Späh (FDP, bisher)\n✅ Peter Grünenfelder (FDP, neu)\n\nVielen Dank für Ihre Stimme! 🗳️\n\n#buergerlichesticket #regierungsratswahlen2023 #svpzuerich #freiundsicher Natalie Rickli Carmen Walker Späh FDP Peter Grünenfelder, Regierungsratskandidat Zürich Die Mitte Kanton Zürich FDP Kanton Zürich")


##select new ones 
if (nrow(file.info(list.files("input/report/zip", full.names = T)))==1) {
  
  API<-API

} else {
  
  load(file=paste0("output/wordcloud/wordcloud_cp_",as.Date(date)-7,".rdata"))
  API<-API %>% 
    filter(!id %in% names(API_cor_nouns)) %>% #ids are set as docids (see below) and therefore can be accessed by names()
    filter(!is.na(Text))
  rm(API_cor_nouns)
  
  
  
}

#identify language
API$language<-textcat(API$Text)
table(API$language)
#correct by hand
#API$language[API$language=="danish"]<-c("german")
# API$language[API$language=="romanian"]<-c("french")
#library(openxlsx)
#write.xlsx(API,"C:/Users/s_sim/Desktop/fdp.xlsx")
#API<-read.xlsx("C:/Users/s_sim/Desktop/fdp_clean.xlsx")
#API$Anfang<-as.Date(API$Anfang,origin="1899-12-30")
#API$Ende<-as.Date(API$Ende,origin="1899-12-30")
# check_lang<-API %>% 
#   filter(!language %in% c("german","french"))
# API$language[API$language=="catalan"|API$language=="slovenian-iso8859_2"]<-c("french")
# API$language[API$language=="indonesian"]<-c("german")

API_de<-API %>% 
  filter(language=="german") 

API_de_cor<-corpus(API_de,text_field="Text",docid_field = "id")

API_fr<-API %>% 
  filter(language=="french") 

API_fr_cor<-corpus(API_fr,text_field="Text",docid_field = "id")

API_it<-API %>% 
  filter(language=="italian") 

API_it_cor<-corpus(API_it,text_field="Text",docid_field = "id")


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


#Italian noun identification
#spacy_download_langmodel("it_core_news_sm")
spacy_initialize(model = "it_core_news_sm")

corpus_NOUNreduction_it=function (text) {
  txt = text
  spacy_initialize(model="it_core_news_sm")
  selection = spacy_parse(txt, tag = TRUE, pos = TRUE)
  selection = paste(selection$token[(selection$pos=="PROPN" | selection$pos=="NOUN")], collapse=" ")
  return(selection)
}

plan(multisession)
API_list_nouns = API_it_cor %>%
  furrr::future_map(function(x) corpus_NOUNreduction_it(x), .progress=TRUE ) 

API_it_cor_nouns<-API_it_cor

for (i in 1:length(API_it_cor_nouns)){
  API_it_cor_nouns[[i]]=API_list_nouns[[i]]
}

spacy_finalize()

#combine corpi
if (nrow(file.info(list.files("input/report/zip", full.names = T)))==1) {
  
  API_cor_nouns<-API_de_cor_nouns+API_fr_cor_nouns+API_it_nouns
  
} else {
  
  load(file=paste0("output/wordcloud/wordcloud_cp_",as.Date(date)-7,".rdata"))  #from last week

    API_cor_nouns<-API_de_cor_nouns+API_fr_cor_nouns+API_cor_nouns +API_it_cor_nouns
  
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
                                      "stimme","wähle","listenplatz","kreis","kandidatinnen","kandidierenden","kandidaten","regierungsrätin","kandidatin",
                                      "kandidierende","couvert","urne","video","2x","canton","stadtkreise","ec23","kr23","election","electionscantonales",
                                      "elezione","candidato","elezionicantonali2023","cantonali2023","ec2023"))


#remove names of each party for each party
API_dfm_sp<-dfm_subset(API_dfm,Akteur=="SPS")
topfeatures(API_dfm_sp,n=100)
API_dfm_sp<-dfm_remove(API_dfm_sp,pattern=c("sp","ps","spkantonzuerich"))

API_dfm_fdp<-dfm_subset(API_dfm,Akteur=="FDP")
topfeatures(API_dfm_fdp,n=100)
API_dfm_fdp<-dfm_remove(API_dfm_fdp,pattern=c("fdp","plr","plrge","fdp.die"))

API_dfm_glp<-dfm_subset(API_dfm,Akteur=="GLP")
topfeatures(API_dfm_glp,n=100)
API_dfm_glp<-dfm_remove(API_dfm_glp,pattern=c("glp","pvl","glpzh","grünliberalen"))

API_dfm_mitte<-dfm_subset(API_dfm,Akteur=="Mitte")
topfeatures(API_dfm_mitte,n=100)
API_dfm_mitte<-dfm_remove(API_dfm_mitte,pattern=c("mitte","centre","lecentre","lecentregeneve"))

API_dfm_gps<-dfm_subset(API_dfm,Akteur=="GPS")
topfeatures(API_dfm_gps,n=100)
API_dfm_gps<-dfm_remove(API_dfm_gps,pattern=c("gps","grüne","grünen","vert","pes"))

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



