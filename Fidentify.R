Fidentify<- function(x){
  x<-x %>% 
    mutate(party = case_when(
      grepl("SP|PS|Parti socialiste|Sozialdemokratische|Jungsozialist|JUSO", disclaimer) ~ "SPS",
      grepl("FDP|PLR", disclaimer) ~ "FDP",
      grepl("Grüne|Vert|GRÜNE", disclaimer) ~ "GPS",
      grepl("GLP|Grünliberale|GRÜNLIBERALE|vert'libéral|vert'libéraux|Vert'libéraux", disclaimer) ~ "GLP",
      grepl("Mitte|Centre|MITTE|CENTRE", disclaimer) ~ "Mitte",
      grepl("EVP", disclaimer) ~ "EVP",
      grepl("SVP|UDC", disclaimer) ~ "SVP",
      grepl("Gewerbeverband|des arts et métiers|KMU geprüft|FER Genève", disclaimer) ~ "GV",       
      grepl("Arbeitgeberband|Union patronale", disclaimer) ~ "AV",
      grepl("Economiesuisse|economiesuisse", disclaimer) ~ "ES",
      grepl("Bauernverband|Union suisse des paysans", disclaimer) ~ "BV",
      grepl("Gewerkschaftsbund|Union syndicale", disclaimer) ~ "GB",
      grepl("IG Wirtschaftsverbände|Perspektive", disclaimer) ~ "IGW",
    ))
  
  
  x %>% 
    mutate(party = case_when(
  grepl("Grünenfelder", disclaimer) ~ as.character(NA),   #for candidate ads that should be removed
  grepl("Ryser", Page.name) ~ as.character(NA),   #for candidate ads that should be removed
  TRUE ~ party))
  
  }

Fidentify_page<- function(x){
  x %>% 
    mutate(party = case_when(
      grepl("SP|PS|Parti socialiste|Sozialdemokatische|Jungsozialist|JUSO", page_name) ~ "SPS",
      grepl("FDP|PLR", page_name) ~ "FDP",
      grepl("Grüne|Vert|GRÜNE", page_name) ~ "GPS",
      grepl("GLP|Grünliberale|GRÜNLIBERALE|vert'libéral|vert'libéraux|Vert'libéraux|glp", page_name) ~ "GLP",
      grepl("Mitte|Centre|MITTE|CENTRE|Gesunde Jugend|Parteipäckli", page_name) ~ "Mitte",
      grepl("EVP", page_name) ~ "EVP",
      grepl("SVP|UDC", page_name) ~ "SVP",
      grepl("Gewerbeverband|des arts et métiers|KMU geprüft|FER Genève|NON IN 179", page_name) ~ "GV",       
      grepl("Arbeitgeberband|Union patronale", page_name) ~ "AV",
      grepl("Economiesuisse|economiesuisse", page_name) ~ "ES",
      grepl("Bauernverband|Union suisse des paysans", page_name) ~ "BV",
      grepl("Gewerkschaftsbund|Union syndicale", page_name) ~ "GB",
      grepl("IG Wirtschaftsverbände|Perspektive|perspektiveschweiz.ch", page_name) ~ "IGW",
    ))
}