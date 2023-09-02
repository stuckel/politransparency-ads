Fidentify<- function(x){
  x<-x %>% 
    mutate(party = case_when(
      grepl("SP|PS|Parti socialiste|Sozialdemokratische|Jungsozialist|JUSO|Partito Socialista Ticino|direkt|Direct", disclaimer) ~ "SPS",
      grepl("FDP|PLR|Jungfreisinnige|Libéraux-Radicaux", disclaimer) ~ "FDP",
      grepl("Grüne|Vert|GRÜNE", disclaimer) ~ "GPS",
      grepl("GLP|Grünliberale|GRÜNLIBERALE|vert'libéral|vert'libéraux|Vert'libéraux|verdi liberali", disclaimer) ~ "GLP",
      grepl("Mitte|Centre|MITTE|CENTRE", disclaimer) ~ "Mitte",
      grepl("EVP", disclaimer) ~ "EVP",
      grepl("SVP|UDC", disclaimer) ~ "SVP",
      grepl("Gewerbeverband|des arts et métiers|KMU geprüft|FER Genève|Fer Genève|HotellerieSuisse", disclaimer) ~ "GV",       
      grepl("Arbeitgeberband|Union patronale|Swissmem|Der Berner Arbeitgeber.|Chambre Vaudoise du Commerce et de l'Industrie", disclaimer) ~ "AV",
      grepl("Economiesuisse|economiesuisse", disclaimer) ~ "ES",
      grepl("Bauernverband|Union suisse des paysans", disclaimer) ~ "BV",
      grepl("Gewerkschaftsbund|Union syndicale", disclaimer) ~ "GB",
      grepl("IG Wirtschaftsverbände|Perspektive", disclaimer) ~ "IGW",
    ))
  
  
  x %>% 
    mutate(party = case_when(
  grepl("Grünenfelder", disclaimer) ~ as.character(NA),   #for candidate ads that should be removed
  grepl("Ryser", Page.name) ~ as.character(NA),   #for candidate ads that should be removed
  grepl("Bühler", Page.name) ~ as.character(NA),   #for candidate ads that should be removed
  grepl("Cozzio", Page.name) ~ as.character(NA),   #for candidate ads that should be removed
  grepl("Grossen|Tiana Moser", Page.name) ~ as.character(NA),   #for candidate ads that should be removed
  TRUE ~ party))
  }

Fidentify_page<- function(x){
  x %>% 
    mutate(party = case_when(
      grepl("SP|PS|Parti socialiste|Sozialdemokatische|Jungsozialist|JUSO|Partito Socialista|Direkt|Socialiste|Direct", page_name) ~ "SPS",
      grepl("FDP|PLR|Jungfreisinnige|Libéraux-Radicaux", page_name) ~ "FDP",
      grepl("Grüne|Vert|GRÜNE", page_name) ~ "GPS",
      grepl("GLP|Grünliberale|GRÜNLIBERALE|vert'libéral|vert'libéraux|Vert'libéraux|glp|Verdi Liberali", page_name) ~ "GLP",
      grepl("Mitte|Centre|MITTE|CENTRE|Gesunde Jugend|Parteipäckli", page_name) ~ "Mitte",
      grepl("EVP", page_name) ~ "EVP",
      grepl("SVP|UDC", page_name) ~ "SVP",
      grepl("Gewerbeverband|des arts et métiers|KMU geprüft|FER Genève|NON IN 179|NON-in181|HotellerieSuisse", page_name) ~ "GV",       
      grepl("Arbeitgeberband|Union patronale|Swissmem|Die Berner Arbeitgeber.|Chambre vaudoise du commerce et de l'industrie", page_name) ~ "AV",
      grepl("Economiesuisse|economiesuisse", page_name) ~ "ES",
      grepl("Bauernverband|Union suisse des paysans", page_name) ~ "BV",
      grepl("Gewerkschaftsbund|Union syndicale", page_name) ~ "GB",
      grepl("IG Wirtschaftsverbände|Perspektive|perspektiveschweiz.ch|perspectivesuisse.ch|prospettivasvizzera.ch", page_name) ~ "IGW",
    ))
}