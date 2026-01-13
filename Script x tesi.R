#install.packages("devtools", type = "win.binary")
#install.packages("worldfootballR")
# LIBRERIA ----------------------------------------------------------------
library(devtools)
library(worldfootballR)
library(dplyr)
library(rpart)
library(rpart.plot)
library(cluster)
library(tidyverse)  
library(factoextra)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(ggpubr)
library(NbClust)

devtools::install_github("JaseZiv/worldfootballR")
mapped_players <- player_dictionary_mapping()



# CITAZIONI -----------------------------------------------------
citation()
citation("devtools")
citation("worldfootballR")
citation("dplyr")
citation("rpart")
citation("rpart.plot")
citation("cluster")
citation("tidyverse")
citation("factoextra")
citation("ggplot2")
citation("ggridges")
citation("gridExtra")
citation("ggpubr")
citation("NbClust")

#dplyr::glimpse(mapped_players)
#tm_league_team_urls(country_name = "Italy",start_year = 2021)
#tm_league_team_urls(country_name = "Spain",start_year = 2021)
#tm_league_team_urls(country_name = "Germany",start_year = 2021)
#tm_league_team_urls(country_name = "England",start_year = 2021)
#tm_league_team_urls(country_name = "France",start_year = 2021)

# FONTE DEI DATI -------------------------------------------------------



## statistiche giocatori ----------------------------------------------------------


stat_ita <- tm_squad_stats(team_url = c("https://www.transfermarkt.com/juventus-turin/startseite/verein/506/saison_id/2021",     
                                           "https://www.transfermarkt.com/inter-mailand/startseite/verein/46/saison_id/2021",       
                                           "https://www.transfermarkt.com/ac-mailand/startseite/verein/5/saison_id/2021",           
                                           "https://www.transfermarkt.com/ssc-neapel/startseite/verein/6195/saison_id/2021",        
                                           "https://www.transfermarkt.com/atalanta-bergamo/startseite/verein/800/saison_id/2021",   
                                           "https://www.transfermarkt.com/as-rom/startseite/verein/12/saison_id/2021",              
                                           "https://www.transfermarkt.com/ac-florenz/startseite/verein/430/saison_id/2021",         
                                           "https://www.transfermarkt.com/lazio-rom/startseite/verein/398/saison_id/2021",          
                                           "https://www.transfermarkt.com/us-sassuolo/startseite/verein/6574/saison_id/2021",       
                                           "https://www.transfermarkt.com/fc-turin/startseite/verein/416/saison_id/2021",           
                                           "https://www.transfermarkt.com/fc-bologna/startseite/verein/1025/saison_id/2021",        
                                           "https://www.transfermarkt.com/hellas-verona/startseite/verein/276/saison_id/2021",      
                                           "https://www.transfermarkt.com/udinese-calcio/startseite/verein/410/saison_id/2021",     
                                           "https://www.transfermarkt.com/cagliari-calcio/startseite/verein/1390/saison_id/2021",   
                                           "https://www.transfermarkt.com/sampdoria-genua/startseite/verein/1038/saison_id/2021",   
                                           "https://www.transfermarkt.com/fc-empoli/startseite/verein/749/saison_id/2021",          
                                           "https://www.transfermarkt.com/genua-cfc/startseite/verein/252/saison_id/2021",          
                                           "https://www.transfermarkt.com/venezia-fc/startseite/verein/607/saison_id/2021",         
                                           "https://www.transfermarkt.com/spezia-calcio/startseite/verein/3522/saison_id/2021",     
                                           "https://www.transfermarkt.com/us-salernitana-1919/startseite/verein/380/saison_id/2021"))

stat_spa <- tm_squad_stats(team_url = c("https://www.transfermarkt.com/real-madrid/startseite/verein/418/saison_id/2021",                
                                          "https://www.transfermarkt.com/fc-barcelona/startseite/verein/131/saison_id/2021",               
                                          "https://www.transfermarkt.com/atletico-madrid/startseite/verein/13/saison_id/2021",             
                                          "https://www.transfermarkt.com/fc-villarreal/startseite/verein/1050/saison_id/2021",             
                                          "https://www.transfermarkt.com/fc-sevilla/startseite/verein/368/saison_id/2021",                 
                                          "https://www.transfermarkt.com/real-sociedad-san-sebastian/startseite/verein/681/saison_id/2021",
                                          "https://www.transfermarkt.com/fc-valencia/startseite/verein/1049/saison_id/2021",               
                                          "https://www.transfermarkt.com/real-betis-sevilla/startseite/verein/150/saison_id/2021",         
                                          "https://www.transfermarkt.com/athletic-bilbao/startseite/verein/621/saison_id/2021",            
                                          "https://www.transfermarkt.com/fc-getafe/startseite/verein/3709/saison_id/2021",                 
                                          "https://www.transfermarkt.com/espanyol-barcelona/startseite/verein/714/saison_id/2021",         
                                          "https://www.transfermarkt.com/celta-vigo/startseite/verein/940/saison_id/2021",                 
                                          "https://www.transfermarkt.com/ca-osasuna/startseite/verein/331/saison_id/2021",                 
                                          "https://www.transfermarkt.com/fc-granada/startseite/verein/16795/saison_id/2021",               
                                          "https://www.transfermarkt.com/ud-levante/startseite/verein/3368/saison_id/2021",                
                                          "https://www.transfermarkt.com/rcd-mallorca/startseite/verein/237/saison_id/2021",               
                                          "https://www.transfermarkt.com/fc-elche/startseite/verein/1531/saison_id/2021",                  
                                          "https://www.transfermarkt.com/fc-cadiz/startseite/verein/2687/saison_id/2021",                  
                                          "https://www.transfermarkt.com/rayo-vallecano/startseite/verein/367/saison_id/2021",             
                                          "https://www.transfermarkt.com/deportivo-alaves/startseite/verein/1108/saison_id/2021"))

stat_ger <- tm_squad_stats(team_url = c("https://www.transfermarkt.com/fc-bayern-munchen/startseite/verein/27/saison_id/2021",        
                                         "https://www.transfermarkt.com/borussia-dortmund/startseite/verein/16/saison_id/2021",        
                                         "https://www.transfermarkt.com/rasenballsport-leipzig/startseite/verein/23826/saison_id/2021",
                                         "https://www.transfermarkt.com/bayer-04-leverkusen/startseite/verein/15/saison_id/2021",      
                                         "https://www.transfermarkt.com/vfl-wolfsburg/startseite/verein/82/saison_id/2021",            
                                         "https://www.transfermarkt.com/borussia-monchengladbach/startseite/verein/18/saison_id/2021", 
                                         "https://www.transfermarkt.com/eintracht-frankfurt/startseite/verein/24/saison_id/2021",      
                                         "https://www.transfermarkt.com/tsg-1899-hoffenheim/startseite/verein/533/saison_id/2021",     
                                         "https://www.transfermarkt.com/sc-freiburg/startseite/verein/60/saison_id/2021",              
                                         "https://www.transfermarkt.com/vfb-stuttgart/startseite/verein/79/saison_id/2021",            
                                         "https://www.transfermarkt.com/hertha-bsc/startseite/verein/44/saison_id/2021",               
                                         "https://www.transfermarkt.com/1-fsv-mainz-05/startseite/verein/39/saison_id/2021",           
                                         "https://www.transfermarkt.com/1-fc-union-berlin/startseite/verein/89/saison_id/2021",        
                                         "https://www.transfermarkt.com/1-fc-koln/startseite/verein/3/saison_id/2021",                 
                                         "https://www.transfermarkt.com/fc-augsburg/startseite/verein/167/saison_id/2021",             
                                         "https://www.transfermarkt.com/arminia-bielefeld/startseite/verein/10/saison_id/2021",        
                                         "https://www.transfermarkt.com/vfl-bochum/startseite/verein/80/saison_id/2021",               
                                         "https://www.transfermarkt.com/spvgg-greuther-furth/startseite/verein/65/saison_id/2021" ))

stat_ing <- tm_squad_stats(team_url = c("https://www.transfermarkt.com/manchester-city/startseite/verein/281/saison_id/2021",          
                                         "https://www.transfermarkt.com/fc-liverpool/startseite/verein/31/saison_id/2021",              
                                         "https://www.transfermarkt.com/fc-chelsea/startseite/verein/631/saison_id/2021",               
                                         "https://www.transfermarkt.com/manchester-united/startseite/verein/985/saison_id/2021",        
                                         "https://www.transfermarkt.com/tottenham-hotspur/startseite/verein/148/saison_id/2021",        
                                         "https://www.transfermarkt.com/fc-arsenal/startseite/verein/11/saison_id/2021",                
                                         "https://www.transfermarkt.com/leicester-city/startseite/verein/1003/saison_id/2021",          
                                         "https://www.transfermarkt.com/aston-villa/startseite/verein/405/saison_id/2021",              
                                         "https://www.transfermarkt.com/fc-everton/startseite/verein/29/saison_id/2021",                
                                         "https://www.transfermarkt.com/wolverhampton-wanderers/startseite/verein/543/saison_id/2021",  
                                         "https://www.transfermarkt.com/west-ham-united/startseite/verein/379/saison_id/2021",          
                                         "https://www.transfermarkt.com/brighton-amp-hove-albion/startseite/verein/1237/saison_id/2021",
                                         "https://www.transfermarkt.com/newcastle-united/startseite/verein/762/saison_id/2021",         
                                         "https://www.transfermarkt.com/leeds-united/startseite/verein/399/saison_id/2021",             
                                         "https://www.transfermarkt.com/fc-southampton/startseite/verein/180/saison_id/2021",           
                                         "https://www.transfermarkt.com/fc-brentford/startseite/verein/1148/saison_id/2021",            
                                         "https://www.transfermarkt.com/crystal-palace/startseite/verein/873/saison_id/2021",           
                                         "https://www.transfermarkt.com/norwich-city/startseite/verein/1123/saison_id/2021",            
                                         "https://www.transfermarkt.com/fc-watford/startseite/verein/1010/saison_id/2021",              
                                         "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2021" ))

stat_fra <- tm_squad_stats(team_url = c("https://www.transfermarkt.com/fc-paris-saint-germain/startseite/verein/583/saison_id/2021",
                                         "https://www.transfermarkt.com/olympique-lyon/startseite/verein/1041/saison_id/2021",       
                                         "https://www.transfermarkt.com/as-monaco/startseite/verein/162/saison_id/2021" ,            
                                         "https://www.transfermarkt.com/losc-lille/startseite/verein/1082/saison_id/2021" ,          
                                         "https://www.transfermarkt.com/fc-stade-rennes/startseite/verein/273/saison_id/2021",       
                                         "https://www.transfermarkt.com/olympique-marseille/startseite/verein/244/saison_id/2021",   
                                         "https://www.transfermarkt.com/ogc-nizza/startseite/verein/417/saison_id/2021" ,            
                                         "https://www.transfermarkt.com/rc-lens/startseite/verein/826/saison_id/2021",               
                                         "https://www.transfermarkt.com/stade-reims/startseite/verein/1421/saison_id/2021",          
                                         "https://www.transfermarkt.com/montpellier-hsc/startseite/verein/969/saison_id/2021",       
                                         "https://www.transfermarkt.com/rc-strassburg-alsace/startseite/verein/667/saison_id/2021",  
                                         "https://www.transfermarkt.com/fc-girondins-bordeaux/startseite/verein/40/saison_id/2021",  
                                         "https://www.transfermarkt.com/fc-nantes/startseite/verein/995/saison_id/2021"  ,           
                                         "https://www.transfermarkt.com/as-saint-etienne/startseite/verein/618/saison_id/2021",      
                                         "https://www.transfermarkt.com/stade-brest-29/startseite/verein/3911/saison_id/2021",      
                                         "https://www.transfermarkt.com/fc-metz/startseite/verein/347/saison_id/2021" ,              
                                         "https://www.transfermarkt.com/sco-angers/startseite/verein/1420/saison_id/2021",           
                                         "https://www.transfermarkt.com/es-troyes-ac/startseite/verein/1095/saison_id/2021",         
                                         "https://www.transfermarkt.com/fc-lorient/startseite/verein/1158/saison_id/2021",           
                                         "https://www.transfermarkt.com/clermont-foot-63/startseite/verein/3524/saison_id/2021"))
## valori di mercato -------------------------------------------------------


vdm_ita <- tm_player_market_values(country_name = "Italy",start_year = 2021)

vdm_spa <- tm_player_market_values(country_name = "Spain",start_year = 2021)

vdm_ger <- tm_player_market_values(country_name = "Germany",start_year = 2021)

vdm_ing <- tm_player_market_values(country_name = "England",start_year = 2021)

vdm_fra <- tm_player_market_values(country_name = "France",start_year = 2021)


### unione dataset ----------------------------------------------------------


ita <- merge(x = stat_ita,y = vdm_ita,by = 'player_name',all = T)

spa <- merge(x = stat_spa,y = vdm_spa,by = 'player_name', all = T)

ger <- merge(x = stat_ger,y = vdm_ger,by = 'player_name',all = T)

ing <- merge(x = stat_ing,y = vdm_ing,by = 'player_name',all = T)

fra <- merge(x = stat_fra,y = vdm_fra,by = 'player_name',all = T)

ds <- rbind(ita,fra,ger,spa,ing)

colSums(is.na(ds))

### selezione u.s. ---------------------------------------------

ds[77,7]=19
ds[632,7]=17
ds[1227,7]=17
ds[1302,7]=18
ds[1575,7]=19
ds[1698,7]=15

young <- which(ds$player_age.x<22)

ds <- ds[young,]

min <- which(ds$minutes_played>90)

ds <- ds[min,]

n <- c(1:552)

rownames(ds) <- n

#### eliminazione doppioni --------------------------------------------


which(table(ds$player_name)>1)

ds <- ds[-c(11,26:27,31,66:67,83:84,87:88,93:94,97:98,101:102,109,126,135:138,
            178,199:200,269:270,278,315,347,375,383,402,461,463),]

n <- c(1:517)

rownames(ds) <- n


### selezione variabili --------------------------------------------------


#### variabili ridondanti ----------------------------------------------------

ds <- ds[, -c(3,
              5,
              15,
              17,
              19,
              21:22)]
colSums(is.na(ds))

#### variabili con una sola modalità-----------------------------------------

ds <- ds[, -c(12,
              13)]
colSums(is.na(ds))

#### variabili non interessanti -------------------------

ds <- ds[,-c(3,
             7,
             12:18,
             21)]
colSums(is.na(ds))

n <- c(1:517)

rownames(ds) <- n

### unione giocatori trasferiti -----------------------------

which(table(ds$player_name)>1)

ds[243,2] <- "Eintracht Frankfurt/Borussia Dortmund"
ds[243,c(6,7,8)] <- ds[243,c(6,7,8)]+ds[244,c(6,7,8)]

ds[116,2] <- "OGC Nice/Angers SCO"
ds[116,c(6,7,8)] <- ds[116,c(6,7,8)]+ds[117,c(6,7,8)]

ds[336,2] <- "Valencia CF/Tottenham Hotspur"
ds[336,c(6,7,8)] <- ds[336,c(6,7,8)]+ds[430,c(6,7,8)]
ds[336,9] <- "LaLiga/Premier League"

ds[19,2] <- "Juventus FC/Tottenham Hotspur"
ds[19,c(6,7,8)] <- ds[19,c(6,7,8)]+ds[447,c(6,7,8)]
ds[19,9] <- "Serie A/Premier League"

ds[24,2] <- "Juventus FC/ACF Fiorentina"
ds[24,c(6,7,8)] <- ds[24,c(6,7,8)]+ds[25,c(6,7,8)]

ds[134,2] <- "Stade Rennais FC/Real Madrid"
ds[134,c(6,7,8)] <- ds[134,c(6,7,8)]+ds[342,c(6,7,8)]
ds[134,9] <- "Ligue 1/LaLiga"

ds[346,2] <- "FC Barcelona/Manchester City"
ds[346,c(6,7,8)] <- ds[346,c(6,7,8)]+ds[452,c(6,7,8)]
ds[346,9] <- "LaLiga/Premier League"

ds[266,2] <- "RB Leipzig/Valencia CF"
ds[266,c(6,7,8)] <- ds[266,c(6,7,8)]+ds[356,c(6,7,8)]
ds[266,9] <- "Bundesliga/LaLiga"

ds[175,2] <- "OGC Nice/Clermont Foot 63"
ds[175,c(6,7,8)] <- ds[175,c(6,7,8)]+ds[176,c(6,7,8)]

ds[61,2] <- "Cagliari Calcio/Atalanta BC"
ds[61,c(6,7,8)] <- ds[61,c(6,7,8)]+ds[62,c(6,7,8)]

ds[76,2] <- "AC Milan/Torino FC"
ds[76,c(6,7,8)] <- ds[76,c(6,7,8)]+ds[77,c(6,7,8)]

ds[78,2] <- "US Salernitana 1919/UC Sampdoria"
ds[78,c(6,7,8)] <- ds[78,c(6,7,8)]+ds[79,c(6,7,8)]

ds[84,2] <- "Atalanta BC/Genoa CFC"
ds[84,c(6,7,8)] <- ds[84,c(6,7,8)]+ds[85,c(6,7,8)]

ds[86,2] <- "Torino FC/FC Empoli"
ds[86,c(6,7,8)] <- ds[86,c(6,7,8)]+ds[87,c(6,7,8)]

ds <- ds[-c(25,62,77,79,85,87,117,176,244,342,356,430,447,452),]

n <- c(1:503)

rownames(ds) <- n

which(table(ds$player_name)>1)

## NA --------------------------------------------------------------


cambio_data <- which(is.na(ds$contract_expiry))
nacontr <- ds[cambio_data,]

vdmna <- which(is.na(ds$player_market_value_euro))
navdm <- ds[vdmna,]


### interpolazione -------------------------------------------------------

#### valori di mercato -------------------------------------------------------

ds[329,11] <- 25000
#valore risalente al 26/9/22

ds[345,11] <- 25000
#valore risalente al 26/9/22

ds[349,11] <- 300000
#valore risalente al 26/12/22

ds[356,11] <- 400000
#valore risalente al 23/9/22

ds[362,11] <- 50000
#valore risalente al 26/9/22

ds[398,11] <- 50000
#valore risalente al 26/9/22

ds[465,11] <- 100000
#valore risalente al 3/11/22

ds[472,11] <- 50000000
#valore risalente al 23/12/21

ds[496,11] <- 350000
#valore risalente al 5/4/23

#### contratti ---------------------------------------------------------------

ds$contract_expiry <- format(as.Date(ds$contract_expiry),"%Y")

ds[36,10] <- 2027
#https://twitter.com/NicoSchira/status/1562768850554613760?ref_src=twsrc%5Etfw%7Ctwcamp%5Etweetembed%7Ctwterm%5E1562768850554613760%7Ctwgr%5Ecf6e6037dae66eda603a23c71747a13b176ce6d2%7Ctwcon%5Es1_&ref_url=https%3A%2F%2Fwww.nicoloschira.com%2Fcalciomercato%2Fcremonese-le-cifre-dell-acquisto-di-felix-afena-gyan-11606

ds[52,10] <- 2026
#https://www.teamtalk.com/brighton/unused-defender-leo-ostigard-joins-napoli-director-development-system-thriving
#https://football-italia.net/figures-behind-ostigards-contract-with-napoli-report/

ds[340,10] <- 2026
#https://www.fcbarcelona.com/en/news/2790083/gavi-renews-contract-with-fc-barcelona-until-2026

ds[413,10] <- 2026
#https://cominghomenewcastle.sbnation.com/2023/1/29/23575438/official-anthony-gordon-joins-newcastle-united-from-everton#:~:text=Newcastle%20United%20confirmed%20the%20signing,be%20around%20%E2%82%AC5M%2B

### eliminazione giocatori con variabili necessarie mancanti ----------------

ds <- ds[-c(30,37,294,374,442,460),]

colSums(is.na(ds))

n <- c(1:497)

rownames(ds) <- n

## riordinamento dataset ---------------------------------------------------




### cambio formato data -----------------------------------------------------

v <- rep(2022, 497)

str(ds)

ds$contract_expiry <- as.numeric(ds$contract_expiry)

ds$player_market_value_euro <- as.numeric(ds$player_market_value_euro)

ds$contract_expiry <- ds$contract_expiry-v

### riordinamento colonne ---------------------------------------------------

ds <- ds[,c(1:2,9,5,3,4,6:8,10:11)]

### rinomina colonne -----------------------------------------------------

colnames(ds)[c(1:11)] <- c("Giocatore","Squadra","Campionato","Nazionalità","Ruolo",
                           "Età","Presenze","Gol","Minuti_giocati","Scadenza_di_contratto",
                           "Valore_di_mercato")


## descrizione dataset -----------------------------------------------------

unique(ds$league)
sort(unique(ds[ds$league=="Serie A",2]))
#manca Inter Milan

sort(unique(ds[ds$league=="Bundesliga",2]))
#manca 1.FC Union Berlin

unique(ds$nationality)
#57 nazioni diverse

sort(unique(ds$position))

unique(ds$player_market_value_euro)

# ANALISI DESCRITTIVE -----------------------------------------------------

summary(ds[,c(6:11)])

pairs(ds[,c(7:9,11)], lwd = 3, pch = 16, cex = 1.25, col = "red", gap = 0, xaxt = "n", yaxt = "n",
      )

sort(table(ds$Ruolo))

sort(table(ds$Campionato))

sort(table(ds$Nazionalità))

# ALBERI DI REGRESSIONE ---------------------------------------------------

library(rpart)

library(rpart.plot)

set.seed(8174)

tree <- rpart(`Valore_di_mercato` ~ `Presenze`+`Gol`+`Minuti_giocati`+`Età` +
                `Scadenza_di_contratto`+`Ruolo`, data=ds, control=rpart.control(cp=.0001))

#prp(tree, extra = 1, faclen=0, roundint = F, digits = 2, box.col=c("#ABCDEF", "#FFD800", "#DDADAF", "#B2B2B2")[findInterval(tree$frame$yval, v = c(0,10000000,30000000,50000000))])

tree$cptable
#tabella con l'errore per ogni numero di split

#summary(tree)
#riassume tutti i passaggi principali, tutte le suddivisioni dei nodi...

plotcp(tree)
abline(v= 7, col= 'red', lty= 'dashed')
#grafico per vedere il costo complessità dei nodi

best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
#minimo dell'errore tra tutti gli split

pruned_tree <- prune(tree, cp=best)
#potatura dell'albero

prp(pruned_tree, extra = 1, faclen=0, roundint = F,
    digits = 2, box.col=c("#ABCDEF", "#FFD800", "#DDADAF", "#B2B2B2")
    [findInterval(pruned_tree$frame$yval, v = c(0,10000000,30000000,50000000))])
#grafico dell'albero

# ANALISI DEI GRUPPI ------------------------------------------------------

## metodo gerarchico -------------------------------------------------------


Z <- scale(ds[,c(6:11)])

matr_dist <- dist(Z,method = "euclidean")

library(cluster)

gr.gioc <- hclust(matr_dist,method = "ward.D")

NbClust::NbClust(data = matr_dist, min.nc = 2,max.nc = 5,method = "ward.D",
                 index = "silhouette")

plot(gr.gioc,main = "Dendrogramma giocatori",
     sub = "Metodo di Ward")
rect.hclust(gr.gioc,k=4)

## kmedie ------------------------------------------------------------------
library(tidyverse)  
library(factoextra)

kmedie.2 <- kmeans(Z,2)
table(kmedie.2$cluster)
kmedie.gioc <- kmeans(Z,4)
table(kmedie.gioc$cluster)

ds[,c(6:11)] %>%
  mutate(Cluster = kmedie.2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

ds$gruppi <- factor(kmedie.gioc$cluster,labels = 
                        c('gr1','gr2','gr3','gr4'))

library(dplyr)

ds[,c(6:11)] %>%
  mutate(Cluster = kmedie.gioc$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

library(ggplot2)
library(ggridges)
theme_set(theme_ridges())

plot1 <- ggplot(ds, aes(x = Età, y = gruppi)) +
  geom_density_ridges(aes(fill = gruppi), show.legend = F) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#293133", "#E52B50"))

plot2 <- ggplot(ds, aes(x = Presenze, y = gruppi)) +
  geom_density_ridges(aes(fill = gruppi), show.legend = F) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#293133", "#E52B50"))

plot3 <- ggplot(ds, aes(x = Gol, y = gruppi)) +
  geom_density_ridges(aes(fill = gruppi), show.legend = F) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#293133", "#E52B50"))

plot4 <- ggplot(ds, aes(x = Minuti_giocati, y = gruppi)) +
  geom_density_ridges(aes(fill = gruppi), show.legend = F) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#293133", "#E52B50"))

plot5 <- ggplot(ds, aes(x = Scadenza_di_contratto, y = gruppi)) +
  geom_density_ridges(aes(fill = gruppi), show.legend = F) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#293133", "#E52B50"))

plot6 <- ggplot(ds, aes(x = Valore_di_mercato, y = gruppi)) +
  geom_density_ridges(aes(fill = gruppi), show.legend = F) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#293133", "#E52B50"))

library(gridExtra)

grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6)

gruppo1 <- subset(ds,gruppi=="gr1")

gruppo2 <- subset(ds,gruppi=="gr2")

gruppo3 <- subset(ds,gruppi=="gr3")

gruppo4 <- subset(ds,gruppi=="gr4")

italiani <- subset(ds,Nazionalità == "Italy")

library(ggpubr)

ggplot(italiani, aes(gruppi)) +
  geom_bar(fill = c("#00AFBB", "#E7B800", "#293133", "#E52B50")) +
  theme_pubclean()

prova <- ds

prova <- prova %>%
  mutate(Nazionalità = ifelse(!Nazionalità %in%  c("Italy","Spain","Germany",
                                                    "France","England"),
                              "Altro", Nazionalità))

sort(table(prova$Nazionalità))

ggplot(prova, aes(fill=gruppi, x=Nazionalità)) + 
  geom_bar(position="stack")+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#293133", "#E52B50"))

ggplot(prova, aes(fill=gruppi, x=Nazionalità)) + 
  geom_bar(position="fill")+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#293133", "#E52B50"))

colSums(table(ds$gruppi,ds$Campionato))

