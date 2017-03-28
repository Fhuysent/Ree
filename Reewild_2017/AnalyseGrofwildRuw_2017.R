# open libs
library(ggplot2)
library(INBOtheme)
library(plyr)
library(functional)
library(reshape2)
library(scales)
library(gridExtra)
library(sqldf)
library(reshape)
library(RODBC)
library(RPostgreSQL)

setwd("Q:/OG_Faunabeheer/Projecten/Lopende projecten/INBOPRJ-10218 - Analyse en rapportage van jachtwilddata in Vlaanderen/Grofwild/Ree/R/2017")

# 1. Gegevens ophalen
# --------------------------------------------------------------------------------------------------------------------------

#maak verbinding####
#Run connect batch file first
drv <- dbDriver("PostgreSQL")

verbinding <- dbConnect(drv, dbname="autopsies",host="127.0.0.1",port=5432,user="frank",password="lQzgDqS5kFiXdUFTqw9UorObsL0WBVXLwZONoFWtIs" )

# voor de meldingsformulieren grofwild:
dbo_Meldingsformulier <- dbReadTable(verbinding, c("public", "meldingsformulier"))
dbo_Groep <- dbReadTable(verbinding, c("public", "groep"))
dbo_Leeftijd <- dbReadTable(verbinding, c("public", "leeftijd"))
dbo_Geslacht <- dbReadTable(verbinding, c("public", "geslacht"))
dbo_Jachtmethode <- dbReadTable(verbinding, c("public", "jachtmethode"))
dbo_WettelijkKader <- dbReadTable(verbinding, c("public", "wettelijk_kader"))
dbo_Eloket <- dbReadTable(verbinding, c("public", "eloket"))
dbo_Diersoort <- dbReadTable(verbinding, c("public", "diersoort"))
dbo_RapportStatus <- dbReadTable(verbinding, c("public", "rapport_status"))
dbo_Rapport <- dbReadTable(verbinding, c("public", "rapport"))

# voor de identificaties grofwild:
dbo_Identificatie <- dbReadTable(verbinding, c("public", "identificatie"))
dbo_Staal <- dbReadTable(verbinding, c("public", "staal"))
dbo_Doodsoorzaak <- dbReadTable(verbinding, c("public", "doodsoorzaak"))
dbo_Vindplaats <- dbReadTable(verbinding, c("public", "vindplaats"))
dbo_Onderkaak <- dbReadTable(verbinding, c("public", "onderkaak"))

# Close PostgreSQL connection 
dbDisconnect(verbinding)


#2. Gegevens opkuisen
#-----------------------------------------------------------------------------------------------------------------

##Versie=NA is ingegeven door Erik, maar krijgen ook LaatsteVersi=1, dus filter op LaatsteVersie
Meldingen <- subset(dbo_Meldingsformulier, laatste_versie==TRUE)
##RapportStatus 3 en 4 zitten er niet meer in, wel nog NA's, die mogen 2 worden
#Meldingen$RapportStatusID[is.na(Meldingen$RapportStatusID)] <- 5
#Meldingen <- subset(Meldingen, RapportStatusID!=1)
##Enkel Ree
Meldingen <-subset(Meldingen, diersoort_id==5)
##Controle afschotdatum en jaar labelnummer komen overeen?
controle <- Meldingen
controle$test <- substr(controle$label_nummer,5,8)
controle$test2 <- as.character(controle$afschot_datum, "%Y")
controle$test <- as.numeric(controle$test)
controle$test2 <- as.numeric(controle$test2)
controle$test3 <- controle$test-controle$test2
table(controle$test3)
fouten <- subset(controle, test3!=0)
remove(controle)

Meldingen$Jaar <- as.character(Meldingen$afschot_datum, "%Y")

# Data omzetten naar namen in script
Meldingen$NummerAfschotplan <- Meldingen$nummer_afschotplan
Meldingen$Provincie <- "Limburg"
Meldingen$Provincie[substr(Meldingen$nummer_afschotplan,1,2)=="AN"] <- "Antwerpen"
Meldingen$Provincie[substr(Meldingen$nummer_afschotplan,1,2)=="VB"] <- "Vlaams-Brabant"
Meldingen$Provincie[substr(Meldingen$nummer_afschotplan,1,2)=="WV"] <- "West-Vlaanderen"
Meldingen$Provincie[substr(Meldingen$nummer_afschotplanr,1,2)=="OV"] <- "Oost-Vlaanderen"
Meldingen$LabelNummer <- Meldingen$label_nummer
Meldingen$Jaar <- factor(Meldingen$Jaar)
Meldingen$Labeltype <- substr(Meldingen$label_nummer,13,13)
Meldingen$Labeltype[Meldingen$Labeltype=="B"] <- "Bokken"
Meldingen$Labeltype[Meldingen$Labeltype=="G"] <- "Geiten"
Meldingen$Labeltype[Meldingen$Labeltype=="K"] <- "Kitsen"
Meldingen$Datum <- as.Date(Meldingen$afschot_datum, "%d/%m/%Y")
Meldingen$Type <- "Geit"
Meldingen$Type[Meldingen$geslacht_id==1 & Meldingen$leeftijd_id==17] <- "Bokkits"
Meldingen$Type[Meldingen$geslacht_id==1 & Meldingen$leeftijd_id==18] <- "Jaarlingbok"
Meldingen$Type[Meldingen$geslacht_id==1 & Meldingen$leeftijd_id==19] <- "Bok"
Meldingen$Type[Meldingen$geslacht_id==2 & Meldingen$leeftijd_id==17] <- "Geitkits"
Meldingen$Type[Meldingen$geslacht_id==2 & Meldingen$leeftijd_id==18] <- "Smalree"
Meldingen$Type <- factor(Meldingen$Type, levels=c("Bokkits","Geitkits","Jaarlingbok","Smalree","Bok","Geit"))
Meldingen$Gewicht <- Meldingen$ontweid_gewicht
Meldingen$OKG <- rowMeans(subset(Meldingen, select = c(onderkaaklengte_links, onderkaaklengte_rechts)), na.rm = TRUE)
Meldingen$TotEmbryo <- Meldingen$aantal_embryos

##Overzicht wegschrijven voor analyses Afzonderlijk
tmp <- Meldingen[,c(36,38,41)]
tmp$Type2[tmp$Type=="Bokkits"] <- "RK"
tmp$Type2[tmp$Type=="Geitkits"] <- "RK"
tmp$Type2[tmp$Type=="Geit"] <- "RG"
tmp$Type2[tmp$Type=="Smalree"] <- "RG"
tmp$Type2[tmp$Type=="Bok"] <- "RB"
tmp$Type2[tmp$Type=="Jaarlingbok"] <- "RB"
tmp$Type2 <- factor(tmp$Type2)
tmp2 <- ddply(tmp, c("NummerAfschotplan","Type2"), summarise,
              value=length(LabelNummer))
tmp2 <- cast(tmp2, NummerAfschotplan~Type2, sum)
setwd("Q:/OG_Faunabeheer/Projecten/Lopende projecten/INBOPRJ-10218 - Analyse en rapportage van jachtwilddata in Vlaanderen/Grofwild/Ree/R/2017")
write.csv(tmp2, file="Meldingen_2017.csv")
remove(tmp, tmp2)

##Jaren tussen 2013 en huidig jaar apart houden voor vergelijking met voorbije jaren
Meldingen2015  <- subset(Meldingen, Jaar==2014|Jaar==2015)
##Vanaf hier gaan we verder met enkel 2016
Meldingen2016 <- subset(Meldingen, Jaar==2016)

Dubbels <-as.data.frame(table(Meldingen$LabelNummer))
Dubbels <- subset(Dubbels, Freq>1)
names(Dubbels)[names(Dubbels)=="Var1"] <- "LabelNummer"
Dubbels <- merge(Dubbels,Meldingen, by="LabelNummer", all.x=TRUE)
Dubbels <- Dubbels[c(1,2,10)]

###Overzichtstabel 2016 voor WBE's
tabel <- ddply(Meldingen, c("NummerAfschotplan","Type"), summarise,
               Aantal = length(LabelNummer))
tabel <- cast(tabel, NummerAfschotplan~Type)
tabel$Totaal <- rowSums(tabel, na.rm=TRUE)
write.csv(tabel, file="Afschot_WBEs_2016.csv")

Meldingen$controlejaar <- substr(Meldingen$NummerAfschotplan, 7,10)
controle <- subset(Meldingen, controlejaar!=2016)
controle <- controle[c(8:12,15)]
Meldingen$controlenummer <- substr(Meldingen$NummerAfschotplan, 12,15)
Meldingen$controlenummer <- as.numeric(Meldingen$controlenummer)
controle <- subset(Meldingen, controlenummer>100)
controle <- controle[c(8:12,15)]

remove(Dubbels)
Meldingen2015 <- Meldingen2015[c(35:44, 11)]
Meldingen2016 <- Meldingen2016[c(35:44, 11)]
names(Meldingen2015)[11] <- paste("PostcodeAfschotLocatie")
names(Meldingen2016)[11] <- paste("PostcodeAfschotLocatie")
names(Meldingen2015)[2] <- paste("Registernr")
names(Meldingen2016)[2] <- paste("Registernr")
names(Meldingen2015)[4] <- paste("Labelnummer")
names(Meldingen2016)[4] <- paste("Labelnummer")

##Inladen gegevens jaren tot en met 2013
Meldingen2013 <- read.csv("Q:/OG_Faunabeheer/Projecten/Lopende projecten/INBOPRJ-10218 - Analyse en rapportage van jachtwilddata in Vlaanderen/Grofwild/Ree/Data/Meldingen_voorbijejaren_postcode.csv",sep=";", dec=",")
Meldingen2013$Jaar <- factor(Meldingen2013$Jaar)
Meldingen2013$Datum <- as.Date(Meldingen2013$Datum, "%d/%m/%Y")
Meldingen2013$Type <- factor(Meldingen2013$Type)

MeldingenAll <- rbind(Meldingen2013,Meldingen2015, Meldingen2016)

MeldingenAll$Maand <- as.numeric(format(MeldingenAll$Datum, format = "%m"))
MeldingenAll$Dag <- as.numeric(format(MeldingenAll$Datum, format = "%d"))
write.csv(MeldingenAll, file="MeldingenAll.csv")


######GESCHOTEN OP#######
d <- subset(MeldingenAll, Labeltype=="Bokken"|Labeltype=="Geiten"|Labeltype=="Kitsen")
d <- subset(d, Maand>0)

d$Maand <- as.factor(d$Maand)
d$Bin <- NA
d$Bin[d$Maand==1 & d$Dag <= 15] <- 1
d$Bin[d$Maand==1 & d$Dag > 15] <- 2
d$Bin[d$Maand==2 & d$Dag <= 14] <- 3
d$Bin[d$Maand==2 & d$Dag > 14] <- 4
d$Bin[d$Maand==3 & d$Dag <= 15] <- 5
d$Bin[d$Maand==3 & d$Dag > 15] <- 6
d$Bin[d$Maand==4 & d$Dag <= 15] <- 7
d$Bin[d$Maand==4 & d$Dag > 15] <- 8
d$Bin[d$Maand==5 & d$Dag <= 15] <- 9
d$Bin[d$Maand==5 & d$Dag > 15] <- 10
d$Bin[d$Maand==6 & d$Dag <= 15] <- 11
d$Bin[d$Maand==6 & d$Dag > 15] <- 12
d$Bin[d$Maand==7 & d$Dag <= 15] <- 13
d$Bin[d$Maand==7 & d$Dag > 15] <- 14
d$Bin[d$Maand==8 & d$Dag <= 15] <- 15
d$Bin[d$Maand==8 & d$Dag > 15] <- 16
d$Bin[d$Maand==9 & d$Dag <= 15] <- 17
d$Bin[d$Maand==9 & d$Dag > 15] <- 18
d$Bin[d$Maand==10 & d$Dag <= 15] <- 19
d$Bin[d$Maand==10 & d$Dag > 15] <- 20
d$Bin[d$Maand==11 & d$Dag <= 15] <- 21
d$Bin[d$Maand==11 & d$Dag > 15] <- 22
d$Bin[d$Maand==12 & d$Dag <= 15] <- 23
d$Bin[d$Maand==12 & d$Dag > 15] <- 24
d$Bin <- as.factor(d$Bin)
d$Maand <- as.factor(d$Maand)
d$Bin <- as.numeric(d$Bin)

d2 <- ddply(d, c("Jaar"), summarise,
               Afschot   = length(Labelnummer))

d3 <- ddply(d, c("Jaar","Bin"), summarise,
            Afschotdeel = length(Labelnummer))

d4 <- merge(d2,d3,by=c("Jaar"))
d4$Aandeel <- (d4$Afschotdeel/d4$Afschot)*100 

d5 <- subset (d4,Jaar==2015)
d4 <- subset (d4,Jaar!=2015)

d5 <- ddply(d5, c("Bin"), summarise,
            Jaar2015    = mean(Aandeel))

d4 <- ddply(d4, c("Bin"), summarise,
            VorigeJaren   = mean(Aandeel),
            N = length(Aandeel),
            upper = max(Aandeel),
            lower = min(Aandeel)
            )


d6 <- merge(d4,d5, by="Bin", all.x=TRUE)

d6$VorigeJaren[d6$Bin==7] <- NA
d6$VorigeJaren[d6$Bin==8] <- NA
d6$VorigeJaren[d6$Bin==18] <- NA
d6$VorigeJaren[d6$Bin==19] <- NA
d6$VorigeJaren[d6$Bin==20] <- NA
d6$VorigeJaren[d6$Bin==21] <- NA
d6$VorigeJaren[d6$Bin==22] <- NA
d6$VorigeJaren[d6$Bin==23] <- NA
d6$Jaar2015[d6$Bin==7] <- NA
d6$Jaar2015[d6$Bin==8] <- NA
d6$Jaar2015[d6$Bin==18] <- NA
d6$Jaar2015[d6$Bin==19] <- NA
d6$Jaar2015[d6$Bin==20] <- NA
d6$Jaar2015[d6$Bin==21] <- NA
d6$Jaar2015[d6$Bin==22] <- NA
d6$Jaar2015[d6$Bin==23] <- NA
d6$upper[d6$Bin==7] <- NA
d6$upper[d6$Bin==8] <- NA
d6$upper[d6$Bin==18] <- NA
d6$upper[d6$Bin==19] <- NA
d6$upper[d6$Bin==20] <- NA
d6$upper[d6$Bin==21] <- NA
d6$upper[d6$Bin==22] <- NA
d6$upper[d6$Bin==23] <- NA
d6$lower[d6$Bin==7] <- NA
d6$lower[d6$Bin==8] <- NA
d6$lower[d6$Bin==18] <- NA
d6$lower[d6$Bin==19] <- NA
d6$lower[d6$Bin==20] <- NA
d6$lower[d6$Bin==21] <- NA
d6$lower[d6$Bin==22] <- NA
d6$lower[d6$Bin==23] <- NA
d6$Bin <- as.numeric(d6$Bin)


ggplot(d6, aes(x=Bin)) +
  geom_bar(aes(y=VorigeJaren), stat="identity", alpha=1, width=0.75) +
  geom_point(aes(y=Jaar2015), size=6, colour="#688599") +
  geom_errorbar(aes(ymax=upper, ymin=lower), colour="brown", width=0.2)+
  annotate("rect", xmin=0.5 , xmax=6.5, ymin=27, ymax=29, alpha=1, fill="grey80")+
  geom_text(aes(x= 3.5, y = 28, label = "geit en kits"),colour="brown", size=4) +
  annotate("rect", xmin=8.5, xmax=17.5, ymin=27, ymax=29, alpha=1, fill="grey80")+
  geom_text(aes(x= 13, y = 28, label = "bok"), colour="brown", size=4) +
  ylab("Percentage ree?n in het jaarlijks afschot") +
  xlab(NULL) +
  theme_INBO()+
  scale_x_continuous(limits=c(0.5,24.5), breaks=c(0.5,2.5,4.5,6.5,8.5,10.5,12.5,14.5,16.5,18.5,20.5,22.5,24.5),
  labels=c("Januari","Februari","Maart","April","Mei","Juni","Juli","Augustus","September","Oktober","November","December","")) +
  theme(axis.text.x = element_text(angle=90, hjust=0, vjust=3.5))+
  coord_cartesian(ylim = c(0, 30))

###Working directory voor opslaan figuren
setwd("Q:/OG_Faunabeheer/Projecten/Lopende projecten/INBOPRJ-10218 - Analyse en rapportage van jachtwilddata in Vlaanderen/Grofwild/Ree/Figuren")

###Figuur###
ggsave(p, file = "AfschotPerMaand.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)
ggsave(p, file = "AfschotPerMaand.eps", width = 22.4 /2.54, height = 15 / 2.54)

remove(p)
remove(d)
remove(d2)
remove(d3)
remove(d4)
remove(d5)
remove(d6)

##########################
########GEWICHT##########

#plot
Gewicht <- subset(Meldingen, Gewicht!="NA")
Gewicht <- subset(Gewicht, Gewicht<30.1)
Gewicht <- subset(Gewicht, Gewicht>4.9)
Gewicht <- subset(Gewicht, Jaar=="2016")

ggplot(Gewicht, aes(x=Type, y=Gewicht)) + 
  geom_boxplot(width=0.75, colour=INBOreddishbrown, outlier.colour=INBOreddishbrown) + 
  ylab("Gewicht (kg)") +
  scale_x_discrete(limits=c("Geitkits","Bokkits","Jaarlingbok","Smalree","Geit","Bok"))+
  scale_y_continuous(breaks=c(5,10,15,20,25)) +
  theme_INBO()

summary(Gewicht$Gewicht~Gewicht$Type)


###Figuur###
ggsave(p, file = "GewichtPerType.jpg", width = 22.4 /2.54, height = 12 / 2.54, dpi=300)
ggsave(p, file = "GewichtPerType.eps", width = 22.4 /2.54, height = 12 / 2.54)

remove(Gewicht)
remove(p)


#######ONDERKAAKLENGTE########

Onderkaak <- Meldingen
Onderkaak$OKG <- as.numeric(Onderkaak$OKG) 
Onderkaak <- subset(Onderkaak, OKG>100)
Onderkaak <- subset(Onderkaak, OKG<200)
Onderkaak <- subset(Onderkaak, Jaar=="2016")

###plot

ggplot(Onderkaak, aes(x=Type, y=OKG)) + 
  geom_boxplot(width=0.75, colour=INBOreddishbrown, outlier.colour=INBOreddishbrown) + 
  ylab("Onderkaaklengte (mm)") +
  scale_x_discrete(limits=c("Geitkits","Bokkits","Jaarlingbok","Smalree","Geit","Bok"))+
  scale_y_continuous(breaks=c(125,150,175)) +
  theme_INBO()

summary(Onderkaak$OKG~Onderkaak$Type)

###Figuur###
ggsave(p, file = "OKLPerType.jpg", width = 22.4 /2.54, height = 12 / 2.54, dpi=300)
ggsave(p, file = "OKLPerType.eps", width = 22.4 /2.54, height = 12 / 2.54)

remove(p)
remove(Onderkaak)


#############################
#Grafiek zwangere dieren#####
#############################

DRACHT<- read.csv("EmbryosOverzicht.csv", sep=";", dec=",")
DRACHT <- subset(MeldingenAll, Type=="Smalree"|Type=="Geit")
DRACHT$Jaar <- as.factor(DRACHT$Jaar)

DRACHT$Embryo[DRACHT$TotEmbryo==0] <- 0
DRACHT$Embryo[DRACHT$TotEmbryo==1] <- 1
DRACHT$Embryo[DRACHT$TotEmbryo==2] <- 2
DRACHT$Embryo[DRACHT$TotEmbryo==3] <- 3
DRACHT$Embryo[is.na(DRACHT$TotEmbryo)] <- "NIET INGEVULD"
DRACHT$Embryo[DRACHT$TotEmbryo>=4] <- "FOUT"
DRACHT <- subset(DRACHT, Embryo!="FOUT")

DRACHT2 <- ddply(DRACHT, c("Jaar","Embryo"), summarise,
                  Aantal = length(Labelnummer))

DRACHT2b <- subset(DRACHT2, Embryo!="NIET INGEVULD")
DRACHT2b <- ddply(DRACHT2b, c("Jaar"), summarise,
                  Totaal = sum(Aantal))

DRACHT2c <- ddply(DRACHT2, c("Jaar"), summarise,
                  TotaalAantal = sum(Aantal))

DRACHT2 <- merge(DRACHT2,DRACHT2b, by=c("Jaar"))
DRACHT2 <- merge(DRACHT2,DRACHT2c, by=c("Jaar"))

remove(DRACHT2b)
remove(DRACHT2c)
DRACHT2$Percent <- DRACHT2$Aantal/DRACHT2$Totaal*100
DRACHT2b <- ddply(DRACHT2, c("Jaar"), transform, label_hoogte=cumsum(Aantal))
DRACHT2b$Label_hoogte_midden <- DRACHT2b$label_hoogte - (DRACHT2b$Aantal/2)
DRACHT2b$Percent <- ifelse(DRACHT2b$Embryo=="NIET INGEVULD",NA,DRACHT2b$Percent)
DRACHT2b$Embryo[DRACHT2b$Embryo=="NIET INGEVULD"] <- "Niet ingevuld"


cols <- c("Niet ingevuld"="grey70","0"="#989868", "1"="#688599", "2"="#CC3D3D", "3"="#EEB600")

ggplot(DRACHT2b, aes(x=Jaar, y=Aantal, fill=Embryo)) + 
  geom_bar(stat="identity", width=0.75) + 
  scale_fill_manual(values=cols) +
  ylab("Aantal vrouwelijke ree?n") + 
  guides(fill= guide_legend(reverse=TRUE, title="Aantal embryo's")) +
  geom_text(aes(y = Label_hoogte_midden, label = ifelse(!is.na(Percent),
                                                        paste(round(Percent, digits = 0), "%"),
                                                        NA)
                ),colour="white", size=3.5) +
  geom_text(aes(y = -20, label = TotaalAantal), size=3.5)  +
  scale_x_discrete(limits=c("2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"))+
  theme_INBO()

DRACHT3 <- subset(DRACHT, Embryo!="NIET INGEVULD")
DRACHT3$Embryo <- as.numeric(DRACHT3$Embryo)
DRACHT_tabel <- ddply(subset(DRACHT3, Embryo>0), c("Jaar"), summarise,
                      Aantal=length(Embryo),
                      Gemiddelde=mean(Embryo, na.rm=TRUE),
                      sd=sd(Embryo, na.rm=TRUE),
                      CI=qnorm(0.975)*sd/sqrt(Aantal))

###Figuur###
ggsave(p, file = "ZwangereDieren.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)
ggsave(p, file = "ZwangereDieren.eps", width = 22.4 /2.54, height = 15 / 2.54)

remove(p)
remove(DRACHT)
remove(DRACHT2)
remove(cols)

####gIS
# Haal bestand voor de koppeling met GIS (-> zie bijlage):
Postcodefile <- Meldingen
Koppeling <- read.csv("Q:/OG_Faunabeheer/Projecten/Lopende projecten/INBOPRJ-10218 - Analyse en rapportage van jachtwilddata in Vlaanderen/Grofwild/Ree/Data/deelgemeenten_GIS.csv",sep=";", dec=",")

# Probleem: eenzelfde postcode heeft verschillende deelgemeentenamen en bij een koppeling blaast hij dus het aantal records op
# -> oplossing: per code de fusiegemeente geven:
dim(sqldf("select Koppeling.CODE
          from Koppeling
          group by Koppeling.CODE
          "))

Koppeling_fusiegemeente <- sqldf("select CODE, NAMFGM_k
                                 from Koppeling i
                                 where rowid in (select rowid 
                                 from Koppeling
                                 where CODE = i.CODE
                                 order by NAMFGM_k ASC limit 1)
                                 order by i.CODE ASC
                                 ")

# -> nog problemen door koppelteken
# -> vervang deze namen in Koppeling_fusiegemeente:
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Dilsen-stokkem"] <- "Dilsen-Stokkem"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Hamont-achel"] <- "Hamont-Achel"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Hechtel-eksel"] <- "Hechtel-Eksel"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Heusden-zolder"] <- "Heusden-Zolder"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Houthalen-helchteren"] <- "Houthalen-Helchteren"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Meeuwen-gruitrode"] <- "Meeuwen-Gruitrode"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Tielt-winge"] <- "Tielt-Winge"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Watermaal-bosvoorde"] <- "Watermaal-Bosvoorde"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Wezembeek-oppem"] <- "Wezembeek-Oppem"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Wortegem-petegem"] <- "Wortegem-Petegem"


temp1 <- merge(Postcodefile, Koppeling_fusiegemeente, by.x = "PostcodeAfschotLocatie", by.y = "CODE", all.x = TRUE, all.y=F)
temp1 <- temp1[-c(15)]

temp2 <- sqldf("select temp1.NAMFGM_k, count(temp1.Labelnummer) as Aantal 
               from temp1
               group by temp1.NAMFGM_k
               order by temp1.NAMFGM_k")

write.csv2(temp2, file = "Afschot_ree_voor_GIS_2014.csv", row.names = FALSE)


###Figuur Verdeling Gemeenten

Meldingen2013PC <- read.csv("Q:/OG_Faunabeheer/Projecten/Lopende projecten/INBOPRJ-10218 - Analyse en rapportage van jachtwilddata in Vlaanderen/Grofwild/Ree/Data/Meldingen_voorbijejaren_postcode.csv",sep=";", dec=",")
MeldingenPC <- rbind(Meldingen2013PC, Meldingen2014)
Koppeling <- read.csv("Q:/OG_Faunabeheer/Projecten/Lopende projecten/INBOPRJ-10218 - Analyse en rapportage van jachtwilddata in Vlaanderen/Grofwild/Ree/Data/deelgemeenten_GIS.csv",sep=";", dec=",")

# Probleem: eenzelfde postcode heeft verschillende deelgemeentenamen en bij een koppeling blaast hij dus het aantal records op
# -> oplossing: per code de fusiegemeente geven:
dim(sqldf("select Koppeling.CODE
          from Koppeling
          group by Koppeling.CODE
          "))

Koppeling_fusiegemeente <- sqldf("select CODE, NAMFGM_k
                                 from Koppeling i
                                 where rowid in (select rowid 
                                 from Koppeling
                                 where CODE = i.CODE
                                 order by NAMFGM_k ASC limit 1)
                                 order by i.CODE ASC
                                 ")

# -> nog problemen door koppelteken
# -> vervang deze namen in Koppeling_fusiegemeente:
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Dilsen-stokkem"] <- "Dilsen-Stokkem"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Hamont-achel"] <- "Hamont-Achel"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Hechtel-eksel"] <- "Hechtel-Eksel"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Heusden-zolder"] <- "Heusden-Zolder"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Houthalen-helchteren"] <- "Houthalen-Helchteren"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Meeuwen-gruitrode"] <- "Meeuwen-Gruitrode"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Tielt-winge"] <- "Tielt-Winge"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Watermaal-bosvoorde"] <- "Watermaal-Bosvoorde"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Wezembeek-oppem"] <- "Wezembeek-Oppem"
Koppeling_fusiegemeente$NAMFGM_k[Koppeling_fusiegemeente$NAMFGM_k=="Wortegem-petegem"] <- "Wortegem-Petegem"


temp1 <- merge(MeldingenPC, Koppeling_fusiegemeente, by.x = "PostcodeAfschotLocatie", by.y = "CODE", all.x = TRUE, all.y=F)
temp1 <- temp1[-c(15)]

temp2 <- sqldf("select temp1.NAMFGM_k, temp1.Jaar, count(temp1.Labelnummer) as Aantal 
               from temp1
               group by temp1.NAMFGM_k, Jaar
               order by temp1.NAMFGM_k, Jaar")
##Gemeenten uit Brussel eruit
Koppeling_fusiegemeente2 <- subset(Koppeling_fusiegemeente, 
                                   NAMFGM_k!="Brussel"
                                   & NAMFGM_k!="Schaarbeek"
                                   & NAMFGM_k!="Ganshoren"
                                   & NAMFGM_k!="Jette"
                                   & NAMFGM_k!="Sint-agatha-berchem"
                                   & NAMFGM_k!="Koekelberg"
                                   & NAMFGM_k!="Sint-jans-molenbeek"
                                   & NAMFGM_k!="Anderlecht"
                                   & NAMFGM_k!="Sint-gillis"
                                   & NAMFGM_k!="Vorst"
                                   & NAMFGM_k!="Oudergem"
                                   & NAMFGM_k!="Ukkel"
                                   & NAMFGM_k!="Watermaal-Bosvoorde"
                                   & NAMFGM_k!="Sint-pieters-woluwe"
                                   & NAMFGM_k!="Etterbeek"
                                   & NAMFGM_k!="Sint-lambrechts-woluwe"
                                   & NAMFGM_k!="Sint-joost-ten-node"
                                   & NAMFGM_k!="Evere"
                                   & NAMFGM_k!="Elsene")
NAMFGM_k <- unique(Koppeling_fusiegemeente2$NAMFGM_k)
##blijkbaar ontbreekt Moerbeke
NAMFGM_k[308]<-"Moerbeke"
Jaren <- c(2002:2014)
Gemeenten <- expand.grid(NAMFGM_k, Jaren)
names(Gemeenten)[1] <- paste("NAMFGM_k")
names(Gemeenten)[2] <- paste("Jaar")
AantalPerGemeenten <- merge(Gemeenten, temp2, by=c("NAMFGM_k","Jaar"), all.x=TRUE)
AantalPerGemeenten$Aantal[is.na(AantalPerGemeenten$Aantal)] <- 0
AantalPerGemeenten$Aantalklasse <- ">30"
AantalPerGemeenten$Aantalklasse[AantalPerGemeenten$Aantal<31] <- "21-30"
AantalPerGemeenten$Aantalklasse[AantalPerGemeenten$Aantal<21] <- "11-20"
AantalPerGemeenten$Aantalklasse[AantalPerGemeenten$Aantal<11] <- "6-10"
AantalPerGemeenten$Aantalklasse[AantalPerGemeenten$Aantal<6] <- "1-5"
AantalPerGemeenten$Aantalklasse[AantalPerGemeenten$Aantal==0] <- "0"
AantalPerGemeenten$Aantalklasse<- factor(AantalPerGemeenten$Aantalklasse, levels=c("0","1-5","6-10","11-20","21-30",">30"))
cols <- c("white","#FFFF73","#F7BA3E","#D68522","#9E4410","#6B0601")

ggplot(AantalPerGemeenten, aes(x=Aantalklasse, fill=Aantalklasse)) +
  geom_bar(color="black") +
  scale_fill_manual(values=cols) +
  theme_INBO() +
  facet_wrap(~Jaar, ncol=3) +
  xlab("Aantal geschoten ree?n per gemeente") +
  ylab("Aantal gemeenten")

tabelGemeenten <- ddply(AantalPerGemeenten, c("Aantalklasse","Jaar"),
                        summarise,
                        Aantal = length(Aantal))

ggsave(file = "VerdelingGemeenten.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)
ggsave(file = "VerdelingGemeenten.eps", width = 22.4 /2.54, height = 15 / 2.54)

AantalPerGemeenten$Jaar <- factor(AantalPerGemeenten$Jaar)
AantalPerGemeenten$Totaal<- factor(AantalPerGemeenten$Aantalklasse)
ggplot(AantalPerGemeenten, aes(x=Aantalklasse, order=Jaar, fill=Totaal)) +
  geom_bar(color="grey70", position="dodge") +
  scale_fill_manual(values=cols) +
  theme_INBO() +
  theme(legend.position="bottom") +
  labs(fill="Aantal ree?n") +
  xlab("Aantal geschoten ree?n per gemeente voor de periode 2002-2014") +
  ylab("Aantal gemeenten")

ggsave(file = "VerdelingGemeenten2.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)
ggsave(file = "VerdelingGemeenten2.eps", width = 22.4 /2.54, height = 15 / 2.54)


##Manier van Thomas
##Gebruikt color palette RdYlBu maar heeft maar 11 kleuren,
##Ik neem manueel 6 kleuren en gebruik die dubbel en wit extra
cols <- c("#a50026","#a50026","#f46d43","#f46d43","#fee090","#fee090","white","white",
          "#abd9e9","#abd9e9","#74add1","#74add1","#4575b4")

ggplot(AantalPerGemeenten, aes(x=Aantalklasse, fill=Jaar)) +
  geom_bar(color="grey70", position="dodge") +
  scale_fill_manual(values=cols) +
  theme_INBO() +
  xlab("Aantal ree?n") +
  ylab("Aantal gemeenten")

ggsave(file = "VerdelingGemeenten3.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)
ggsave(file = "VerdelingGemeenten3.eps", width = 22.4 /2.54, height = 15 / 2.54)



ggplot(AantalPerGemeenten, aes(x=Jaar, fill=Totaal)) +
  geom_bar(color="grey90", position="stack", width=0.7) +
  scale_fill_manual(values=cols) +
  theme_INBO() +
  labs(fill="Aantal ree?n")+
  ylab("Aantal gemeenten")

ggsave(file = "VerdelingGemeenten3.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)
ggsave(file = "VerdelingGemeenten3.eps", width = 22.4 /2.54, height = 15 / 2.54)

##Aantal Veranderingen
Verandering <- subset(AantalPerGemeenten, Jaar=="2014" | Jaar=="2013")
Verandering <- cast(Verandering, NAMFGM_k~Jaar)
table(Verandering$`2013`,Verandering$`2014`)


####Figuur gerealiseerd afschot
Aanvulling_afzonderlijk <- subset(MeldingenAll, Jaar=="2014")
Aanvulling_afzonderlijk <- ddply(Aanvulling_afzonderlijk,
                                 c("Registernr","Provincie","Jaar","Type"), 
                                 summarise, value=length(Labelnummer))
Aanvulling_afzonderlijk2 <- cast(Aanvulling_afzonderlijk,Registernr+Jaar+Provincie~Type, na.rm=TRUE)

Afschot <- subset(MeldingenAll, !is.na(Datum))
Afschot <- subset(Afschot, Labeltype!="")

ggplot(Afschot,aes(x=Jaar, fill=Labeltype)) +
  geom_bar() + 
  ylab("Totaal aantal ree?n aangevraagd")+
  theme_INBO() +
  facet_grid(.~Provincie) +
  scale_x_discrete(labels=c(2002,"",2004,"",2006,"",2008,"",2010,"",2012,"",2014)) + 
  theme(text = element_text(size=12), axis.text.x = element_text(angle=90, vjust=0.5)) +
  guides(fill= guide_legend(reverse=TRUE)) +
  labs(fill="Labeltype")



########################
##########EINDE##########
remove(Meldingen)
remove(AANVRAAG)

###Meldingen2014 per Faunabeheerzone
GISUnion <- read.csv("Q:/OG_Faunabeheer/Projecten/Lopende projecten/INBOPRJ-10218 - Analyse en rapportage van jachtwilddata in Vlaanderen/Grofwild/Ree/Data/Gem_WBE_FZ.csv",sep=";", dec=",")
GISUnion <- as.data.frame(sapply(GISUnion,toupper))
GISUnion <- subset(GISUnion, Faunabeheerzone>0)
Afschotplan <- read.csv("Q:/OG_Faunabeheer/Projecten/Lopende projecten/INBOPRJ-10218 - Analyse en rapportage van jachtwilddata in Vlaanderen/Grofwild/Ree/Data/Afschotplannen2014.csv",sep=";", dec=",")
AfschotFB <- merge(temp1, Afschotplan, by="NummerAfschotplan", all.x=TRUE)
AfschotFB <- as.data.frame(sapply(AfschotFB,toupper))

GISUnion$Locatie <- interaction(GISUnion$Gemeente,GISUnion$WBE)
AfschotFB$Locatie <- interaction(AfschotFB$NAMFGM_k,AfschotFB$WBE)

AfschotFB2 <- unique(AfschotFB$Locatie)
write.csv2(GISUnion, file = "GISUnion.csv", row.names = FALSE)
write.csv2(AfschotFB2, file = "AfschotFB2.csv", row.names = FALSE)

ReeLocatie <- merge(AfschotFB, GISUnion, by="Locatie", all.x=TRUE)
ReeLocatie2 <- ddply(ReeLocatie, c("LabelNummer"), summarise,
                     Aantal = length(Faunabeheerzone),
                     FB = max(Faunabeheerzone))
ReeLocatieDubbel <- subset(ReeLocatie2, Aantal>1)
ReeLocatie2 <- subset(ReeLocatie2, Aantal==1)
ReeLocatie2 <- subset(ReeLocatie2, !is.na(FB))


###Controle labelnummer en afschotplan
Nummers <- read.csv("LabelNummerAfschotplan.csv",sep=";", dec=",")
Nummers$ID1 <- substr(Nummers$Afschotplan,1,2)
Nummers$ID2 <- as.numeric(substr(Nummers$Afschotplan,12,14))
Nummers$ID <- paste(Nummers$ID1,Nummers$ID2)
Nummers <- Nummers[-c(8,9)]
Meldingen2 <- Meldingen[c(8,15,32)]
Meldingen2$Bron <- substr(Meldingen2$LatestOperationUser,1,1)
Meldingen2 <-  subset(Meldingen2, Bron=="G")
Meldingen2 <- Meldingen2[c(1,2)]
Meldingen2$Jaar <- substr(Meldingen2$NummerAfschotplan,7,10)
Meldingen2 <- subset(Meldingen2, Jaar=="2014")
Meldingen2$Nummer <- substr(Meldingen2$LabelNummer,17,23)
Meldingen2$Nummer <- as.numeric(Meldingen2$Nummer)
Meldingen2$Type <- substr(Meldingen2$LabelNummer,13,13)
Meldingen2$ID1 <- substr(Meldingen2$NummerAfschotplan,1,2)
Meldingen2$ID2 <- as.numeric(substr(Meldingen2$NummerAfschotplan,12,15))
Meldingen2$ID <- paste(Meldingen2$ID1,Meldingen2$ID2)
Meldingen2 <- Meldingen2[-c(3,6,7)]
Meldingen2 <- merge(Meldingen2, Nummers, by="ID", all.x=TRUE)
Meldingen2$Controle <- "FOUT"
Meldingen2$ControleGeit[Meldingen2$Nummer>=Meldingen2$Begin.Geit & 
                      Meldingen2$Nummer <= Meldingen2$Eind.Geit] <- "OK"
Meldingen2$ControleBok[Meldingen2$Nummer>=Meldingen2$Begin.Bok & 
                      Meldingen2$Nummer <= Meldingen2$Eind.Bok] <- "OK"
Meldingen2$ControleKits[Meldingen2$Nummer>=Meldingen2$Begin.Kits & 
                      Meldingen2$Nummer <= Meldingen2$Eind.Kits] <- "OK"
Meldingen2$Controle[Meldingen2$ControleGeit=="OK"|
                      Meldingen2$ControleBok=="OK"|
                      Meldingen2$ControleKits=="OK"] <- "OK"
table(Meldingen2$Controle)
fouten <- subset(Meldingen2, Controle=="FOUT")
write.csv(fouten, file="FoutenLabelsAfschotplan2014.csv")
