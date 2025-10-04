#
# Opgave 1 – Boliger og Danmarks Statistik
#
#
# Opgave 1.1 – Det første skridt
#
# Skriv en kode, der viser hvordan du finder en tabel som kan give en liste over byer med
# indbyggertal vha DST-pakken dkstat.

# Trin 1 - Hent pakken dkstat samt dplyr og ggplot:
library(dkstat)
library(dplyr)
library(ggplot2)

# Trin 2 - Hent alle mulige tabeller ned fra Danmarks Statistik:
all_tabs <- dst_get_tables()

# Trin 3 - Søg igennem alle tabellerne for kun at finde dem der indeholde relevante variabler (her er det indbyggertal):
dst_search("befolkning")

# Ud fra ovenstående search ses det i konsollen, at "BY3" er det datasæt der er mest relevant for os.
# Det er bladnt andet fordi, den giver et samlet folketal for hvert byområde og landdistrikt.
# De andre tabeller (f.eks. BY1) kræver, at man nedhenter data fordelt på alder og køn,
# hvilket både giver for mange observationer til at hente i én forespørgsel
# og samtidig gør det mere uoverskueligt at få et samlet folketal pr. by.
# "BY3" summerer selv alder og køn, og giver derfor direkte det samlede indbyggertal pr. byområde,
# hvilket gør den mere anvendelig til opgaven.

# Trin 4 - Indhent metadataen for "BY3":

by3_meta <- dst_meta(table="BY3", lang = "da")

# Trin 5 - Filtrer på Folketal og år:

# Her laves der en filter-liste, hvor vi skriver hvilke værdier vi vil have med.
# I dette tilfælde tager vi alle byer med, men vi er kun interesseret i variablen "Folketal" og årstallet 2025.
by3_meta$variables
by3_meta$values
# variable og values bruges til at se hvad de hedder, så vi kan skrive det rigtigt ind i filter-funktionen.

by3_filters <- list(
  BYER = "*",
  FOLKARTAET = "Folketal",
  Tid = "2025")

# Trin 6 - Hent data fra DST med kriterierne fra ovenstående filter-liste:
by3_data <- dst_get_data(table = "BY3", query = by3_filters)

# Trin 7 - Rens datasættet for irrelevante rækker og kolonner
# (fjern landdistrikter, uden fast bopæl, hovedstadsområdet og hele kolonnen tid og Folketal):
  
# Vi laver et dataframe der indeholder kun de værdier der er relevante.
# Her fjernes alle de værdier, der hedder "Landdistrikter" og "Uden fast bopæl",
# da de ikke giver os noget relevant i forhold til antal indbyggere pr. by.
# Derudover fjerner vi også den første række, der hedder "Hovedstadsområdet",
# da den står som 0, og kolonnen "Tid" og "FOLKARTAET", da de ikke er relevante i opgaven senere.

by3_data1 <- by3_data %>% filter(!grepl("Landdistrikter", BYER)) %>%
  filter(!grepl("Uden fast bopæl", BYER)) %>%
  filter(!grepl("00001100 000-01100 Hovedstadsområdet", BYER)) %>%
  select(-TID, -FOLKARTAET)

# Vi er opmærksomme på, at en del af byerne står til at have 0 indbyggere.
# Ud fra en Google-søgning, kan vi konkludere at det er mange af dem der har knap 200 indbyggere.

# Derudover er der nogle af byerne som f.eks. Birkerød, der står der flere gange,
# og som står til at have hhv. 4, 31 osv. indbyggere. Derfor vil Birkerød opstå flere gange,
# og vil senere blive kategoriseret som en Landsby, i stedet for Større by.
# Denne fejl rettes op ved at slå alle de bynavne der hedder det samme, sammen til én, så der kun er én værdi, af hver bynavn.
# Men først skal alle koderne og ID’erne foran bynavnene fjernes, for at de kan slåes sammen.

# Trin 7.1 - Rens bynavnene (fjerner koder foran og parenteser bagved):
  
  by3_data1$BYER <- by3_data1$BYER %>%
  substring(20) %>%
  sub("\\s*\\(.*\\)$", "", .)

# Trin 7.2 - Slå ens bynavne sammen og summer indbyggertallet:
  
  by3_data1 <- by3_data1 %>%
  group_by(BYER) %>%
  summarise(value = sum(value, na.rm = TRUE))

#
# Opgave 1.2 – Kategori-variabel
#
# Lav en kategorivariabel i R ud fra følgende kriterier:
# bycat = (c("landsby"=250,"lille by"=1000, "almindelig by"=2500, "større by"=10000, "storby"=50000))
# og anvend den på listen over byer.

# Trin 1 - Lav en kategorivariabel og indsæt den i datasættet "by1_data1":
by3_data1$Bytype <- cut(                                                   # Opretter en ny kolonne "Størrelse" i datasættet
  by3_data1$value,                                                            # Bruger antallet af indbyggere (value) til at lave kategorier
  breaks = c(-Inf, 1000, 2500, 10000, 50000, Inf),                            # Definerer grænserne for de forskellige kategorier
  labels = c("Landsby", "Lille by", "Almindelig by", "Større by", "Storby"))  # Sætter navne på de kategorier der dannes

# Vi skriver ikke konkret 250 ind som betingelse, da vi  vurderer at det er intervallet fra alt under 250 op til 1000 indbyggere,
# der svarer til en landsby. Det er først når byen kommer over 1000 indbyggere at man går videre op i kategorierne.

#
# Opgave 1.3 – Merge de to dataframes
#
# Indlæs filen med boliger og tilpas de to dataframes så du kan merge de to sammen via variablen ”by”
# således at du får kategorien bycat med i dit bolig-datasæt.

# Trin 1 - Indlæs filen med boliger:
newhomes <- read.csv2("newhomes.csv")

# Trin 2 - Gør 'by3_data1'-data.framen klar.

# Trin 2.1 - Her fjerner vi indbyggertallet, da det ikke er relevant mere,
# og så navngiver vi by-kolonnen med et relevant navn:
by3_data1 <- by3_data1 %>%
  select(-value) %>%
  rename(                             # Giv kolonnerne nye navne
    By = BYER)                        # Giver BYER kolonnen navnet "By"

# Trin 3 - Gør 'newhomes'-datasættet klar.

# Trin 3.1 - Først så subsetter vi kun kolonnnen 'zip' der indeholder bynavnene og kolonnen 'pris' som indeholder huspriserne,
# samt navgiver vi kolonnerne noget relevant:
newhomes1 <- data.frame(
  By = newhomes$zip,
  Pris = newhomes$pris)

# Trin 3.2 - Nu fjernes alle tal og tegn i kolonnen 'By' så det kun er bynavnet der står under kolonnen:
newhomes1$By <- sub("^[0-9]{4}\\s*|,.*$", "", newhomes1$By)
# '^[0-9]{4}\\s*' → matcher et postnummer i starten (4 tal + evt. mellemrum)
# '|' → betyder “eller”
# ',.*$' → matcher et komma og alt efter det

# Trin 3.3 - For at vi kan lave en kvmpris, så renser vi først kolonnerne pris og kvm i det omrindelige datasæt,
# så de kun indeholder numeriske tal:
newhomes$pris <-as.numeric(gsub(",", ".", gsub("[^0-9,]", "", newhomes$pris)))
newhomes$kvm <- as.numeric(gsub(",", ".", gsub("[^0-9,]", "", newhomes$kvm)))

# Trin 3.4 - Så laver vi en ny kolonne til datasættet 'newhomes1' der indeholder kvmpriserne,
# da vi skal bruge det i det endelig datasæt:
newhomes1$kvmpris <- newhomes$pris / newhomes$kvm

# Trin 3.5 - Nu laver vi tallene lidt pænere at se på, ved at tilføje en tusindtals-(.) og decimaltalsseperator(,)
# i kvmpris-kolonnen:

newhomes1$kvmpris <- format(
  round(newhomes1$kvmpris, 0),   # afrund til hele kroner
  big.mark = ".",                # tusindtalsseparator
  decimal.mark = ",")            # decimaltegn

# Trin 4 - Merge de to data.frames ('by3_data1' og 'newhomes1') så de matcher på 'By'-kolonne,
# og der derved både er by, pris, indbyggertal og størrelse på byen.
by_pris <- merge(newhomes1, by3_data1, by = "By")

# Opgave 1.4 – Lav et plot

# Trin 1 - For at kunne bruge kvmpris-kolonnen til beregninger i en graf, skal den laves numerisk.
# Derfor laves et nyt datasæt, hvor de to pris-kolonner er numeriske.
# Først laves der et nyt merge af newhomes1 og by3_data1:
by_pris_num <- merge(newhomes1, by3_data1, by = "By")

# Trin 1.1 - Så laves kvmpris-kolonnen om til numerisk:
by_pris_num$kvmpris <- as.numeric(gsub("\\.", "", by_pris_num$kvmpris))

# Trin 1.2 - Lav et søjlediagram der viser gennemsnitprisen for hver bystørrelse.
# Hver søjle og farve viser en bytype, og y-aksen viser kvmprisen.
# Derudover laves der en vandret linje der viser landets gennemsnit af kvmpriserne.
avg_kvmpris <- mean(by_pris_num$kvmpris, na.rm = TRUE)   # Gennemsnit kvm for DK

ggplot(by_pris_num, aes(x = Bytype, y = kvmpris, fill = Bytype)) +
  stat_summary(fun = mean, geom = "col", width = 0.8) +
  geom_hline(yintercept = avg_kvmpris, linetype = "dashed", color = "red") +
  annotate("text", 
           x = 3,                                      # Ved 3. søjle / midten af plottet
           y = avg_kvmpris,                           # samme højde som linjen
           label = paste0("Landsgns: ", round(avg_kvmpris, 0)), 
           vjust = -1, color = "black") +
labs(
  title = "Gennemsnitlig kvadratmeterpris fordelt på bystørrelse",
  subtitle = "Sammenligning med landsgennemsnittet",
  caption = "Kilde: Danmarks Statistik og boligsiden",
  x = "Bytype",
  y = "kvmpris (kr.)"
) +
  theme(
    plot.caption     = element_text(hjust = 0)
  )
