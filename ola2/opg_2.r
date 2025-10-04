#
# Opgave 2 – Forbrugertillidsindikatorer og fremtidig vækst i husholdningernes forbrugsudgift
#

#
# Opgave 2.1 – Opdatering af DI’s forbrugertillidsindikator
#

# Der nærlæses i artiklen for at finde starttidspunktet for estimationen og spørgsmålene, der bruges til at sammenligner
# FTI og DI-FTI:

# Starttidspunktet:
# Det ses i tabellen i DI's artiklen at perioden er 2000-2016, og i vores opgave kører vi helt op til år 2025.

# Spørgsmålene:
# I DI' artikel, for at beregne deres forbrugertillidsindikator, er der taget gennemsnittet af følgende 4 spørgsmål:
# 1. Forbrugernes syn på deres egen økonomiske situation i dag
# 2. Forbrugernes syn på Danmarks økonomiske situation i dag
# 3. Forrbugernes købelyst nu
# 4. Forbrugernes kæbelyst fremadrette

## PUNKT 1 - Lav korrelation, forklaringsgrad og visuel graf der viser sammenhængen i perioden 2000-2016 ##

# Vi skal først lave forklaringsgrad og korrelation med den tidsperidode som i DI-artiklen,
# og replikere de tabeller og resultater fra artiklen, så man kan se om man rammer de samme tal som underviser.
# Herefter opdaterer man til de rigtige data/tidsperiode.

#
# Trin 1 - Hent filen om forbrugerforventninger (2000K1 - 2016K2) og navngiv kolonner noget andet.
#
# Datatabellen hedder FORV1 og er hentet fra Danmarks Statistik.
forvent <- read.csv2("forventninger2000_2016K2.csv")

colnames(forvent) <- c("Måned", "Forbrugertillidsindikator",
                       "Fam_oekonomi_ift_1aar_siden",
                       "Fam_oekonomi_om_1aar_ift_nu",
                       "DK_oekonomi_ift_1aar_siden",
                       "DK_oekonomi_om_1aar_ift_nu",
                       "Ansk_stoerre_forbrugsgoder_nu",
                       "Priser_idag_ift_1aar_siden",
                       "Priser_om_1aar_ift_nu",
                       "Arbejsloeshed_om_1aar_ift_nu",
                       "Ansk_stoerre_forbrugsgoder_inden_for_12maaneder",
                       "Opsparingsbehov_nuvaerende_oeko",
                       "Mulighed_for_opsparing_kommende_12maaneder",
                       "Fam_opsparing_koeber_mere_end_tjener")

# Trin 1.1 - Hent filen om privatforbrug og navngiver kolonnerne:

# Datatabellen om privatforbruget hedder NKH1 og er hentet fra Danmarks Statistik.
# Der er valgt ‘P.31 Husholdningernes forbrugsudgifter’, ‘2020-priser, kædede værdier’ og 'Sæsonkorrigeret':
pforbrug <- read.csv2("privatforbrug1999_2016K2.csv")

colnames(pforbrug) <- c("Kvartaler", "Husholdningernes_forbrugsudgifter")

# Trine 1.2 - Laver en vektor med navnene på kvartalerne fra privat forbrug, så de lettere kan bruges senere:
kvartal_navne <- pforbrug$Kvartaler[-c(1:4)]

#
# Trin 2 - Lav en ny data.frame hvor kun de 4 relevante spørgsmål indgår:
#
forventDI <- data.frame(forvent[, c(1, 3, 5, 7, 11)])

#
# Trin 3 - Lav et månedligt gennemsnit af de 4 spørgsmål, som svarer til DI's forbrugertillidsindikator:
#
forventDI$DIFTI <- rowMeans(forventDI[ , c(2:5)])

#
# Trin 4 - Lav månederne om til kvartaler i forventningerDI, hvor det er gennemsnittet af de 3 måneder i hvert kvartal.
#
# Trine 4.1 - Først laves der en funktion der laver det til kvartaler:
til_kvartaler <- function(x) {
  ts_m <- ts(as.numeric(x), start = c(2000, 1), frequency = 12)
  ts_k <- aggregate(ts_m, nfrequency = 4, FUN = mean)
}

# Trin 4.2 - Så bruges funktionen på alle spørgsmålene:
forvent_DI_kvartaler <- as.data.frame(lapply(forventDI[ , -1], til_kvartaler))

# Trin 4.3 - Tilføj nu kolonnenavne til kvartalerne:
forvent_DI_kvartaler <- cbind(kvartal = kvartal_navne, forvent_DI_kvartaler)

#
# Trin 5 - Lav privatforbrugs-datasættet om til årlig procentvækst og indsæt i en ny kolonne i eksisterende datasæt:
#
pforbrug$aarlig_pct_vækst <- c(rep(NA, 4), (exp(diff(log(as.numeric(pforbrug$Husholdningernes_forbrugsudgifter)), lag = 4)) - 1) * 100)

#
# Trin 6 - Lav et data.frame som indeholder det gennemsnitlige nettotal for alle 4 spørgsmål i forbrugerforventningsspørgsmålene
# og den årlige realvækst i privatforbruget
#

DI_sammenligning <- merge(
  forvent_DI_kvartaler[ , c(1, 6)],
  pforbrug[ , c(1, 3)],
  by.x = 1, by.y = 1,)

#
# Trin 7 - Lav en korrelation og forklaringsgrad på de to variabler.
#

# Trin 7.1 - Lav korrelationen:
cor_sammenligning <- cor(DI_sammenligning$DIFTI, DI_sammenligning$aarlig_pct_vækst)

# Trin 7.2 - Lav en summary for at se korrelation:
summary(cor_sammenligning)

# Trin 7.3 - Lav en lineær model for at se forklaringsgraden af realvæksten og forventingerne:
lm_DI_2016 <- lm(aarlig_pct_vækst ~ DIFTI, data = DI_sammenligning)

# Trin 7.4 - Lav en summary for at se forklaringsgraden:
summary(lm_DI_2016)
# Eller bare skriv det uden noget ("cor_sammenligning2025" for at få et rent tal)

# RESULTAT:
#Forklaringsgrad (R^2) = 0.4408
 
# Korrelation = 0.6703994

# Trin 8 - Lav en graf som i DI-artiklen (for år 2000-2016), for at se om linjen og søjlerne bevæger sig i samme mønster.

# A) Først laver jeg marginerne bredere, så der er plads til en beskrivende tekst, ved både højre og venstre søjle:
  par(mar = c(6, 5, 4, 5))
  
  # B) Så laver vi et søjlediagram (barplot) af den årlige vækst i privatforbruget:
  barplot(DI_sammenligning$aarlig_pct_vækst,          # Data vi vil vise som søjler
          col = "skyblue",                            # Farve på søjler
          border = NA,                                # Ingen kant rundt om søjler
          ylim = c(-8,8),                             # Gør plads til en y-akse med tal fra -8 til 8 (plotområde)
          axes = FALSE,                               # Ingen akser, vi laver selv akser senere
          main = "DI's forbrugertillidsindikator følger privatforbruget")  # Titel på grafen
  
  # C) Lav højre y-akse til søjlerne
  axis(4, at = seq(-8,8,2))                           # Tegner akse i højre (4) side fra -8 til 8 med interval på 2
  mtext("Årlig realvækst i privatforbrug (pct.)",
        side=4, line=3)                               # 'side=4' betyder højre side, 'line=3' er hvor langt den står fra aksen
  
  # D) Tegn en linje for forbrugertillidsindikatoren ovenpå eksisterende plot:
  par(new=TRUE)                                              # fortæller R, at vi vil tegne ovenpå det eksisterende plot
  plot(DI_sammenligning$DIFTI, type="l", col="black", lwd=2, # 'lwd=2' er linje tykkelsen
       axes=FALSE, xlab="", ylab="", ylim=c(-25,25))         # 'ylim=c(-25,25)' sikrer, at venstre akse går fra -25 til 25
  
  # E) Tilføj venstre y-akse (for DIFTI)
  axis(2, at=seq(-25,25,5))                                      # 'axis(2, ...)' betyder venstre side
  mtext("Forbrugertillidsindikator (nettotal)", side=2, line=3)  # 'side=2' betyder højre side, 'line=3' er hvor langt den står fra aksen
  
  
  # F) Tilføj x-aksen med kvartaler
  axis(1, at=seq(1, nrow(DI_sammenligning), 4), 
       labels = substr(DI_sammenligning$kvartal[seq(1, nrow(DI_sammenligning), 4)], 1, 4), las=2)
  # 'axis(1, ...)' = nederste akse (x-akse)
  # 'at=seq' siger hvor tallene skal stå (her hvert 4. kvartal/én gang om året).
  # 'labels=' viser hvilke tekster der skal stå.
  # 'substr (..., 1, 4)' fortæller den kun skal bruge tegn 1 til 4 i kvartalnavnene (f.eks. 2000, 2001 osv.)
  # 'las=2' gør teksten lodret, så den er lettere at læse
  
  # G) Her indsættes en linje hen over nul-skæringspunktet:
  abline(h = 0, col = "black", lwd = 1)               # h = 0 → tegner en vandret linje ved y = 0. lwd = 1 → tykkelse på linjen.
  
  # H) Så tilføjes en tekst nederst i plotområdet indeholdende kilderne:
  mtext("Kilde: Danmarks Statistik og DI", side = 1, line = 4, adj = 0) # Indsætter tekst nederst til venstre.
  # 'side=1' betyder nederst, 'line=4' betyder hvor langt væk fra aksen, 'adj=0' betyder det skal stå til venstre.
  
  # G) Så tilføjer vi en legend
  legend("topright",                                   # placeringen ("topright", "bottomleft" osv.)
         legend = c("Årlig realvækst i privatforbrug",
                    "DI's forbrugertillidsindikator"),
         pch    = c(15, NA),                          # 15 = udfyldt firkant til søjlerne, NA = inden firkant til linjen
         pt.cex = 1.5,                                # Gør firkanten større
         border = c(NA, NA),                          # Der skal ikke være nogen kant rundt om firkanterne
         lty    = c(NA, 1),                           # Linjetype (NA = ingen for søjler, 1 = fuldt optrukket for linjen)
         lwd    = c(NA, 2),                           # Linjetykkelse (NA = ingen for søjlerne, 2 = til linjen)
         col    = c("skyblue", "black"),              # Farver
         bty    = "n")                                # Ingen ramme rundt om legend

#
# Trin 8.2 - Her laves en graf hvor forbrugertillidsindikatoren er løftet med 2,39, så den matcher den der er i DI-artiklen
# (gennemsnittet af forbrugertillidsindiaktoren, for alle kvartaler)
#
# For at grafen ser bedre ud og ikke er så negativ i tallen, så rykker vi gennemsnittet af forbrugertillidsindikatoren fra -2,39 til 0.
# Dette påvirker ikke korrelationen osv, kun det visuelle i grafen, så tallene ikke er så negative.
#

# Først laves en et nyt datasæt med de nye netto-indekstal (løftet med 2,39) og årlige procentvækst i privatforbruget:
DI_sammenligning_loeftet <- data.frame(Kvartal = DI_sammenligning$kvartal,
                              DIFTI = DI_sammenligning$DIFTI + 2.39,
                              aarlig_pct_vækst = DI_sammenligning$aarlig_pct_vækst)

# Så laves en ny graf med de nye (løftet) tal, på præcis samme måde som før:
#
par(mar = c(6, 5, 4, 5))

#
barplot(DI_sammenligning_loeftet$aarlig_pct_vækst,
        col = "skyblue",
        border = NA,
        ylim = c(-8,8),
        axes = FALSE,
        main = "DI's forbrugertillidsindikator følger privatforbruget - Løftede tal")

#
axis(4, at = seq(-8,8,2))
mtext("Årlig realvækst i privatforbrug (pct.)", side=4, line=3)

#
par(new=TRUE)
plot(DI_sammenligning_loeftet$DIFTI, type="l", col="black", lwd=2,
     axes=FALSE, xlab="", ylab="", ylim=c(-25,25))

#
axis(2, at=seq(-25,25,5))
mtext("Forbrugertillidsindikator (nettotal)", side=2, line=3)

#
axis(1, at=seq(1, nrow(DI_sammenligning_loeftet), 4), 
     labels=substr(DI_sammenligning_loeftet$Kvartal[seq(1, nrow(DI_sammenligning_loeftet), 4)], 1, 4), las=2)

#
legend("topright",
       legend = c("Årlig realvækst i privatforbrug",
                  "DI's forbrugertillidsindikator"),
       pch    = c(15, NA),
       pt.cex = 1.5,
       border = c(NA, NA),
       lty    = c(NA, 1),
       lwd    = c(NA, 2),
       col    = c("skyblue", "black"),
       bty    = "n")

abline(h = 0, col = "black", lwd = 1)  # h = 0 → tegner en vandret linje ved y = 0. lwd = 1 → tykkelse på linjen.

mtext("Kilde: Danmarks Statistik og DI", side = 1, line = 4, adj = 0) # Indsætter tekst nederst til venstre.
# 'side=1' betyder nederst, 'line=4' betyder hvor langt væk fra aksen, 'adj=0' betyder det skal stå til venstre.

## PUNKT 2 - Lav korrelation, forklaringsgrad og visuel graf der viser sammenhængen i perioden 2000-2025 ##

#
# Nu skal det hele laves igen, bare for 2000K1 - 2025K2.
#

#
# Trin 1 - Hent filen om forbrugerforventninger (1998 - 2025) og navngiv kolonner noget andet.
#
forvent2025 <- read.csv2("forventninger1998_2025.csv")

colnames(forvent2025) <- c("Måned", "Forbrugertillidsindikator",
                       "Fam_oekonomi_ift_1aar_siden",
                       "Fam_oekonomi_om_1aar_ift_nu",
                       "DK_oekonomi_ift_1aar_siden",
                       "DK_oekonomi_om_1aar_ift_nu",
                       "Ansk_stoerre_forbrugsgoder_nu",
                       "Priser_idag_ift_1aar_siden",
                       "Priser_om_1aar_ift_nu",
                       "Arbejsloeshed_om_1aar_ift_nu",
                       "Ansk_stoerre_forbrugsgoder_inden_for_12maaneder",
                       "Opsparingsbehov_nuvaerende_oeko",
                       "Mulighed_for_opsparing_kommende_12maaneder",
                       "Fam_opsparing_koeber_mere_end_tjener")

# Trin 1.1 - Hent filen om privatforbrug og navngiver kolonnerne:
pforbrug2025 <- read.csv2("privatforbrug1997_2025.csv")

colnames(pforbrug2025) <- c("Kvartal", "Husholdningernes_forbrugsudgifter")

# Trine 1.2 - Laver en vektor med navnene på kvartalerne fra privat forbrug, så de lettere kan bruges senere:
kvartal_navne2025 <- pforbrug2025$Kvartal[-c(1:12)]

#
# Trin 2 - Lav en ny data.frame hvor kun de 4 relevante spørgsmål indgår,
# og fra januar 2000 og ned til og med juni 2025, så perioden matcher privatforbruget, som går til andet kvartal 2025:
#
forventDI2025 <- data.frame(forvent2025[25:330, c(1, 3, 5, 7, 11)])

#
# Trin 3 - Lav et månedligt gennemsnit af de 4 spørgsmål, som svarer til DI's forbrugertillidsindikator:
#
forventDI2025$DIFTI <- rowMeans(forventDI2025[ , c(2:5)])

#
# Trin 4 - Lav månederne om til kvartaler i forventningerDI, hvor det er gennemsnittet af de 3 måneder i hvert kvartal.
#
# Trine 4.1 - Først laves der en funktion der laver det til kvartaler:
til_kvartaler <- function(x) {
  ts_m <- ts(as.numeric(x), start = c(2000, 1), frequency = 12)
  ts_k <- aggregate(ts_m, nfrequency = 4, FUN = mean)
}

# Trin 4.2 - Så bruges funktionen på alle spørgsmålene:
forvent_DI_kvartaler2025 <- as.data.frame(lapply(forventDI2025[ , -1], til_kvartaler))

# Trin 4.3 - Tilføj nu kolonnenavne til kvartalerne:
forvent_DI_kvartaler2025 <- cbind(kvartal = kvartal_navne2025, forvent_DI_kvartaler2025)

#
# Trin 5 - Lav privatforbrugs-datasættet om til årlig procentvækst og indsæt i en ny kolonne i eksisterende datasæt:
#
pforbrug2025$aarlig_pct_vækst <- c(rep(NA, 4), (exp(diff(log(as.numeric(pforbrug2025$Husholdningernes_forbrugsudgifter)), lag = 4)) - 1) * 100)

#
# Trin 6 - Lav et data.frame som indeholder det gennemsnitlige nettotal for alle 4 spørgsmål i forbrugerforventningsspørgsmålene
# og den årlige realvækst i privatforbruget, startende fra K1 2000:
#

DI_sammenligning2025 <- merge(
  forvent_DI_kvartaler2025[ , c(1, 6)],
  pforbrug2025[-c(1:13), c(1, 3)],
  by.x = 1, by.y = 1,)

#
# Trin 7 - Lav en korrelation og forklaringsgrad på de to variabler.
#

# Trin 7.1 - Lav korrelationen:
cor_sammenligning2025 <- cor(DI_sammenligning2025$DIFTI, DI_sammenligning2025$aarlig_pct_vækst)

# Trin 7.2 - Lav en summary for at se korrelation:
summary(cor_sammenligning2025)
# Eller bare skriv det uden noget ("cor_sammenligning2025") for at få et rent tal

# Trin 7.3 - Lav en lineær model for at se forklaringsgraden af realvæksten og forventingerne:
lm_DI <- lm(aarlig_pct_vækst ~ DIFTI, data = DI_sammenligning2025)

# Trin 7.4 - Lav en summary for at se forklaringsgraden:
summary(lm_DI)

# RESULTAT:
# Forklaringsgrad (R^2) = 0.4422

# Korrelation = 0.6692

# Trin 8 - Lav en graf som i DI-artiklen, for visuelt at se hvordan det bevæger i forhold til hinanden:

# A) Først laver jeg marginerne bredere, så der er plads til besskrivende tekst, ved både højre og venstre søjle:
par(mar = c(6, 5, 4, 5))

# B) Så laver vi et søjlediagram (barplot) af årlig vækst
barplot(DI_sammenligning2025$aarlig_pct_vækst,          # Data vi vil vise som søjler
        col = "skyblue",                                # Farve på søjler
        border = NA,                                    # Ingen kant rundt om søjler
        ylim = c(-10,10),                               # Gør plads til en y-akse med tal fra -10 til 10 (plotområde)
        axes = FALSE,                                   # Ingen akser, vi laver selv akser senere
        main = "DI's forbrugertillidsindikator følger privatforbruget")      # Titel på grafen

# C) Lav højre y-akse til søjlerne
axis(4, at = seq(-10,10,2))                           # Tegner akse i højre (4) side fra -10 til 10 med interval på 2
mtext("Årlig realvækst i privatforbrug (pct.)",
      side=4, line=3)                                 # 'side=4' betyder højre side, 'line=3' er hvor langt den står fra aksen

# D) Tegn linje for forbrugertillid ovenpå 
par(new=TRUE)                                                  # fortæller R, at vi vil tegne ovenpå det eksisterende plot
plot(DI_sammenligning2025$DIFTI, type="l", col="black", lwd=2, # 'lwd=2' er linje tykkelsen
     axes=FALSE, xlab="", ylab="", ylim=c(-40,40))             # 'ylim=c(-25,25)' sikrer, at venstre akse går fra -25 til 25

# E) Tilføj venstre y-akse (for DIFTI)
axis(2, at=seq(-40,40,5))                                     # 'axis(2, ...)' betyder venstre side
mtext("Forbrugertillidsindikator (nettotal)", side=2, line=3) # 'side=2' betyder højre side, 'line=3' er hvor langt den står fra aksen

# F) Tilføj x-aksen med kvartaler
axis(1, 
     at = seq(1, nrow(DI_sammenligning2025), 4), 
     labels = substr(DI_sammenligning2025$kvartal[seq(1, nrow(DI_sammenligning2025), 4)], 1, 4),
     las = 2)
# 'axis(1, ...)' = nederste akse (x-akse)
# 'at=seq' siger hvor tallene skal stå (her hvert 4. kvartal/én gang om året).
# 'labels=' viser hvilke tekster der skal stå.
# 'substr (..., 1, 4)' fortæller den kun skal bruge tegn 1 til 4 i kvartalnavnene (f.eks. 2000, 2001 osv.)
# 'las=2' gør teksten lodret, så den er lettere at læse

# G) Så tilføjer vi en legend
legend("topleft",                                   # placeringen ("topright", "bottomleft" osv.)
       legend = c("Årlig realvækst i privatforbrug",
                  "DI's forbrugertillidsindikator"),
       pch    = c(15, NA),                          # 15 = udfyldt firkant til søjlerne, NA = inden firkant til linjen
       pt.cex = 1.5,                                # Gør firkanten større
       border = c(NA, NA),                          # Der skal ikke være nogen kant rundt om firkanterne
       lty    = c(NA, 1),                           # Linjetype (NA = ingen for søjler, 1 = fuldt optrukket for linjen)
       lwd    = c(NA, 2),                           # Linjetykkelse (NA = ingen for søjlerne, 2 = til linjen)
       col    = c("skyblue", "black"),              # Farver
       bty    = "n")                                # Ingen ramme rundt om legend

abline(h = 0, col = "black", lwd = 1)  # h = 0 → tegner en vandret linje ved y = 0. lwd = 1 → tykkelse på linjen.

mtext("Kilde: Danmarks Statistik og DI", side = 1, line = 4, adj = 0) # Indsætter tekst nederst til venstre.
# 'side=1' betyder nederst, 'line=4' betyder hvor langt væk fra aksen, 'adj=0' betyder det skal stå til venstre.

## PUNKT 3 - Lav en vurdering af om forbrugertillidsindikatoren fra DI fortsat er bedre end forbrugertillidsindikatoren fra DST ##

#
# Trin 1 - Først laves en ny data.frame hvor kun Danmarks Statistiks forbrugertillidsindikator indgår,
# og som kun er ned til og med juni 2025, så perioden matcher privatforbruget, som går til første kvartal 2025:
#
forbrugertillid_dst <- data.frame(forvent2025[25:330, 2])

# Trin 1.1 - Kolonnen omdøbes til det korrekte navn:
names(forbrugertillid_dst) <- "Forbrugertillidsindikator"

#
# Trin 2 - Så bruges funktionen 'til_kvartaler' til at lave det nye data.frame (med forbrugertilliden) om til kvartaler i steder for måneder:
#
forbrugertillid_dst <- as.data.frame(lapply(forbrugertillid_dst, til_kvartaler))

#
# Trin 3 - Tilføj nu kolonnenavne til kvartalerne i datasættet 'forbrugertillid_dst':
#
forbrugertillid_dst <- cbind(kvartal = kvartal_navne2025, forbrugertillid_dst)

#
# Trin 4 - Lav et data.frame som indeholder det gennemsnitlige nettotal for DKs Statistiks forbrugertillidsindikator
# og den årlige realvækst i privatforbruget:
#
tillid_forbrug <- merge(
  forbrugertillid_dst[ , ],
  pforbrug2025[ , c(1, 3)],
  by.x = 1, by.y = 1,)

#
# Trin 5 - Lav en korrelation og forklaringsgrad på de to variabler.
#
# Trin 5.1 - Lav korrelationen:
cor_tillid_forbrug <- cor(tillid_forbrug$Forbrugertillidsindikator, tillid_forbrug$aarlig_pct_vækst)

# Trin 5.2 - Lav en summary for at se korrelation:
summary(cor_tillid_forbrug)
# Eller bare skriv det uden noget ("cor_sammenligning2025") for at få et rent tal

# Trin 5.3 - Lav en lineær model for at se forklaringsgraden af realvæksten og forventingerne:
lm_DST <- lm(aarlig_pct_vækst ~ Forbrugertillidsindikator, data = tillid_forbrug)

# Trin 5.4 - Lav en summary for at se forklaringsgraden:
summary(lm_DST)

# RESULTAT:
# Forklaringsgrad (R^2) = 0.3545

# Korrelation = 0.6008

#
# Trin 6 - Der laves en graf som med de andre sammenligninger:
#

par(mar = c(6, 5, 4, 5))

barplot(tillid_forbrug$aarlig_pct_vækst,
        col = "skyblue",
        border = NA,
        ylim = c(-10,10),
        axes = FALSE,
        main = "DST forbrugertillidsindikator følger privatforbruget i mindre grad")

axis(4, at = seq(-10,10,2))
mtext("Årlig realvækst i privatforbrug (pct.)",
      side=4, line=3)

par(new=TRUE)
plot(tillid_forbrug$Forbrugertillidsindikator, type="l", col="black", lwd=2,
     axes=FALSE, xlab="", ylab="", ylim=c(-35,35))

axis(2, at=seq(-35,35,5))
mtext("Forbrugertillidsindikator (nettotal)", side=2, line=3)

axis(1, 
     at = seq(1, nrow(tillid_forbrug), 4), 
     labels = substr(tillid_forbrug$kvartal[seq(1, nrow(tillid_forbrug), 4)], 1, 4),
     las = 2)

legend("topleft",
       legend = c("Årlig realvækst i privatforbrug",
                  "DST's forbrugertillidsindikator"),
       pch    = c(15, NA),
       pt.cex = 1.5,
       border = c(NA, NA),
       lty    = c(NA, 1),
       lwd    = c(NA, 2),
       col    = c("skyblue", "black"),
       bty    = "n")

abline(h = 0, col = "black", lwd = 1)

mtext("Kilde: Danmarks Statistik og DI", side = 1, line = 4, adj = 0)

#
# Trin 7 - Lav en tabel/data.frame hvor DI's tal og DST's tal står side om side til sammenligning:
#
DI_DST_sammenlign <- data.frame(
  Resultat   = c("Forklaringsgrad", "Korrelation"),
  `DI-FTI` = c(0.44, 0.66),
  `DST-FTI`    = c(0.35, 0.60))

#
# Trin 8 - Lav en graf med privatforbrug som søjler og de to forskellige resultater af forbrugertillidsindikatoren (DI og DST),
# så de visuelt kan sammenlignes:
#

par(mar = c(6, 5, 4, 5))

barplot(tillid_forbrug$aarlig_pct_vækst,
        col = "lightblue",
        border = NA,
        ylim = c(-10,10),
        axes = FALSE,
        main = "DI's forbrugertillidsindikator følger i højere grad privatforbruget")

axis(4, at = seq(-10,10,2))
mtext("Årlig realvækst i privatforbrug (pct.)",
      side=4, line=3)

par(new=TRUE)
plot(tillid_forbrug$Forbrugertillidsindikator, type="l", col="red", lwd=2,
     axes=FALSE, xlab="", ylab="", ylim=c(-40,40))

axis(2, at=seq(-40,40,5))
mtext("Forbrugertillidsindikator (nettotal)", side=2, line=3)

par(new=TRUE)
plot(DI_sammenligning2025$DIFTI, type="l", col="darkgreen", lwd=2,
     axes=FALSE, xlab="", ylab="", ylim=c(-40,40))

axis(1, 
     at = seq(1, nrow(tillid_forbrug), 4), 
     labels = substr(tillid_forbrug$kvartal[seq(1, nrow(tillid_forbrug), 4)], 1, 4),
     las = 2)

legend("topleft",
       legend = c("Årlig realvækst i privatforbrug",
                  "DST's forbrugertillidsindikator",
                  "DI's forbrugertillidsindikator"),
       pch    = c(15, NA, NA),
       pt.cex = 1.5,
       border = c(NA, NA, NA),
       lty    = c(NA, 1, 1),
       lwd    = c(NA, 2, 2),
       col    = c("lightblue", "red", "darkgreen"),
       bty    = "n")

abline(h = 0, col = "black", lwd = 1)  # h = 0 → tegner en vandret linje ved y = 0. lwd = 1 → tykkelse på linjen.

mtext("Kilde: Danmarks Statistik og DI", side = 1, line = 4, adj = 0) # Indsætter tekst nederst til venstre.
# 'side=1' betyder nederst, 'line=4' betyder hvor langt væk fra aksen, 'adj=0' betyder det skal stå til venstre.

#
# Vi skal finde ud hvorfor der er forskel i hhv. DST's og DI's forbrugertillidsindikator.
#

# Trin 1 - Lav et data.frame som kun indeholder de 3 spørgsmål der er forskel i de to metoder (DI og DST):
unikke <- data.frame(forvent2025[25:330, c(4,6,11)])

# Trin 1.1 - Lav et data.frame som kun indeholder de 3 spørgsmål begge metoder har tilfælles (DI og DST):
faelles <- data.frame(forvent2025[25:330, c(3,5,7)])

# Trin 1.2 - Lav et data.frame med alle 6 spørgsmål, der indgår enten fælles eller unikt i begge metoder:
spm <- data.frame(forvent2025[25:330, c(4,6,11,3,5,7)])

# Trin 2 - Lav de tre data.frame til kvartaler:
unikke_kvartaler <- as.data.frame(lapply(unikke[ , ], til_kvartaler))
faelles_kvartaler <- as.data.frame(lapply(faelles[ , ], til_kvartaler))
spm_kvartaler <- as.data.frame(lapply(spm[ , ], til_kvartaler))

# Trin 3 - Tilføj nu kolonnenavne til kvartalerne:
unikke_kvartaler <- cbind(kvartal = kvartal_navne2025, unikke_kvartaler)
faelles_kvartaler <- cbind(kvartal = kvartal_navne2025, faelles_kvartaler)
spm_kvartaler <- cbind(kvartal = kvartal_navne2025, spm_kvartaler)

# Trin 4 - Lav en graf der viser bevægelserne i de tre unikke spørgsmål:
plot(x = seq_len(nrow(unikke_kvartaler)), # Vi bruger seq_len(nrow(...)) for at tvinge x-aksen til at være 1, 2, 3 … op til antal rækker.
     y = unikke_kvartaler$Fam_oekonomi_om_1aar_ift_nu,
     type="l", col="red", lwd=2,
     axes=FALSE, xlab="", ylab="", ylim=c(-65,65),
     main="Unikke spørgsmål for hhv. DI & DST")

lines(x = seq_len(nrow(unikke_kvartaler)),
      y = unikke_kvartaler$DK_oekonomi_om_1aar_ift_nu,
      col="green", lwd=2)

lines(x = seq_len(nrow(unikke_kvartaler)),
      y = unikke_kvartaler$Ansk_stoerre_forbrugsgoder_inden_for_12maaneder,
      col="blue", lwd=2)

axis(2, at=seq(-65,65,10))
mtext("Forbrugerforventninger indekstal", side=2, line=3)

axis(1, 
     at = seq(1, nrow(unikke_kvartaler), 4), 
     labels = substr(unikke_kvartaler$kvartal[seq(1, nrow(unikke_kvartaler), 4)], 1, 4),
     las = 2)

abline(h = 0, col = "black", lwd = 1)

mtext("Kilde: Danmarks Statistik", side = 1, line = 4, adj = 0)

legend("topleft", inset = c(0, -0.09), xpd=TRUE,   # 'inset =' vælger hvor den placeres. 'xpd=TRUE' tillader den står uden for det normale plotboks
       legend = c("Familiens økonomiske situation om et år ift. nu",
                  "Danmarks økonomiske situation om et år ift. nu",
                  "Anskaffe større forbrugsgode inden for 12 måneder"),
       lty = c(1, 1, 1), lwd = c(2, 2, 2),
       col = c("red", "green", "blue"),
       bty = "n")

# Trin 5 - Lav en graf der viser bevægelserne i de tre spørgsmål som DI og DST har til fælles:
plot(x = seq_len(nrow(faelles_kvartaler)),
     y = faelles_kvartaler$Fam_oekonomi_ift_1aar_siden,
     type="l", col="red", lwd=2,
     axes=FALSE, xlab="", ylab="", ylim=c(-65,65),
     main="Fælles spørgsmål for hhv. DI & DST")

lines(x = seq_len(nrow(unikke_kvartaler)),
      y = faelles_kvartaler$DK_oekonomi_ift_1aar_siden,
      col="green", lwd=2)

lines(x = seq_len(nrow(unikke_kvartaler)),
      y = faelles_kvartaler$Ansk_stoerre_forbrugsgoder_nu,
      col="blue", lwd=2)

axis(2, at=seq(-65,65,10))
mtext("Forbrugerforventninger indekstal", side=2, line=3)

axis(1, 
     at = seq(1, nrow(faelles_kvartaler), 4), 
     labels = substr(faelles_kvartaler$kvartal[seq(1, nrow(faelles_kvartaler), 4)], 1, 4),
     las = 2)

abline(h = 0, col = "black", lwd = 1)

mtext("Kilde: Danmarks Statistik", side = 1, line = 4, adj = 0)

legend("topleft", inset = c(0, -0.09), xpd=TRUE,
       legend = c("Familiens økonomiske situation nu ift. om et år",
                  "Danmarks økonomiske situation nu ift. om et år",
                  "Anskaffe større forbrugsgode nu"),
       lty = c(1, 1, 1), lwd = c(2, 2, 2),
       col = c("red", "green", "blue"),
       bty = "n")

# Trin 6 - Lav standardafvigelser for de 6 spørgsmål. De laves her som en vektor:
sd_unikke <- sapply(unikke_kvartaler[ , -1], sd)

sd_faelles <- sapply(faelles_kvartaler[ , -1], sd)

# Trin 6.1 - Saml standardafvigelserne for nemmere sammenligning:
sd_sammenlign <- data.frame(
  Standardafvigelse = c(sd_unikke, sd_faelles),
  Metode = c("DST", "DST", "DI",
             "DI/DST", "DI/DST", "DI/DST"))

# RESULTAT:
# Selvom de spørgsmål med højest standardafvigelser er de tre spørgsmål begge metoder bruger,
# så får DST’s indikator et ekstra boost af mere ustabile spørgsmål
# ("Familiens økonomiske situation om et år, sammenlignet med i dag" og "Danmarks økonomiske situation om et år, sammenlignet med i dag"),
# mens DI’s indikator baserer sig på mere stabile spørgsmål (Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.).
# Det kan forklare, hvorfor DI’s indikator er “glattere” og har højere forklaringsgrad med privatforbruget.

# Trin 7 - Lav en korrelation mellem alle spørgsmålene og privatforbrugets årlig realvækst,
# for at se hvilke spørgsmål der bedst forklarer udviklingen i forbruger

# Trin 7.1 - Først laves to forskellige data.frames med korrelationsresultaterne for unikke spørgsmål og forbruget
# og fælles spørgsmål og forbruget:
unikke_cor <- data.frame(
  Korrelation_med_forbruget = cor(unikke_kvartaler[ , 2:4],
                                  pforbrug2025$aarlig_pct_vækst[-c(1:12)]),
  Metode = c("DST", "DST", "DI"))

faelles_cor <- data.frame(
  Korrelation_med_forbruget = cor(faelles_kvartaler[ , 2:4],
                                  pforbrug2025$aarlig_pct_vækst[-c(1:12)]),
  Metode = c("DI/DST", "DI/DST", "DI/DST"))

# Trin 7.2 - Så samles begge korrelationsdatasæt i en
cor_samlet <- rbind(unikke_cor, faelles_cor)

# RESULTAT:
# Her ser vi i korrelationen, at de fire spørgsmål som DI bruger har størst korrelation,
# og de 2 spørgsmål, som KUN DST bruger, har lavest korrelation.
# F.eks. er det spørgsmål med stærkest korrelation, det om at købe større forbrugsgode inden for et år,
# og netop dét spørgsmål er det som DI unikt bruger i deres beregner.
# Det viser derfor, at DI’s unikke spørgsmål (forbrugsgoder inden for 12 mdr.)
# faktisk har en stærkere sammenhæng med privatforbruget end nogle af DST’s spørgsmål.

# DST’s indikator bygger bl.a. på spørgsmålet om DKs økonomi om 1 år ift. nu, som har lav korrelation med privatforbruget.
# Det kan være med til at trække sammenhængen ned.
# DI’s indikator bruger i stedet forbrugsgoder-spørgsmålet, som har høj korrelation
# – og det kan forklare, hvorfor DI’s samlede indikator har højere forklaringsgrad.

# DI’s metode ser mere robust ud, fordi deres unikke spørgsmål er bedre koblet til faktisk forbrug.
# DST’s metode er mere svingende og inkluderer spørgsmål med svagere relation til privatforbruget.
# Det giver et klart argument for, at DI’s indikator stadig er bedre til at afspejle privatforbrugstendenser.

# Trin 8 - Lav en korrelation med alle spørgsmålene, så vi kan se om nogle af spørgsmål overlapper hinanden resultatmæssigt:
spm_cor<- cor(spm_kvartaler[ , -1])

# RESULTAT:
# Korrelationen viser, at der ikke er tale om fuld dobbeltvægt mellem spørgsmålene (ingen korrelationer over 0,9), men der er et tydeligt overlap.
# Især "Familiens økonomi nu ift. et år siden" og "Familiens økonomi om et år ift. nu" har en høj korrelation på 0,88.
# Og eftersom DI kun har et af spørgsmålene, men DST har begge spørgsmål i deres beregnnig, betyder det,
# at DST’s metode i højere grad end DI’s bygger på to næsten ens spørgsmål, hvilket kan være med til at forklare forskellen mellem de to indikatorer.
# DI’s metode fremstår dermed mere uafhængig i sin sammensætning, mens DST’s kan være præget af større overlap.

#
# Opgave 2.2 – Forudsigelser af forbruget
#
# Beregn/forudsig den årlige realvækst i husholdningernes forbrugsudgift for 2. kvartal 2025 med
# henholdsvis DI’s forbrugertillidsindikator og forbrugertillidsindikatoren fra DST.

# Trin 1 - Lav to lineær regressions-modeller.
# Én hvor den afhængige variabel er den årlige realvækst i privatforbrug, og den uafhængige er DI’s indikator.
# Én hvor den uafhængige er DST’s indikator.
# Her bruger vi de lineære regressions-modeller vi lavede i opgave 2.1:

# Model med DI's indikator
lm_DI
# <- lm(aarlig_pct_vækst ~ DIFTI, data = DI_sammenligning2025)

# Model med DST's indikator
lm_DST
# <- lm(aarlig_pct_vækst ~ Forbrugertillidsindikator, data = tillid_forbrug)

# Trin 2 - Find værdien af indikatorerne i 3. kvartal 2024
# Vi slår op i datasættene, og finder hvilke værdier DI’s og DST’s tillidsindikator har i K3 2025.

# Trin 2.1 - Først laves et datasæt der indeholder forbrugertillidsindikatoren for hhv. DI og DST, der går til og med september 2025.
# Vi kan ikke genbruge de datasæt vi lavede i opgave 2.1 da det datasæt kun går til juni i 2025, da det skulle matche det data vi havde i privatoforbrugstabellen.
tillid_DI_K3 <- data.frame(forvent2025[, c(1, 3, 5, 7, 11)])
tillid_DST_K3 <- data.frame(forvent2025[, c(1, 2)])

# Trin 2.2 - Lav et månedligt gennemsnit af de 4 spørgsmål i forventningerne, som gvier os DI's forbrugertillidsindikator:

tillid_DI_K3$DIFTI <- rowMeans(tillid_DI_K3[ , c(2:5)])

# Trin 2.3 - Så tages gennemsnittet af månederne juli, august og september i både DI's og DST's datasæt:
DI_K3_2025 <- data.frame(DIFTI = mean(tail(tillid_DI_K3$DIFTI, 3)))
DST_K3_2025 <- data.frame(Forbrugertillidsindikator = mean(tail(tillid_DST_K3$Forbrugertillidsindikator, 3)))

# Trin 2.4 - Nu kan den årlige realvækst forudsiges, ud fra de to lineære modeller
# samt de to nye dataframes med tillidsindikatorernes gennemsnit for 3 kvartal 2025:
pred_DI <- predict(lm_DI, newdata = DI_K3_2025)
pred_DST <- predict(lm_DST, newdata = DST_K3_2025)

# Trin 2.5 - Print de to forudsigelser så vi kan se resultatet:
pred_DI
pred_DST

# RESULTATET af forudsigelserne af privatforbruget i 3. kvartal 2025:
# DI's forbrugertillidsindikator = -1.187645
# DST's forbrugertillidsindikator = -1.94548

# Resultaterne viser, at både DI’s og DST’s forbrugertillidsindikatorer forudsiger et fald i privatforbruget i 3. kvartal 2025.
# Prognosen med DI’s indikator peger på et fald på ca. -1,2 %,
# mens prognosen med DST’s indikator viser et lidt større fald på ca. -2,0 %.
# Selvom der er en forskel i styrken af nedgangen, indikerer begge modeller,
# at husholdningernes købekraft vil være lavere end samme kvartal året før,
# hvilket kan tolkes som tegn på svækket forbrugertillid og tilbageholdenhed blandt husholdningerne.