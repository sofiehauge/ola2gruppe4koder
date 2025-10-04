#
# Opgave 3 – Logistik regression, husholdningernes forbrugsudgift og forbrugertillid
#
#
# Opgave 3.1 – Feature Engineering, dummy variable
#
# Dan en dummy variable af den kvartalsvise årlige vækst i husholdningernes forbrugsudgift
# for perioden 1. kvartal 1998 til 2. kvartal 2025.
# Hvor ofte stiger den kvartalsvise årlige vækst i husholdningernes forbrugsudgift?
# Og hvor ofte falder den kvartalsvise årlige vækst i husholdningernes forbrugsudgift?

# Trin 1 - Først laver vi en dummy variabel der fortæller os om den årlige vækst er steget eller faldet.
# Her bruger vi funktionen 'ifelse', hvor at vi siger at hvis værdien i den årlige vækst kolonne
# er mere end 0, skal den have værdien 1 i en ny kolonne, og hvis værdien i væksten er mindre en 0,
# skal den have værdien 0 i den nye kolonne:

pforbrug2025$dummy_variabel <- ifelse(pforbrug2025$aarlig_pct_vækst > 0, 1, 0)

# Trin 2 - Vis hvor ofte den kvartalvise årlige vækst hhv. stiger og falder i perioden 1999 - 2025:
table(pforbrug2025$dummy_variabel)

# RESULTATET af antal stigninger og falder:
# Stigning = 82
# Fald = 28

# Trin 2.1 - For overblikket skyld viser vi også den procentvise andel af hhv. stigningen og faldet i væksten:
prop.table(table(pforbrug2025$dummy_variabel)) * 100

# RESULTATET af hhv. den procentvise stigning og fald:
# Stigning = 74,5%
# Fald = 25,5%

# Trin 3 - Vi vælger også her at visualisere resultaterne ved hjælp af et søjlediagramer, der viser både antal og procentandel

# Trin 3.1 - Lav en dataframe der indeholder antal stigninger og fald:
dummy_res <- data.frame(
  Udvikling = c("Fald", "Stigning"),
  Antal = c(28, 82)
)

# Trin 3.2 - Tilføj procentandel til ovenstående dataframe:
dummy_res$Procent <- round(dummy_res$Antal / sum(dummy_res$Antal) * 100, 1)

# Trin 3.3 - Lav søjlediagrammet:
ggplot(dummy_res, aes(x = Udvikling, y = Antal, fill = Udvikling)) +
  geom_col() +
  geom_text(aes(label = paste0(Procent, "%")), nudge_y = 3, size = 5) +
  labs(
    title = "Udvikling i husholdningernes årlige vækst",
    caption = "Kilde: Danmarks Statistik",
    x = "Udvikling",
    y = "Antal kvartaler"
  ) +
  theme_minimal() +
  theme(legend.position = "none")+
  theme(
    plot.caption     = element_text(hjust = 0)
  )


#
# Opgave 3.2 – Logistisk regression og forudsigelser
#

# Lav en logistisk regression med dummy variablen fra opgave 2.1 og de fire indikatorer i DI’s forbrugertillidsindikator.

# Trin 1 - Lav et dataframe der indeholder DI forbrugertillidsindikator for perioden 1998-2025:
# Vi har i opgave 2.1 lavet et dataframe med DI's forbrugerforventninger, dog lavede vi det kun fra år 2000.
# Nu skal vi bruge det ned fra år 1998, derfor laves et nyt:

# Trin 1.1 - Lav et datafram hvor kun de 4 relevante spørgsmål indgår,
# og fra januar 1998 ned til og med juni 2025, så perioden matcher privatforbruget, som går til andet kvartal 2025:
forventDI1998 <- data.frame(forvent2025[1:330, c(1, 3, 5, 7, 11)])

# Trin 1.2 - Lav et månedligt gennemsnit af de 4 spørgsmål, som er med i DI's beregninger for forbrugertillidsindikatoren:
forventDI1998$DIFTI <- rowMeans(forventDI1998[ , c(2:5)])

# Trin 1.3 - Lav datasættet i kvartaler i stedet for måneder. Her bruges kvartal-funktionen fra opg. 2.1:
forventDI1998 <- as.data.frame(lapply(forventDI1998[ , -1], til_kvartaler))

# Trin 1.4 - Tilføj nu kolonnenavne til kvartalerne:
# Udtrækker kvartalerne fra pforbrug:
kvartal_navne1998 <- pforbrug2025$Kvartal[-c(1:4)]
# Indsæt kolonne med kvartalnavne i "forventDI1998":
forventDI1998<- cbind(Kvartal = kvartal_navne1998, forventDI1998)

# Trin 2 - Merge de to datasæt (privatforbrug m/ dummyvariabel og forbrugertillid), så de kan bruges til logistisk regresssion:
dat_log <- merge(
  pforbrug2025[, c("Kvartal", "dummy_variabel")],
  forventDI1998 [, c("Kvartal", "DIFTI")],
  by = "Kvartal"
)

# Trin 3 - Opsæt og estimer en logistisk regression, hvor dummyvariablen forklares af DI’s forbrugertillid.
log_dummy <- glm(dummy_variabel ~ DIFTI,
                          data = dat_log,
                           family = binomial)

# Trin 4 - Print den logistiske regressioner
log_dummy

# Hvilken retning forudsiger jeres model, at den årlige vækst i husholdningernes forbrugsudgift, vil gå i 3. kvartal 2025?

# Trin 5 - Så bruges funktione predict() sammen med forbrugertillidsindikatoren for K3 2025, som vi lavede i opgave 2.2:
pred_log <- predict(log_dummy, newdata = DI_K3_2025, type = "response")

# Trin 5.1 - Print forudsigelsen:
pred_log

# RESULTAT af forudsigelse af dummy variabel for K3 2025 (stiger eller falder privatforrbruget):
# 1 = 0.27
# Dette betyder at der er 27% chance for at værdien er 1 (altså stigende) i K3 2025.
# Dvs. der er 73% chance for at værdien er 0, og derfor forudsiger vi at privatforbruget vil falde i K3 2025.

#
# Opgave 3.3 – Simpel validering af model
#
# Hvor ofte forudsiger jeres model i opgave 3.2, at den kvartalsvise årlige realvækst i husholdningernes forbrugsudgift stiger?
# Hvor ofte er det så reelt tilfældet, at den kvartalsvise årlige realvækst i husholdningernes forbrugsudgift stiger,
# set i forhold til, hvad jeres model forudsiger? 

# Trin 1 - Forudsig sandsynligheder for at værdien er 1, altså at forbruget stiger:
dat_log$pred_prob <- predict(log_dummy, type = "response")

# Trin 2 - Oversæt ovenstående til 0/1 forudsigelser. Hvis den er over 50% så er det 1:
dat_log$pred <- ifelse(dat_log$pred_prob > 0.5, 1, 0)

# Trin 3 - Lav en konfusionsmatrix der viser hvor ofte modellen forudsiger at privatforbruget stiger,
# og hvor ofte det reelt stiger:
conf <- table(Forudsagt = dat_log$pred, Faktisk = dat_log$dummy_variabel)

# RESULTAT af hvor often den forudsiger forbruget stiger og hvor ofte det reelt stiger:
# Forudsigelse = 91 gange
# Reelt = 82 gange

# Trin 4 - Lav et søjlediagram der viser hvor ofte modellen har ret i at forbruget stiger:

# Trin 4.1 - Antal korrekte:
korrekte_stigninger <- conf["1","1"]
korrekte_fald <- conf["0","0"]

# Trin 4.2 - Samlet antal faktiske stigninger og fald
antal_stigninger <- sum(conf[ , "1"])
antal_fald <- sum(conf[ , "0"])

# Trin 4.3 - Lav dataframe med "korrekte ud af total"
df_korrekt <- data.frame(
  Kategori = c("Stigning", "Fald"),
  Korrekte = c(korrekte_stigninger,korrekte_fald),
  Total    = c(antal_stigninger, antal_fald)
)

# Trin 4.4 - Beregn procentandelen for korrekte pr. kategori
df_korrekt$Procent <- round(df_korrekt$Korrekte / df_korrekt$Total * 100, 1)

# Trin 4.5 - Lav søjlediagram der viser antal korrekte og procentandelen der er korrekte:
ggplot(df_korrekt, aes(x = Kategori, y = Procent, fill = Kategori)) +
  geom_col() +
  geom_text(aes(label = paste0(Korrekte, "/", Total, " (", Procent, "%)")),
            nudge_y = 3) +
  labs(title = "Modellens præcision pr. kategori",
       caption = "Kilde: Danmarks Statistik",
       x = "", y = "Korrekt forudsagte i %") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0))

#
# Opgave 3.4 – Potentielle forbedringer af model
#

# Hvordan kan I prøve at forbedre jeres model?
# Lav 2 valgfrie scenarier og test dem i forhold til jeres baseline.

# Scenarie A:

# I vores baseline-model brugte vi en standard cutoff-værdi på 0,5, hvilket betyder,
# at modellen forudsiger en stigning (1), hvis sandsynligheden er over 50%.
# Dette kan dog være problematisk, da vores datasæt er skævt fordelt:
# 74,5% af observationerne er stigninger, mens kun 25,5% er fald.
# Hvis man altid gætter på stigning, vil man derfor få mange rigtige bare pga. ubalancen
# - men modellen bliver ikke nødvendigvis bedre.
# For at tage højde for denne ubalance har vi i dette scenarie justeret
# cutoff-værdien til datasættets reelle andel af stigninger, dvs. 0,745.

# Trin 1 - Vi laver et objekt der indeholder gennemsnitværdien (hvor ofte 1 fremgår) i "dat_log":
cut_mean <- mean(dat_log$dummy_variabel)
# RESULTAT = 1 fremgår i 74,5% af kvartalerne.

# Trin 2 - Tilføj en ny kolonne med nye dummyvariabler, justeret efter den nye cutoff på 74,5:
dat_log$pred_justeret <- ifelse(dat_log$pred_prob > cut_mean, 1, 0)

# Trin 3 - Lav en ny konfusionsmatrix for den justerede cutoff:
conf_scA <- table(Forudsagt = dat_log$pred_justeret, Faktisk = dat_log$dummy_variabel)

# Trin 4 - Hent hvor mange rigtige (korrekte stigninger og fald) for justeret cutoff:
korrekte_stigninger_justeret <- conf_scA["1","1"]
korrekte_fald_justeret <- conf_scA["0","0"]

# Trin 5 - Lav procentvis korrekthed for både stigning og fald:
antal_stigninger_justeret <- sum(conf_scA[ , "1"])
antal_fald_justeret <- sum(conf_scA[ , "0"])

df_korrekt_justeret <- data.frame(
  Kategori = c("Stigning", "Fald"),
  Korrekte = c(korrekte_stigninger_justeret, korrekte_fald_justeret),
  Total    = c(antal_stigninger_justeret, antal_fald_justeret)
)

df_korrekt_justeret$Procent <- round(df_korrekt_justeret$Korrekte / df_korrekt_justeret$Total * 100, 1)

# Trin 6 - Lav et visuelt søjleddiagram:
ggplot(df_korrekt_justeret, aes(x = Kategori, y = Procent, fill = Kategori)) +
  geom_col() +
  geom_text(aes(label = paste0(Korrekte, "/", Total, " (", Procent, "%)")),
            nudge_y = 3) +
  labs(title = "Scenarie A: Modellens præcision med justeret cutoff",
       caption = "Kilde: Danmarks Statistik",
       x = "", y = "Korrekt forudsagte i %") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0))

# Scenarie B:

# I scenarie B har vi forsøgt at forbedre modellen ved at inddrage forbrugertillidsindikatoren med et kvartals forsinkelse (lag 1).
# Tanken bag dette er, at husholdningernes forbrug typisk ikke reagerer øjeblikkeligt på ændringer i tilliden,
# men at der kan være en vis tidsforskydning. Et fald i forbrugertilliden i ét kvartal kan derfor først slå igennem i privatforbruget i det efterfølgende kvartal.
# For at teste denne antagelse oprettede vi en ny variabel (DIFTI_lag1), der indeholder værdien af DI’s forbrugertillidsindikator fra forrige kvartal.
# Denne blev herefter anvendt som forklarende variabel i en logistisk regression, hvor dummyvariablen (stigning/fald i forbrug) er den afhængige variabel.
# På den måde kan vi undersøge, om modellen bliver mere præcis, når vi tager højde for den forsinkede effekt af forbrugertilliden.
# Formålet med denne tilgang er at se, om modellens forudsigelsesevne forbedres, og samtidig vurdere,
# om det giver et mere realistisk billede af sammenhængen mellem forbrugertillid og husholdningernes forbrugsvækst.

# Trin 1 - Opret en ny kolonne med DIFTI forskudt ét kvartal tilbage:
dat_log$DIFTI_lag1 <- dplyr::lag(dat_log$DIFTI, 1)

# Trin 2 - Estimer en ny logistisk regression med den forsinkede indikator:
log_lag1 <- glm(dummy_variabel ~ DIFTI_lag1,
                  data = dat_log,
                  family = binomial,
                   na.action = na.exclude)

# Trin 3 - Forudsig sandsynligheder:
dat_log$pred_prob_B <- predict(log_lag1, type = "response")

# Trin 4 - Oversæt til 0/1 forudsigelser (cutoff = 0,5):
dat_log$pred_B <- ifelse(dat_log$pred_prob_B > 0.5, 1, 0)

# Trin 5 - Evaluer modellens præcision:
mean(dat_log$pred_B == dat_log$dummy_variabel, na.rm = TRUE)

# Trin 6 - Lav en ny konfusionsmatrix for at sammenligne:
conf_scB <- table(Forudsagt = dat_log$pred_B, Faktisk = dat_log$dummy_variabel)

# Trin 7 - Hent hvor mange rigtige (korrekte stigninger og fald) for forskudt DIFTI:
korrekte_stigninger_forskudt <- conf_scB["1","1"]
korrekte_fald_forskudt <- conf_scB["0","0"]

# Trin 8 - Lav procentvis korrekthed for både stigning og fald:
antal_stigninger_forskudt <- sum(conf_scB[ , "1"])
antal_fald_forskudt <- sum(conf_scB[ , "0"])

df_korrekt_forskudt <- data.frame(
  Kategori = c("Stigning", "Fald"),
  Korrekte = c(korrekte_stigninger_forskudt, korrekte_fald_forskudt),
  Total    = c(antal_stigninger_forskudt, antal_fald_forskudt)
)

df_korrekt_forskudt$Procent <- round(df_korrekt_forskudt$Korrekte / df_korrekt_forskudt$Total * 100, 1)

# Trin 9 - Lav et visuelt søjleddiagram:
ggplot(df_korrekt_forskudt, aes(x = Kategori, y = Procent, fill = Kategori)) +
  geom_col() +
  geom_text(aes(label = paste0(Korrekte, "/", Total, " (", Procent, "%)")),
            nudge_y = 3) +
  labs(title = "Scenarie B: Modellens præcision med forskudt DIFTI",
       caption = "Kilde: Danmarks Statistik",
       x = "", y = "Korrekt forudsagte i %") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0))