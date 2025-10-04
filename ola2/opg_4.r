#
# Opgave 4 – Forbrug og forbrugertillidsindikator fra DST og DI, samt loops i lister
#
# Opgave 4.1 – Illustration af forbrugertillid
# Hent data for forbrugertillidsundersøgelsen fra januar 1996 til i dag og omregn jeres data til
# kvartaler. Lav en grafisk illustration af jeres omregnede data for DST’s forbrugertillidsindikator og
# kommentér på, hvornår de danske forbrugere er mest og mindst optimistiske.

# Da vores tidligere forbrugertillidsindikator-datasæt kun går ned til 1997,
# vælger vi her at hente dataen ned igen, igennem API. Vi ved allerede hvad datasættet vi ksal bruge hedder,
# da vi har arbejdet med det i de tidligere opgaver. Derfor behover vi ikke hente alle tabeller ned
# og søge efter relevante datasæt. Dog skriver vi koden ind her alligevel, da dette er den rigtige måde
# at gøre det på, hvis ikke man i forvejen ved hvad datasættet man søger hedder.

# Trin 0 - Hent alle de relevante pakker:
library(dkstat)
library(tidyverse)
library(ggplot2)

# Trin 1 - Hent alle mulige tabeller ned fra Danmarks Statistik:
all_tabs <- dst_get_tables()

# Trin 2 - Søg igennem alle tabellerne for kun at finde dem der indeholde relevante variabler 
dst_search("Forbrugerforventninger")

# Trin 3 - Indhent metadataen for FORV1

forvent<- dst_meta(table = "FORV1", lang = "da")

# Trin 4 - Lav en liste med de filtre vi vil have påsat det data vi henter ned fra DST:

# Trin 4.1 - Først ser vi hvad variablerne og værdier hedder i datasættet,
# så vi kan skrive det rigtigt ind i filter-funktionen:

forvent$variables
forvent$values
forvent$basics
forvent$values$Tid

# Trin 4.2 - Så laver vi en liste med filtre:
my_filters <- list(
  INDIKATOR= "Forbrugertillidsindikatoren",
  Tid       = "*"
)

# Trin 5 - Hent data fra DST med kriterierne fra ovenstående filter-liste:

df_forvent <- dst_get_data(table = "FORV1", query = my_filters)

# Trin 6 - Rens dataen så vi kan arbejde med den:

# Trin 6.1 - Først vender vi tabellen på den berede led, så tiden er en kolonne,
# forbrugertillidsindikatoren er en kolonne og værdien står under forbrugertilldsindiaktoren:
df_forvent <- df_forvent %>%
  pivot_wider(
    names_from = INDIKATOR,   # Henter kolonnenavnene fra INDIKATOR
    values_from = value       # Fylder cellerne med værdier fra 'value'
  )

# Trin 6.2 - Vi fjerner årene op til 1996M01:
df_forvent_1996 <- df_forvent %>%
  filter(TID >= "1996-01-01")

# Trin 7 - Omregn månederne til kvartaler:

# Trin 7.1 - Lav en kolonne med år og en med kvartaler ud fra datoen:
df_forvent_1996$År       <- as.integer(format(df_forvent_1996$TID, "%Y"))
df_forvent_1996$Kvartal  <- (as.integer(format(df_forvent_1996$TID, "%m")) - 1) %/% 3 + 1

# Trin 7.2 - Vi laver et gennemsnit af forbrugertilliden for 3 måneder (et kvartal):
df_kvartal <- aggregate(`F1 Forbrugertillidsindikatoren` ~ År + Kvartal,
                        data = df_forvent_1996,
                        FUN  = mean, na.rm = TRUE)

# Trin 7.3 - Så sorterer vi det i den rigtig rækkefølge:
df_kvartal <- df_kvartal[order(df_kvartal$År, df_kvartal$Kvartal), ]


# Trin 7.4 - Vi laver en tekst-kolonne som indeholder kvartalnavnene:
df_kvartal$kvartal_label <- paste0(df_kvartal$År, "Q", df_kvartal$Kvartal)

# Trin 8 - Tegn en linjegraf:
ggplot(df_kvartal, aes(x = kvartal_label, y = `F1 Forbrugertillidsindikatoren`, group = 1)) +
  geom_line() +
  geom_point(size = 0.8) +
  scale_x_discrete(
    breaks = function(x) x[seq(1, length(x), by = 4)],   # viser hver 4. kvartal
    labels = function(x) str_remove(x, "-Q[1-4]$")        # fjerner Q1, Q2, osv.
  ) +
  labs(
    title = "Udvikling i forbrugertillid (kvartaler)",
    x = "Kvartal",
    y = "Forbrugertillidsindikator"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7)
  )


#
#Opgave 4.2 – Gennemsnit af underspørgsmål
#
# Beregn gennemsnittet for underspørgsmålet
# ”Set i lyset af den økonomiske situation, mener du, at det for øjeblikket er fordelagtigt at anskaffe større forbrugsgoder som fjernsyn, vaskemaskine eller lignende, eller er det bedre at vente?”
# for perioden 1. kvartal 2000 til og med 3. kvartal 2025.

# Trin 1 - Lav et nyt filter, som skal bruges på samme datasæt fra DST (FORV1):
new_filters <- list(
  INDIKATOR= "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
  Tid       = "*"
)

# Trin 2 - Hent data fra DST med kriterierne fra ovenstående filter-liste:
df_anskaffelse <- dst_get_data(table = "FORV1", query = new_filters)

# Trin 3 - Rens dataen så vi kan arbejde med den:

# Trin 3.1 - Først vender vi tabellen på den berede led, så tiden er en kolonne,
# forbrugertillidsindikatoren er en kolonne og værdien står under forbrugertilldsindiaktoren:
df_anskaffelse <- df_anskaffelse %>%
  pivot_wider(
    names_from = INDIKATOR,   # Henter kolonnenavnene fra INDIKATOR
    values_from = value       # Fylder cellerne med værdier fra 'value'
  )

# Trin 3.2 - Vi fjerner årene op til 2000 M01:
df_anskaffelse2000 <- df_anskaffelse %>%
  filter(TID >= "2000-01-01")

# Trin 4 - Formater kolonnen TID til Dato:
df_anskaffelse2000$TID <- as.Date(df_anskaffelse2000$TID)
# Af en eller anden grund, starter TID så nu den 31. januar 1999. Derfor:

# Trin 4.1 - Filtrer så datasættet starter fra 2000-01-01:
df_anskaffelse2000 <- df_anskaffelse2000 %>%
  filter(TID >= as.Date("2000-01-01"))

# Trin 5 - Træk årstallet ud af datoen i TID og gem det som en ny kolonne 'År':
df_anskaffelse2000$År  <- as.integer(format(df_anskaffelse2000$TID, "%Y"))

# Trin 6- Beregn gennemsnittet pr. år:
df_aar <- df_anskaffelse2000 %>%
  group_by(År) %>%
  summarise(aar_mean = mean(`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`, na.rm = TRUE))

# Trin 7 - Visualiserer med en linje plot, den gennemsnitlige indikatortal for spørgsmålet:
ggplot(df_aar, aes(x = År, y = aar_mean)) +
  geom_line() +
  geom_point(size = 1.5) +
  labs(
    title = "Årligt gennemsnit af 'Anskaffelse af større forbrugsgoder'",
    x = "År",
    y = "Gennemsnitlig vurdering"
  ) +
  theme_minimal()

# Trin 8 - Vis gennemsnittet for hele perioden 2000-2025:
mean(df_aar$aar_mean)

# RESULTAT = gennemsnittet for hele perioden er -10,62788

#
# Opgave 4.3 – De 11 grupper af forbrug
#
# Hent data for de 11 (15 ) grupper af forbrug blandt husholdningerne.

# Hvad brugte danskerne flest penge på i 2023?
#
# Trin 1 - Først søger vi efter tabeller der kan være relevante:

dst_search("Husholdningers forbrug")
# Her vurderer vi har NKHC021 er den relevante tabel til denne opgave.

# Trin 2 - Hent metadata for tabellen "NKHC021":
meta <- dst_meta(table="NKHC021", lang = "da")

# Trin 2.1 - Så tjekker vi hvad variablerne og værdierne hedder, så vi kan lave et filter:
meta$variables
meta$basics
meta$values$PRISENHED$text
meta$values$SÆSON$text
# Her gemmes alle værdierne (grupperne) under "Formaal" som en vektor:
values <- meta$values$FORMAAAL$text
# Når vi printer vektoren, kan vi se alle grupperne:
values

# Trin 3 - Lav filter der indeholder de 15 grupper, kædede værdier og alle kvartalerne:
filter_grp <- list(
  FORMAAAL = "*",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid       = "*")

# Trin 4 - Hent datasættet over de 15 forbrugsgrupper, ud fra ovenstående filter:
df_forbrug <- dst_get_data(table="NKHC021", query = filter_grp)

# Trin 4 - Så renses datasættet så vi nemmere kan arbejde med det:
# Trin 4.1 - Først fjerner vi "I alt" værdien under formål samt kolonner "Prisenhed" og "Sæson":
df_forbrug <- df_forbrug %>% filter(FORMAAAL != "CPT I alt")

df_forbrug <- df_forbrug %>% select(-PRISENHED, -SÆSON)

# Trin 4.2 - Så fjerner vi kvartalerne op til år 2020:
df_forbrug2020 <- df_forbrug %>%
  filter(TID >= "2020-01-01")

# Trin 5 - Så opretter vi en år-kolonne i datasættet:
df_forbrug2020$År <- format(as.Date(df_forbrug2020$TID), "%Y")

# Trin 6 - Så udregner vi et års gennemsnit af forbruget pr. gruppe, ligger det i et nyt dataframe
# i en kolonne der hedder "gennemsnit" :
aar_gns_grp <- aggregate(df_forbrug2020$value, 
                     by = list(gruppe = df_forbrug2020$FORMAAAL, år = df_forbrug2020$År),
                     FUN = mean)

colnames(aar_gns_grp)[3] <- "gennemsnit"

# Trin 7 - Vi laver et nyt dataframe kun med 2023 og udtrækker den gruppe danskerne brugte flest penge på i 2023:
aar2023 <- subset(aar_gns_grp, år == "2023")

vinder_2023 <- aar2023[which.max(aar2023$gennemsnit), ]

# RESULTATET 2023:
# Det danskerne har brugt flest penge på i 2023 er Boligbenyttelse (67.200 mio. kr.)

# Vi er også nysgerrige på hvad danskerne i gennemsnit har brugt flest penge på i 2025 indtil videre:
aar2025 <- subset(aar_gns_grp, år== "2025")

vinder_2025 <- aar2025[which.max(aar2025$gennemsnit), ]

# RESULTAT 2025:
# Det danskerne har brugt flest penge på i 2025 er også Boligbenyttelse (69.050 mio. kr.).
# Dog er der brugt flere penge i snit i 2025 end 2023 på boligbenyttelse.

# Trin 8 - Vi visualisere med en graf, hvilken gruppe der er brugt flest penge på i snit,
# hvert år i perioden 2020-2025:
ggplot(aar_gns_grp, aes(x = år, y = gennemsnit, fill = gruppe)) +
  geom_col(position = "dodge") +
  labs(title = "Årlig gennemsnitligt forbrug pr. gruppe",
       x = "År",
       y = "Gns. forbrug (mio. kr.)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Hvilken gruppe af forbruget steg mest fra 2020 til 2023?
#
# Trin 9 - Vi starter med at lave et dataframe kun med perioden 2020-2023:
aar2020_2023 <- subset(aar_gns_grp, år >= 2020 & år <= 2023)

# Trin 10 - Vi laver dataframen bred i stedet for lang, men hvor det er gns. pr. år der er i kolonnerne:
ændring <- aar2020_2023 %>%
  pivot_wider(
    names_from = år,
    values_from = gennemsnit
  )

# Trin 11 - Derefter beregner vi stigningen fra 2020 til 2023 (ved at minusse år 2023 med 2020):
ændring$stigning <- ændring$`2023` - ændring$`2020`

# RESULTATET af den gruppe der er steget mest (2020-2023) = Restauranter og hoteller (6.423,50 mio. kr.)

# Vi er også nysgerrige på hvad der er steget mest hvis vi kigger på år 2020 til 2025:
aar2020_2025 <- subset(aar_gns_grp, år >= 2020 & år <= 2025)

ændring2025 <- aar2020_2025 %>%
  pivot_wider(
    names_from = år,
    values_from = gennemsnit
  )

ændring2025$stigning <- ændring2025$`2025` - ændring2025$`2020`

# RESULTATET af den gruppe der er steget mest (2020-2025) = Restauranter og hoteller (6.753,50 mio. kr.)

# Vi visualisere lige stigningen pr. forbrugsgrupper:
ggplot(ændring2025, aes(x = reorder(gruppe, stigning), y = stigning, fill = stigning > 0)) +
  geom_col() +
  coord_flip() +  # gør diagrammet vandret for bedre læsbarhed
  labs(
    title = "Ændring i forbrug pr. gruppe (2020–2025)",
    x = "Forbrugsgruppe",
    y = "Ændring i forbrug (2020–2025)"
  ) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),  # Hvis fill er TRUE (altså større end 0) er den blå, hvis den er FALSE er den rød.
                    labels = c("FALSE" = "Fald", "TRUE" = "Stigning"),       ## Hvis fill er TRUE (altså større end 0) er teksten "stigning", hvis den er FALSE er den "fald".
                    name = "") +
  theme_minimal()

#
# Opgave 4.4 – 22 simple lineære regressioner
#
# Lav 22 simple lineære regressioner mellem hver af de 11 (15) grupper i forbruget (y-variable) og
# henholdsvis forbrugertillidsindikatoren fra DST og DI. I skal gemme summary i 22 lister.
# I skal lave jeres regressioner fra 1. kvartal 2000 til og med 2. kvartal 2025.

# Trin 1 - Hent den kvartalvise forbrugertillidsindikator for DST som vi allerede har udarbejdet i opg. 2.1:
forbrugertillid_dst

# Trin 2 - Hent og rens df_forbrug datasættet vi arbjede med i opg. 4.3:
df_15grp <- df_forbrug %>%
  filter(TID >= "1999-01-01")

# Trin 2.1 - Så formaterer vi datasættet til at være bred, hvor hver gruppe står som variabler i kolonnerne:
df_15grp <- df_15grp %>%
  pivot_wider(
    names_from = FORMAAAL,
    values_from = value)

# Trin 3 - Lav et nyt datasæt med privatforbrug-datasættet med de 15 grupper om til årlig procent vækst:
# Trin 3.1 - Først laves en funktion der kan lave en kolonne om til årlig realvæks, som kan bruge på alle forbrugsgrupperne:
grow_fun <- function(x) {
  v <- as.numeric(x)
  c(rep(NA, 4), (exp(diff(log(v), lag = 4)) - 1) * 100)
}

# Trin 3.2 - Brug funktionen på hele datasættet, så alle gruppernes nettotal erstattes med den årlige realvækst:
df_vaekst <- df_15grp %>%
  transmute(TID,
            across(-c(TID), grow_fun, .names = "{.col}_v"))

# Trin 3.3 - Rens datasættet igen.
# Fjern 1999 rækkerne og tilføj kvartalnavnene fra opg. 2.1 i stedet for månederne der står nu.
# Samtidig med at kolonnen navngives kvartal:
df_vaekst <- df_vaekst[-c(1:4), ]

df_vaekst <- df_vaekst %>%
  mutate(TID = kvartal_navne2025) %>%
  rename(kvartal = TID)

# Trin 4 - Merge dataframen med DST forbrugertillidsindiaktor og den med forbrugsgrupperne:

dst_merged <- df_vaekst %>%
  left_join(forbrugertillid_dst, by = "kvartal")

# Trin 5 - Lav en lineær model for hver af forbrugsgrupperne:
# Trin 5.1 - Først skal vi se hvad alle kolonnerne/grupperne rent faktisk hedder:
names(dst_merged)

# Trin 5.2 - Eftersom vi skal gemme alle forbrugsgruppernes lineære regression i hver deres liste, laver vi en lm og gemmer summary individuelt for hver grupper
# Dog samlet under en overordnet liste:
# En tom liste:
model_list <- list()

model_list[["CPA Fødevarer mv._v"]] <- summary(lm(`CPA Fødevarer mv._v` ~ Forbrugertillidsindikator,
                                                             data = dst_merged))
model_list[["CPB Drikkevarer og tobak mv._v"]] <- summary(lm(`CPB Drikkevarer og tobak mv._v` ~ Forbrugertillidsindikator,
                                                  data = dst_merged))
model_list[["CPC Beklædning og fodtøj_v"]] <- summary(lm(`CPC Beklædning og fodtøj_v` ~ Forbrugertillidsindikator,
                                                             data = dst_merged))
model_list[["CPD Boligbenyttelse_v"]] <- summary(lm(`CPD Boligbenyttelse_v` ~ Forbrugertillidsindikator,
                                                  data = dst_merged))
model_list[["CPE Elektricitet, fjernvarme og andet brændsel_v"]] <- summary(lm(`CPE Elektricitet, fjernvarme og andet brændsel_v` ~ Forbrugertillidsindikator,
                                                             data = dst_merged))
model_list[["CPF Boligudstyr, husholdningstjenester mv._v"]] <- summary(lm(`CPF Boligudstyr, husholdningstjenester mv._v` ~ Forbrugertillidsindikator,
                                                         data = dst_merged))
model_list[["CPG Medicin, lægeudgifter o.l._v"]] <- summary(lm(`CPG Medicin, lægeudgifter o.l._v` ~ Forbrugertillidsindikator,
                                                    data = dst_merged))
model_list[["CPH Køb af køretøjer_v"]] <- summary(lm(`CPH Køb af køretøjer_v` ~ Forbrugertillidsindikator,
                                                                               data = dst_merged))
model_list[["CPI Drift af køretøjer og transporttjenester_v"]] <- summary(lm(`CPI Drift af køretøjer og transporttjenester_v` ~ Forbrugertillidsindikator,
                                                                           data = dst_merged))
model_list[["CPJ Information og kommunikation_v"]] <- summary(lm(`CPJ Information og kommunikation_v` ~ Forbrugertillidsindikator,
                                                               data = dst_merged))
model_list[["CPK Fritid, sport og kultur_v"]] <- summary(lm(`CPK Fritid, sport og kultur_v` ~ Forbrugertillidsindikator,
                                                     data = dst_merged))
model_list[["CPL Undervisning_v"]] <- summary(lm(`CPL Undervisning_v` ~ Forbrugertillidsindikator,
                                                                             data = dst_merged))
model_list[["CPM Restauranter og hoteller_v"]] <- summary(lm(`CPM Restauranter og hoteller_v` ~ Forbrugertillidsindikator,
                                                                 data = dst_merged))
model_list[["CPN Forsikring og finansielle tjenester_v"]] <- summary(lm(`CPN Forsikring og finansielle tjenester_v` ~ Forbrugertillidsindikator,
                                                            data = dst_merged))
model_list[["CPO Andre varer og tjenester_v"]] <- summary(lm(`CPO Andre varer og tjenester_v` ~ Forbrugertillidsindikator,
                                                 data = dst_merged))

# Trin 6 - Visualiser forklaringsgraden i dataframe og et søjlediagram:
# Træk R²-værdier ud fra alle modeller
r2_dst <- data.frame(
  gruppe = names(model_list),
  r_squared = sapply(model_list, function(m) m$r.squared)
)

# Søjlediagram:
ggplot(r2_dst, aes(x = reorder(gruppe, r_squared), y = r_squared)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Forklaringsgrad (R²) for hver forbrugsgruppe (DST's forbrugertillidsindikator)",
    x = "Forbrugsgruppe",
    y = "R²"
  ) +
  theme_minimal()

# Så skal det hele laves igen, bare med DI's forbrugertillidsindikator

# Trin 1 - Hent den kvartalvise forbrugertillidsindikator for DI som vi allerede har udarbejdet i opg. 2.1,
# men udtræk kun kolonnerne "kvartal" og "DIFTI":
DIFTI <- forvent_DI_kvartaler2025[, c(1, 6)]

# Trin 2 - Merge dataframen med DI's forbrugertillidsindiaktor og den med forbrugsgrupperne (df_vaekst):

di_merged <- df_vaekst %>%
  left_join(DIFTI, by = "kvartal")

# Trin 3 - Lav en lineær model for hver af forbrugsgrupperne sammenholdt med DIFTI:

# Trin 3.2 - Her genrbruger vi koden for DST's forbrugertillid, men udskifter med DI's:
# En tom liste:
model_list_DI <- list()

model_list_DI[["CPA Fødevarer mv._v"]] <- summary(lm(`CPA Fødevarer mv._v` ~ DIFTI,
                                                  data = di_merged))
model_list_DI[["CPB Drikkevarer og tobak mv._v"]] <- summary(lm(`CPB Drikkevarer og tobak mv._v` ~ DIFTI,
                                                             data = di_merged))
model_list_DI[["CPC Beklædning og fodtøj_v"]] <- summary(lm(`CPC Beklædning og fodtøj_v` ~ DIFTI,
                                                         data = di_merged))
model_list_DI[["CPD Boligbenyttelse_v"]] <- summary(lm(`CPD Boligbenyttelse_v` ~ DIFTI,
                                                    data = di_merged))
model_list_DI[["CPE Elektricitet, fjernvarme og andet brændsel_v"]] <- summary(lm(`CPE Elektricitet, fjernvarme og andet brændsel_v` ~ DIFTI,
                                                                               data = di_merged))
model_list_DI[["CPF Boligudstyr, husholdningstjenester mv._v"]] <- summary(lm(`CPF Boligudstyr, husholdningstjenester mv._v` ~ DIFTI,
                                                                           data = di_merged))
model_list_DI[["CPG Medicin, lægeudgifter o.l._v"]] <- summary(lm(`CPG Medicin, lægeudgifter o.l._v` ~ DIFTI,
                                                               data = di_merged))
model_list_DI[["CPH Køb af køretøjer_v"]] <- summary(lm(`CPH Køb af køretøjer_v` ~ DIFTI,
                                                     data = di_merged))
model_list_DI[["CPI Drift af køretøjer og transporttjenester_v"]] <- summary(lm(`CPI Drift af køretøjer og transporttjenester_v` ~ DIFTI,
                                                                             data = di_merged))
model_list_DI[["CPJ Information og kommunikation_v"]] <- summary(lm(`CPJ Information og kommunikation_v` ~ DIFTI,
                                                                 data = di_merged))
model_list_DI[["CPK Fritid, sport og kultur_v"]] <- summary(lm(`CPK Fritid, sport og kultur_v` ~ DIFTI,
                                                            data = di_merged))
model_list_DI[["CPL Undervisning_v"]] <- summary(lm(`CPL Undervisning_v` ~ DIFTI,
                                                 data = di_merged))
model_list_DI[["CPM Restauranter og hoteller_v"]] <- summary(lm(`CPM Restauranter og hoteller_v` ~ DIFTI,
                                                             data = di_merged))
model_list_DI[["CPN Forsikring og finansielle tjenester_v"]] <- summary(lm(`CPN Forsikring og finansielle tjenester_v` ~ DIFTI,
                                                                        data = di_merged))
model_list_DI[["CPO Andre varer og tjenester_v"]] <- summary(lm(`CPO Andre varer og tjenester_v` ~ DIFTI,
                                                             data = di_merged))


# Trin 4 - Visualiser forklaringsgraden i dataframe og et søjlediagram:
# Træk R²-værdier ud fra alle modeller
r2_di <- data.frame(
  gruppe = names(model_list_DI),
  r_squared = sapply(model_list_DI, function(m) m$r.squared)
)

r2_di$r_squared <- round(r2_di$r_squared, 3)

# Søjlediagram:
ggplot(r2_di, aes(x = reorder(gruppe, r_squared), y = r_squared)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Forklaringsgrad (R²) for hver forbrugsgruppe (DI's forbrugertillidsindikator)",
    x = "Forbrugsgruppe",
    y = "R²"
  ) +
  theme_minimal()
