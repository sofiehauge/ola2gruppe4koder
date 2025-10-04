#
# Opgave 5 – Eurostat og API
#
# Opgave 5.1 – Kvartalsvis årlig realvækst for en række Eurolande
# Beregn den kvartalsvise årlige realvækst for husholdningernes forbrugsudgift for
# Danmark, Belgien, Holland, Sverige, Østrig, Tyskland, Frankrig, Italien og Spanien
# i perioden 1. kvartal 2000 til og med 2. kvartal 2025. I skal hente data vha. API’et fra Eurostat.

# Trin 0 - Hent de nødvendige R-pakker:
library(dplyr)      
library(stringr)  
library(eurostat)
library(ggplot2)

# Trin 1 - Først hentes alle tabellerne fra eurostat ned så vi kan søge i dem:
alltabs <- get_eurostat_toc()

# Trin 2 - Søg efter tabeller hvor titlen nævner husholdingernes forbrugsudgift:
sel <- alltabs %>%
  select(title, code) %>%
  filter(str_detect(title, regex("Final consumption",  ignore_case = TRUE))) %>%
  filter(str_detect(title, regex("households ", ignore_case = TRUE)))

# Vi ved at vi skal have data fra husholdningernes forbrugdudgifter fordelt kvartalvis,
# derfor er det datasættet "tipsho41" vi skal hente ned, da det passer til vores opgave.

# Trin 3 - Hent metadata/DSD for tabellen tipsho41:
pforbrug <- get_eurostat_dsd("tipsho41")

# Trin 4 - Så skal vi se hvilke variabler/concepts der er, og hvilke relevante værdier der er,
# så vi kan filtrere i det for senere at lave et datasæt:
unique(pforbrug$concept)

# Trin 5 - Her laves der en filter-liste, hvor vi skriver hvilke værdier vi vil have med.
# I dette tilfælde tager vi alle landende fra opgaven, kvartalvis (Quarterly) og Current prices:
eu_filter = list(
  freq = "Q",
  unit ="CP_MNAC",
  geo = c("DK", "BE", "NL", "SE", "AT", "DE","FR", "IT", "ES")
)

# Trin 6 - Så laver vi en ny data frame med de filtre som vi har lavet:
dfp_forbrug <- get_eurostat_data("tipsho41",
                                 filters = eu_filter,
                                 date_filter= "1999": "2025")

# Trin 7 - Rens datasættet så vi kun har lande, tid og værdien (i mio. national valuta):
dfp_forbrug <- dfp_forbrug %>%
  select(-na_item, -unit, -s_adj)

# Trin 7.1 - Navngiv kolonnerne noget relevant:
colnames(dfp_forbrug) <- c("land",
                           "kvartal",
                           "privat_forbrug")

# Trin 8 - Nu skal kolonnen "Priat forbrug" laves til en årlig realvækst,
# så vi ser procentstigningen siden samme kvartal året før.
# Datasættet grupperes først efter lande, så den årlige realvækst kun bliver taget inden for det samme land,
# og ikke lapper ind over hinanden.
# Derudover fjerne vi NA'erne her:
pforbrug_result <- dfp_forbrug %>%
  group_by(land) %>%                    # Sørger for, at hvert land behandles for sig selv.
  mutate(                               # Tilføjer en ny kolonne til datasættet.
    aarlig_pct_vaekst =    
      (exp(log(privat_forbrug) - lag(log(privat_forbrug), 4)) - 1) * 100) %>%
    filter(!is.na(aarlig_pct_vaekst))  # Fjerne NA'er

# lag(log(Privat_forbrug), 4) --> Henter log-værdien 4 perioder tilbage (dvs. samme kvartal året før).
# -1 --> Gør ratioen til en vækstrate.
# * 100 --> Omregner til procent.

# Trin 9 - Lav et linje plot der visualiserer hvert lands årlig realvækst i privatforbruget:
ggplot(pforbrug_result, aes(x = kvartal, y = aarlig_pct_vaekst, group = land)) +
  geom_line(color = "red") +
  facet_wrap(~ land, ncol = 3) +
  scale_x_discrete(
    breaks = function(x) x[seq(1, length(x), by = 4)],   # viser hver 4. kvartal
    labels = function(x) str_remove(x, "-Q[1-4]$")        # fjerner Q1, Q2, osv.
  ) +
  labs(
    title = "Kvartalvise årlig realvækst i privatforbrug pr. land",
    x = "År", y = "Vækst (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)   # roter labels
  )

#
# Opgave 5.2 – Højeste kvartalsvise årlige realvækst
#
# Hvilket af de lande har gennemsnitligt haft den højeste kvartalsvise årlige realvækst
# i husholdningernes forbrugsudgift i perioden 1. kvartal 2000 til 2. kvartal 2025.

# Trin 1 - Beregn gennemsnit pr. land:
gennemsnit <- tapply(pforbrug_result$aarlig_pct_vaekst,
                     pforbrug_result$land,
                     mean)

# Trin 2 - Find land med højeste gennemsnit:
which.max(gennemsnit)      # viser navnet på landet
max(gennemsnit)            # viser selve gennemsnittet

# Trin 3 - Lav en data frame med gennemsnittet 
df_gennemsnit <- data.frame(
  land = names(gennemsnit),
  gennemsnit_vaekst = as.numeric(gennemsnit)
)

#
# Opgave 5.3 – Coronakrisen som outlier
#
# Fjern Coronakrisen fra jeres data og find igen den gennemsnitligt kvartalsvise realvækst i
# husholdningernes forbrugsudgift i perioden 1. kvartal 2000 til 2. kvartal 2024.
# I hvilket af landene har Coronakrisen haft den største effekt på den gennemsnitligt kvartalsvise realvækst.

# Trin 0 - Download "zoo"-pakken, som bruges til at lave det om til kvartaler:
library("zoo")

# Trin 1 - Lav teksten under kvartal-kolonnen om til en kvartalsvariabel, som kan sammenlignes som datoer:
pforbrug_result$kvartal <- as.yearqtr(pforbrug_result$kvartal, format = "%Y-Q%q")

# Trin 2 - Definér coronakrisen:
corona_start <- as.yearqtr("2020 Q1")
corona_slut  <- as.yearqtr("2020 Q3")

# Trin 3 - Beregn gennemsnit uden coronakrisen:

# Trin 3.1 - Først laver vi et datasæt hvor coronakrisen er fjerenet:
df_uden <- pforbrug_result %>%
  filter(kvartal < corona_start | kvartal > corona_slut)

# Trin 3.2 - Beregn gennemsnittet for hvert land, nu uden coronakrisen:
gennemsnit_uden <- tapply(df_uden$aarlig_pct_vaekst,
                          df_uden$land,
                          mean)

# Trin 3.3 - Lav et dataframe med gennemsnittet for hvert land:
df_gennemsnit_uden <- data.frame(
  land = names(gennemsnit_uden),
  gennemsnit_vaekst = as.numeric(gennemsnit_uden)
)

# Trin 4 - Lav et samlet dataframe der viser gennemsnit både med og uden corona samt forskellen i de to gennemsnit:
samlet <- df_gennemsnit %>%
  rename(gennemsnit_med = gennemsnit_vaekst) %>%
  full_join(
    df_gennemsnit_uden %>% rename(gennemsnit_uden = gennemsnit_vaekst),
    by = "land"
  ) %>%
  mutate(
    forskel = gennemsnit_uden - gennemsnit_med
  )

# Trin 5 - Lav et boxplot-graf, der viser coronaens kvartaler som outliers:
# Trin 5.1 - Først laves et nyt dataframe, der indeholde hele perioden, men hvor coronaperioden også er defineret med TRUE eller FALSE:
p_box <- pforbrug_result %>%
  mutate(                         # Tilføjer en ny kolonne
    corona_periode = kvartal >= as.yearqtr("2020 Q1") & kvartal <= as.yearqtr("2020 Q3")
  )
# "as.yearqtr("2020 Q1") og as.yearqtr("2020 Q3")" --> laver start- og slutgrænser for coronaperioden.
# ">= ... & <=" ... --> tjekker for hver række, om kvartalet ligger mellem 2020 Q1 og 2020 Q3 (inkl.).

ggplot(p_box, aes(x = land, y = aarlig_pct_vaekst)) +
  geom_boxplot(outlier.shape = NA) +      # undgå dobbelt-outliers
  geom_point(                             # Tilføjer en nyt lag med punkter  
    data = subset(p_box, corona_periode), # Bruger kun de rækker, hvor corona_periode == TRUE
    aes(x = land, y = aarlig_pct_vaekst), # Samme akser som boxplottet
    color = "red",                        # Laver prikkere røde
  ) +
  coord_flip() +                          # Bytter om på x- og y-aksen: Landene kommer nu på y-aksen (venstre side) - Vækstraterne ligger vandret
  labs(
    title = "Coronaperioden fremstår som outliers i vækstfordelingen i flere lande",
    x = "Land",
    y = "Årlig vækst (%)"
  ) +
  theme_minimal()

# geom_boxplot() laver en boxplot for hvert land.
# outlier.shape = NA --> De “klassiske” outlier-prikker som automatisk tegnes, slåes fra, fordi vi selv tilføjer de røde coronaprikker bagefter.


#
# Opgave 5.4 – Effekt af Corona på forbruget
#
# I hvilket europæiske land faldt den gennemsnitligt kvartalsvise realvækst i husholdningernes forbrugsudgift,
# i perioden 1. kvartal 2020 til 2. kvartal 2024, mest?

# Trin 1 - Udtræk kun de perioden K1 2020 til K2 2025 for hvert land:
df_perioden <- subset(pforbrug_result,
                      kvartal >= as.yearqtr("2020 Q1") & kvartal <= as.yearqtr("2025 Q2"))

# Trin 2 - Beregn gennemsnitter for perioden pr. land:
gennemsnit_perioden <- tapply(df_perioden$aarlig_pct_vaekst,
                        df_perioden$land,
                        mean)

# Trin 3 - Lav et dataframe med gennemsnittet for perioden for hvert land:
df_gennemsnit_perioden <- data.frame(
  land = names(gennemsnit_perioden),
  gennemsnit_vaekst = as.numeric(gennemsnit_perioden)
)

# Trin 4 - Print landene med det største fald i perioden:
which.min(gennemsnit_perioden)   # viser landekoden
min(gennemsnit_perioden)         # viser gennemsnittet


