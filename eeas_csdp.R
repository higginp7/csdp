library(rvest)

read_html("https://en.wikipedia.org/wiki/List_of_military_and_civilian_missions_of_the_European_Union") %>%
  html_table(header = TRUE, fill = TRUE) %>% 
  `[[`(3) -> eu_wiki

eu_wiki %<>%
  janitor::clean_names() %>% 
  mutate(
    no_day = grepl("^[0-9][A-Za-z0-9 -]*$", beginning),
    start = ifelse(no_day == FALSE, paste(1, beginning, sep = " "), beginning),
    start = lubridate::dmy(start)) %>% 
  mutate(
    no_day = grepl("^[0-9][A-Za-z0-9 -]*$", end),
    end = ifelse(no_day == FALSE, paste(1, end, sep = " "), end),
    end = lubridate::dmy(end)
  ) 

eu_wiki %<>% 
  mutate(across(where(is.character), ~ str_replace(., "\\[[0-9]+\\]", ""))) %>% 
  mutate(across(where(is.character), ~ str_replace_all(., "\\[[a-zA-Z0-9]+\\]", "")))

eu_wiki %<>% 
  mutate(country = countrycode(name, "country.name", "country.name")) 

eu_wiki %<>% 
  select(!no_day)

eu_wiki %<>% 
  mutate(country = ifelse(grepl("Rafah", name), "Palestinian Territories", country)) %>% 
  mutate(country = ifelse(grepl("Aceh", name, ignore.case = TRUE), "Indonesia", country))

# eu_wiki$duration <- as.numeric(difftime(eu_wiki$end, eu_wiki$start, units = "days"))

eu_wiki %<>% 
  mutate(type = str_extract(abbreviation, "^[^\\s]+")) %>% 
  mutate(type = ifelse(country == "Somalia" & abbreviation == "EU NAVFOR Somalia",
                       "EU NAVFOR", type))

eu_wiki %<>% 
  mutate(country = ifelse(grepl("EUNAVFOR Med", abbreviation), "Libya", country))

# ADDING MOLDOVA, UKRAINE AS SEPARATE COUNTRIES
# Define the pattern you want to match

pattern_to_match <- "EUBAM Moldova and Ukraine"

# Filter the rows that match the pattern
moldova_row <- eu_wiki %>%
  filter(abbreviation == pattern_to_match)

moldova_row %<>% 
  mutate(country = ifelse(grepl("EUBAM Moldova and Ukraine", abbreviation), "Moldova", country),
         abbreviation = ifelse(grepl("EUBAM Moldova and Ukraine", abbreviation), "EUBAM Moldova", abbreviation))

eu_wiki %<>% 
  mutate(country = ifelse(grepl("EUBAM Moldova and Ukraine", abbreviation), "Ukraine", country),
         abbreviation = ifelse(grepl("EUBAM Moldova and Ukraine", abbreviation), "EUBAM Ukraine", abbreviation))


# Replicate the matched rows
eu_wiki <- bind_rows(eu_wiki, moldova_row)

# ADDING CHAD, Central African Republic AS SEPARATE COUNTRIES
# Define the pattern you want to match
pattern_to_match <- "EUFOR Tchad/RCA"

# Filter the rows that match the pattern
chad_row <- eu_wiki %>%
  filter(abbreviation == pattern_to_match)

chad_row %<>% 
  mutate(country = ifelse(grepl("EUFOR Tchad/RCA", abbreviation), "Chad", country),
         abbreviation = ifelse(grepl("EUFOR Tchad/RCA", abbreviation), "EUFOR Tchad", abbreviation))

eu_wiki %<>% 
  mutate(country = ifelse(grepl("EUFOR Tchad/RCA", abbreviation), "Central African Republic", country),
         abbreviation = ifelse(grepl("EUFOR Tchad/RCA", abbreviation), "EUFOR RCA", abbreviation))

eu_wiki <- bind_rows(eu_wiki, chad_row)

# eeas %<>% 
#   select(abbreviation = mission, everything())
# test <- eu_wiki %>% 
#   full_join(eeas, by = "abbreviation")


##### Test with the ongoing missions according to the EEAS #####

# Create a vector of EU missions
# eu_missions <- c(
#   "EU NAVFOR Somalia",
#   "EU RACC Sahel",
#   "EUAM Iraq",
#   "EUAM RCA",
#   "EUAM Ukraine",
#   "EUBAM Libya",
#   "EUBAM Moldova and Ukraine",
#   "EUBAM Rafah",
#   "EUCAP Sahel Mali",
#   "EUCAP Sahel Niger",
#   "EUCAP Somalia",
#   "EUFOR Althea",
#   "EULEX Kosovo",
#   "EUM Armenia",
#   "EUMAM Ukraine",
#   "EUMM Georgia",
#   "EUMPM Niger",
#   "EUNAVFOR MED",
#   "EUPM Moldova",
#   "EUPOL COPPS",
#   "EUTM Mozambique",
#   "EUTM RCA",
#   "EUTM Somalia",
#   "EUTM Mali"
# )

# Create a data frame with one column
# eeas <- data.frame(abbreviation = eu_missions)
# eeas %<>% 
#   mutate(ongoing = TRUE)
# eeas %<>% 
#   country = countrycode::countrycode(mission, "country.name", "country.name")) 
# eeas %<>% 
#   mutate(country = ifelse(grepl("RCA", mission), "Central African Republic",
#                           ifelse(grepl("Moldova",mission), "Moldova",
#                                  ifelse(grepl("Rafah", mission), "Israel", 
#                                         ifelse(grepl("Althea", mission), "Bosnia and Herzogovina",
#                                                ifelse(grepl("IRINI", mission), "Libya", country))))))
# 
# test <- eu_wiki %>% 
#   full_join(eeas, by = "abbreviation") %>% 
#   select(country = country, abbreviation, start, end, ongoing, everything())

eutm_mozambique <- data.frame(country = "Mozambique",
                              abbreviation = "EUTM Mozambique",
                              type = "EUTM",      
                              start = as.Date("2021-07-15"),     
                              end = "")

euracc_chad <- data.frame(country = "Chad",
                              abbreviation = "EU RACC Chad",
                              type = "EU RACC",      
                              start = as.Date("2021-06-01"),     
                              end = "")

euracc_mali <- data.frame(country = "Mali",
                             abbreviation = "EU RACC Mali",
                             type = "EU RACC",      
                             start = as.Date("2021-06-01"),     
                             end = "")

euracc_mauritania <- data.frame(country = "Mauritania",
                             abbreviation = "EU RACC Mauritania",
                             type = "EU RACC",      
                             start = as.Date("2021-06-01"),     
                             end = "")

euracc_niger <- data.frame(country = "Niger",
                             abbreviation = "EU RACC Niger",
                             type = "EU RACC",      
                             start = as.Date("2021-06-01"),     
                             end = "")

euracc_burkina <- data.frame(country = "Burkina Faso",
                             abbreviation = "EU RACC Burkina Faso",
                             type = "EU RACC",      
                             start = as.Date("2021-06-01"),     
                             end = "")

eu_wiki %>%
  select(country, abbreviation, type, start, end) -> eu_mini

eu_mini <- rbind(eu_mini,
                 eutm_mozambique,
                 euracc_burkina,
                 euracc_chad,
                 euracc_mali,
                 euracc_mauritania,
                 euracc_niger)

View(eu_mini)
#"country"      "abbreviation" "type"         "start"        "end"  

rm(chad_row,
   moldova_row,
   eutm_mozambique,
   euracc_burkina,
   euracc_chad,
   euracc_mali,
   euracc_mauritania,
   euracc_niger)

eu_mini %<>% 
  mutate(start_year = year(start),
         end_year = year(end))

eu_mini %<>% 
  mutate(end_year = ifelse(abbreviation == "EUSEC RD Congo", 2006, end_year))

eu_mini %<>% 
  mutate(end_year = ifelse(abbreviation == "PAMECA", 2021, end_year))

eu_mini %<>% 
  mutate(end_year = ifelse(is.na(end_year), 2023, end_year))

eu_mini %<>% 
  select(!c(start, end))

eu_mini %>% 
  filter(start_year > 1999) %>% 
  pivot_longer(!c(type, country, abbreviation), names_to = "event", values_to = "year") %>%
  mutate(year = as.Date(as.character(year), format = "%Y")) %>%
  mutate(year = ymd(year)) %>% 
  complete(abbreviation, year = seq.Date(min(year), max(year), by = "year")) %>%
  mutate(csdp = ifelse(event == "start_year", 1,
                       ifelse(event == "end_year", 1, 0))) %>% 
  group_by(abbreviation) %>% 
  fill(country, .direction = "updown") %>% 
  ungroup()-> eu_long

# Find the indices of "start_year" and "end_year" rows
start_indices <- which(eu_long$event == "start_year")
end_indices <- which(eu_long$event == "end_year")

# Loop through the pairs of start and end indices
for (i in 1:length(start_indices)) {
  start_index <- start_indices[i]
  end_index <- end_indices[i]
  
  # Set "event" to "duration" for rows between start and end
  eu_long$event[(start_index + 1):(end_index - 1)] <- "duration"
}

eu_long %<>% 
  mutate(year = year(year))

eu_long %<>% 
  mutate(iso = countrycode::countrycode(country, "country.name", "iso3c")) %>% 
  mutate(iso = ifelse(country == "Kosovo", "XKX", iso))

# Caused by warning: ! Some values were not matched unambiguously: Kosovo

eu_long %<>% 
  mutate(csdp = ifelse(event == "duration", 1, 
                       ifelse(event == "start_year", 1,
                              ifelse(event == "end_year", 1, 0)))) %>% 
  mutate(csdp = ifelse(is.na(csdp), 0, 1))

eu_long %>% distinct(abbreviation) %>% pull(abbreviation) -> abbrev_vector

patterns <- c(
  "AMIS EU Supporting Action",
  "AMM",
  "EU NAVFOR Somalia",
  "EU RACC Burkina Faso",
  "EU RACC Chad",
  "EU RACC Mali",
  "EU RACC Mauritania",
  "EU RACC Niger",
  "EUAM Iraq",
  "EUAM Ukraine",
  "EUAVSEC South Sudan",
  "EUBAM Libya",
  "EUBAM Moldova",
  "EUBAM Rafah",
  "EUBAM Ukraine",
  "EUCAP Sahel Mali",
  "EUCAP Sahel Niger",
  "EUCAP Somalia",
  "EUFOR Artemis",
  "EUFOR BiH",
  "EUFOR Concordia",
  "EUFOR RCA",
  "EUFOR RD Congo",
  "EUFOR Tchad",
  "EUJUST Georgia",
  "EUJUST LEX Iraq",
  "EULEX Kosovo",
  "EUMA",
  "EUMAM RCA",
  "EUMAM Ukraine",
  "EUMCAP",
  "EUMM Georgia",
  "EUMPM Niger",
  "EUNAVFOR Med",
  "EUNAVFOR Med Irini",
  "EUPAT",
  "EUPAT Armenia",
  "EUPM BiH",
  "EUPM Moldova",
  "EUPOL Afghanistan",
  "EUPOL COPPS",
  "EUPOL FYROM",
  "EUPOL Kinshasa",
  "EUPOL RD Congo",
  "EUSEC RD Congo",
  "EUSSR Guinea-Bissau",
  "EUTM Mali",
  "EUTM Mozambique",
  "EUTM RCA",
  "EUTM Somalia",
  "PAMECA"
)

# Create an empty data frame to store the results with the same number of rows as eu_long

results_df <- data.frame(matrix(0, ncol = length(patterns), nrow = nrow(eu_long)))

# Iterate over patterns and update columns
for (i in 1:length(patterns)) {
  var_name <- paste0(patterns[i], "_binary")
  results_df[, i] <- as.integer(str_detect(eu_long$abbreviation, patterns[i]))
}

# Rename the columns to match the patterns
colnames(results_df) <- paste0(patterns, "_binary")

results <- cbind(eu_longer, results_df)

results <- results[,-9]

results  %>% 
  group_by(country, year) %>% 
  summarise(number_csdp = sum(csdp)) %>% 
  ungroup() %>% 
  distinct(country, year, .keep_all = TRUE) -> results_csdp

results_csdp %<>% 
  mutate(binary_csdp = ifelse(number_csdp > 0, 1, 0)) 

library(plm)

results_csdp %>% 
  pdata.frame(., index = c("country", "year")) -> csdp_pdf

csdp_pdf %>% 
  is.pbalanced()


  
 
grav <- read.csv("C:/Users/Paula/Desktop/Gravity_V202211.csv")  


grav %>% 
  filter(year > 1999) %>% 
  select(year, iso3_o, iso3_d, distw_arithmetic,diplo_disagreement, 
         comlang_ethno, col45, comrelig, col_dep_ever, heg_o, empire,
         distcap, 
         pop_o:gdpcap_ppp_d,tradeflow_comtrade_o:tradeflow_imf_d) -> grav_2000

write.csv(grav_2000, "grav_2000.csv", row.names = FALSE)

write.csv(csdp_pdf, "csdp_pdf.csv", row.names = FALSE)

results_csdp %<>% 
  mutate(iso3_d = countrycode::countrycode(country, "country.name", "iso3c"))

results_csdp %<>% 
  mutate(iso3_d = ifelse(country == "Kosovo", "XKX", iso3_d))

results_csdp %<>% 
  mutate(year = as.integer(year) + 2001) 

class(results_csdp$iso3_d)
class(grav_mini)

grav_mini %>% 
  filter(iso3_o %in% eu_iso_vector) %>% 
  right_join(results_csdp, by = c("iso3_d", "year")) -> grav_csdp

grav_mini %>% 
  filter(iso3_o == "BEL") %>% 
  inner_join(results_csdp, by = c("iso3_d", "year")) %>%  V


class(grav_mini$year)
class(results_csdp$year)



# write.csv(grav_2000, "grav_2000.csv", row.names = FALSE)
# 
# write.csv(csdp_pdf, "csdp_pdf.csv", row.names = FALSE)

library(countrycode)

grav <- read.csv("grav_2000.csv")
csdp <- read.csv("csdp_pdf.csv")

csdp %<>% 
  mutate(iso3_d = countrycode(country, "country.name", "iso3c"), 
         iso3_d = ifelse(country == "Kosovo", "XKX", iso3_d))


grav %>% 
  filter(iso3_o == "BEL") -> grav_bel

grav %>% 
  filter(iso3_o == "FRA") -> grav_france

csdp %>% 
  inner_join(grav_bel, by = c("iso3_d", "year")) -> csdp_belgium

csdp %>% 
  right_join(grav_bel, by = c("iso3_d", "year")) -> all_belgium

acled %<>% 
  mutate(iso3_d = countrycode(country, "country.name", "iso3c"), 
         iso3_d = ifelse(country == "Kosovo", "XKX", iso3_d))

all_belgium %<>% 
  mutate(country = countrycode(iso3_d, "iso3c", "country.name")) 

all_belgium %<>% 
  mutate(number_csdp = ifelse(is.na(number_csdp), 0, number_csdp)) %>% 
  mutate(binary_csdp = ifelse(is.na(number_csdp), 0, binary_csdp))

all_belgium %>%    
  left_join(acled, by = c("iso3_d", "year"), suffix = c("_grav", "_acled")) -> all_belgium_acled


all_belgium_acled %>% 
  pdata.frame(., index = c("country_grav", "year")) -> bel_acled_plm

bel_acled_plm %>% plm::is.pbalanced()

bel_acled_plm %>% plm::pdim()

bel_acled_plm %>% names
install.packages("pglm")
library(pglm)

formula <- binary_csdp ~ as.factor(col45) + distcap

all_belgium_acled %>% 
  group_by(iso3_d) %>% 
  summarise(total_csdp_years = sum(number_csdp, na.rm = TRUE),
            col45 = first(col45),
            distcap = first(distcap),
            avg_battles = mean(battles, na.rm = TRUE),
            avg_violence_civ = mean(violence_against_civilians, na.rm = TRUE)) -> csdp_sum


csdp_sum %>% View

migrant_sum <- migrant %>% 
  group_by(country) %>% 
  summarise(avg_asylum = mean(asylum_req, na.rm = TRUE)) %>% 
  ungroup()


migrant_sum %<>% 
  mutate(iso = countrycode(country, "country.name", "iso3c")) 

migrant_sum %<>% 
  mutate(iso = ifelse(country == "Kosovo (under United Nations Security Council Resolution 1244/99)", "XKX", iso))


# logit_model <- pglm(formula, data = bel_acled_plm, family = binomial, model = "within", 
#                     effect = "individual")

logit_model <- pglm(formula, data = bel_acled_plm, family = binomial,
                    model= "random")

summary(logit_model)

migrant %<>%
  select(country = TIME, everything()) %>%  
  select(-starts_with("...")) %>% 
  slice(-1) 

migrant %<>% 
  janitor::clean_names() %>% 
  mutate_all(~ ifelse(grepl(":", .), NA, .)) %>% 
  pivot_longer(!country,
               names_to = "year", 
               values_to = "asylum_req") %>% 
  mutate(year = parse_number(year)) 

write.csv(migrant, "migrant.csv", row.names = FALSE)


csdp_sum %<>% 
  mutate(iso = countrycode(country_acled, "country.name", "iso3c")) 

migrant_sum %<>% 
  mutate(iso = ifelse(country == "Kosovo (under United Nations Security Council Resolution 1244/99)", "XKX", iso))


csdp_sum %>% 
  select(iso = iso3_d, everything()) %>% 
  left_join(migrant_sum, by = "iso") %>% 
  mutate(binary_csdp = ifelse(total_csdp_years >=1, 1, 0)) -> csdp_migrant_sum


library(glm)

former_french_colonies <- data.frame(colony = c(
  "Algeria", "Morocco", "Tunisia", "Libya", "Mauritania", "Mali", "Niger", "Chad",
  "Burkina Faso", "Ivory Coast (Côte d'Ivoire)", "Benin", "Togo", "Senegal", "Guinea",
  "Guinea-Bissau", "Sierra Leone", "Liberia", "Mali", "Central African Republic", "Gabon",
  "Congo (Brazzaville)", "Democratic Republic of the Congo (formerly Zaire)", "Cameroon",
  "Chad", "Djibouti", "Comoros", "Madagascar", "Seychelles", "Vietnam", "Cambodia",
  "Laos", "Syria", "Lebanon", "Haiti", "French Guiana (overseas department)",
  "Martinique (overseas department)", "Guadeloupe (overseas department)", "Mayotte (overseas department)",
  "Reunion (overseas department)", "French Polynesia (overseas collectivity)",
  "New Caledonia (special collectivity)"))

former_french_colonies %<>% 
  mutate(iso = countrycode(colony, "country.name", "iso3c"))

former_french_colonies %<>% 
  mutate(french_colony = 1)

csdp_migrant_sum %<>% 
  left_join(former_french_colonies, by = "iso")

csdp_migrant_sum %<>% 
  mutate(french_colony = ifelse(french_colony == 1, 1, 0))


csdp_migrant_sum %<>% 
  select(country, iso, total_csdp_years, binary_csdp, french_colony, distcap, avg_battles, avg_violence_civ, avg_asylum, everything())

csdp_migrant_sum %<>% 
  mutate(french_colony = replace_na(french_colony, 0))




csdp_migrant_sum %>% 
  distinct(iso, .keep_all = TRUE) -> my_csdp

glm(binary_csdp ~ avg_battles + avg_violence_civ + as.factor(french_colony) + distcap + avg_asylum, 
    data = my_csdp, family = "binomial") %>% summary()



install.packages("MASS")
library(MASS)
library(tidyverse)
library(countrycode)
library(magrittr)


csdp_migrant_sum %<>% 
  mutate(country = countrycode(iso, "iso3c", "country.name")) 

csdp_migrant_sum %<>% 
  filter(!is.na())

skimr::skim(csdp_migrant_sum)

csdp_migrant_sum %<>% 
  mutate(avg_battles_nona = ifelse(is.na(avg_battles), 0, avg_battles),
         avg_violence_civ_nona = ifelse(is.na(avg_violence_civ), 0, avg_violence_civ),
         avg_aylum_nona = ifelse(is.na(avg_asylum), 0, avg_asylum))


glm.nb(total_csdp_years ~ avg_battles_nona + as.factor(french_colony) +  distcap + avg_aylum_nona, 
       data = csdp_migrant_sum) %>%  summary()


csdp_migrant_sum %>% 
  with(cor(avg_battles_nona, avg_violence_civ_nona))


lm(total_csdp_years ~ avg_battles_nona + as.factor(french_colony) +  distcap + avg_aylum_nona, 
   data = csdp_migrant_sum) %>% summary()


coldat %<>%  
  mutate(iso = countrycode(country, "country.name", "iso3c")) %>% 
  mutate(iso = ifelse(country == "Kosovo", "XKX", iso))


coldat %<>%
  select(iso, col.belgium:col.spain) %>% 
  rowwise() %>%
  mutate(col_binary = as.numeric(any(c_across(starts_with("col.")) == 1))) %>%  
  janitor::clean_names() 

coldat %<>%
  mutate(total_col = sum(c_across(starts_with("col_")) == 1)) 


csdp_migrant_sum %<>% 
  left_join(coldat, by = "iso")

lm(total_csdp_years ~ avg_violence_civ_nona + as.factor(col_binary) +  distcap + avg_aylum_nona, 
   data = csdp_migrant_sum) %>% summary()
