suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(httr))

dir.create("output", showWarning = FALSE)
dir.create("output/admin0", showWarning = FALSE)

httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
  httr::authenticate(":", ":", type = "ntlm"),
  httr::write_disk(tf <- tempfile(fileext = ".csv")))

data <- suppressMessages(readr::read_csv(tf, na = "")) %>%
  dplyr::mutate(dateRep = as.Date(dateRep, format = "%d/%m/%Y")) %>%
  dplyr::select(-day, -month, -year, -countryterritoryCode,
    -countriesAndTerritories, -popData2019, -continentExp) %>%
  dplyr::rename(date = "dateRep", admin0_code = "geoId") %>%
  dplyr::mutate(admin0_code = ifelse(admin0_code == "JPG11668", "International Conveyance", admin0_code)) %>%
  dplyr::select(admin0_code, date, cases, deaths)

message("Most recent date: ", max(data$date))

# fix the codes they use for Greece and UK
data$admin0_code[data$admin0_code == "EL"] <- "GR"
data$admin0_code[data$admin0_code == "UK"] <- "GB"

# if any coutry has all cases zero, get rid of it
ccd <- data %>%
  dplyr::group_by(admin0_code) %>%
  dplyr::summarise(n = sum(cases)) %>%
  dplyr::filter(n == 0) %>%
  dplyr::pull(admin0_code)

if (length(ccd) > 0)
  data <- dplyr::filter(data, !admin0_code %in% ccd)

admin0 <- data %>%
  dplyr::group_by(admin0_code) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(cases = cumsum(cases), deaths = cumsum(deaths)) %>%
  dplyr::filter(date >= min(date[cases > 0])) %>%
  dplyr::arrange(admin0_code, date)

# fill in any date that is missing
mdate <- max(admin0$date)
admin0 <- admin0 %>%
  group_by(admin0_code) %>%
  tidyr::complete(date = seq.Date(max(date), mdate, by = "day")) %>%
  tidyr::fill(cases, deaths) %>%
  arrange(admin0_code, date)

# admin0 %>%
#   group_by(admin0_code) %>%
#   summarise(m = max(date)) %>%
#   filter(m < mdate)

continents <- admin0 %>%
  dplyr::left_join(geoutils::admin0, by = "admin0_code") %>%
  dplyr::group_by(continent_code, date) %>%
  dplyr::filter(!is.na(continent_code)) %>%
  dplyr::summarise(cases = sum(cases), deaths = sum(deaths))

who_regions <- admin0 %>%
  dplyr::left_join(geoutils::admin0, by = "admin0_code") %>%
  dplyr::group_by(who_region_code, date) %>%
  dplyr::summarise(cases = sum(cases), deaths = sum(deaths))

global <- admin0 %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(cases = sum(cases), deaths = sum(deaths))

readr::write_csv(admin0, "output/admin0/all.csv")
readr::write_csv(continents, "output/continents.csv")
readr::write_csv(who_regions, "output/who_regions.csv")
readr::write_csv(global, "output/global.csv")

# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
