library(dplyr)
library(readr)

raw = readr::read_csv("2022/2022_NTD_Annual_Data_-_Breakdowns_20231102.csv")

pkeys = c("Mode", "Type of Service", "NTD ID")

clean = raw %>%
  dplyr::filter(
    Mode %in% c("CR", "HR", "LR", "MB"),
    # Don't use != bcs NAs don't pass through.
    !`Vehicle/Passenger Car Revenue Miles Questionable` %in% c("Q"),
    `Vehicle/Passenger Car Revenue Miles` > 0,
    !`Total Mechanical Failures Questionable` %in% c("Q"),
    `Total Mechanical Failures` > 0,
  ) %>%
  dplyr::group_by(Mode) %>%
  dplyr::mutate(
    # Use the "Doing Business As" name.
    Agency = gsub(".*dba: ", "", Agency),
    `Mean Miles Between Breakdowns` = `Vehicle/Passenger Car Revenue Miles` /
      `Total Mechanical Failures`,
    Rank = dplyr::min_rank(dplyr::desc(`Mean Miles Between Breakdowns`))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    Rank,
    dplyr::all_of(pkeys),
    Agency,
    City,
    State,
    `Vehicle/Passenger Car Revenue Miles`,
    `Total Mechanical Failures`,
    `Mean Miles Between Breakdowns`
  ) %>%
  dplyr::arrange(Mode, Rank)

###############
### Biggest ###
###############

biggest = clean %>%
  dplyr::group_by(Mode) %>%
  dplyr::top_n(10, `Vehicle/Passenger Car Revenue Miles`) %>%
  dplyr::mutate(
    `Rank, 10 Biggest` = dplyr::min_rank(
      dplyr::desc(`Mean Miles Between Breakdowns`)
    )
  ) %>%
  dplyr::ungroup()

clean = clean %>%
  dplyr::left_join(
    biggest %>%
      dplyr::select(dplyr::all_of(pkeys), `Rank, 10 Biggest`),
    by = pkeys
  ) %>%
  dplyr::relocate(`Rank, 10 Biggest`, .after = Rank)

rm(biggest)

############
### 2021 ###
############

clean_2021 = readr::read_csv(
  "breakdowns_2021.csv",
  col_types = readr::cols(`NTD ID` = readr::col_character())
)

clean = clean %>%
  dplyr::left_join(
    clean_2021 %>%
      select(
        `NTD ID`, Mode, `Type of Service`,
        `Mean Miles Between Breakdowns (2021)` = `Mean Miles Between Breakdowns`,
        `Rank (2021)` = Rank
      ),
    by = c("NTD ID", "Mode", "Type of Service")
  ) %>%
  dplyr::mutate(`Rank Change` = `Rank (2021)` - Rank)

rm(clean_2021)

##############
### Checks ###
##############

clean %>%
  dplyr::filter(!is.na(`Rank, 10 Biggest`)) %>%
  dplyr::arrange(Mode, `Rank, 10 Biggest`) %>%
  dplyr::select(-Rank) # %>% View()

# These must have had questionable data in 2021 but not 2022.
clean %>%
  dplyr::filter(is.na(`Rank (2021)`)) %>% # View()
  nrow()

##############
### Export ###
##############

readr::write_csv(clean, "breakdowns_2022.csv")
