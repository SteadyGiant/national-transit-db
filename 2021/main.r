library(dplyr)
library(readr)
library(readxl)

pkeys = c("NTD ID", "Mode", "Type of Service")
raw = readxl::read_excel(
  "2021/2021 Breakdowns_static.xlsx", sheet = "Breakdowns"
) %>%
  dplyr::filter(Mode %in% c("CR", "HR", "LR", "MB")) %>%
  dplyr::select(!dplyr::matches("[0-9]")) %>%
  dplyr::relocate(dplyr::all_of(pkeys), dplyr::everything()) %>%
  dplyr::mutate(
    Agency = gsub(".*dba: ", "", Agency),
    `Mean Miles Between Breakdowns` = `Vehicle/Passenger Car Revenue Miles` / `Total Mechanical Failures`
  )

# NOTE: Include waived? PATH was waived.
quality_flags_to_exclude = c("Q")  # "W"
# Put each problem group in an object so I can View() it.
flagged_quality = raw %>%
  dplyr::filter(
    `Total Mechanical Failures Questionable` %in% quality_flags_to_exclude |
      `Vehicle/Passenger Car Revenue Miles Questionable` %in% quality_flags_to_exclude
  )
no_breakdowns = raw %>%
  dplyr::filter(`Total Mechanical Failures` == 0)
no_miles = raw %>%
  dplyr::filter(`Vehicle/Passenger Car Revenue Miles` == 0)

# Remove problem records.
cln = raw %>%
  dplyr::anti_join(flagged_quality, by = pkeys) %>%
  dplyr::anti_join(no_breakdowns, by = pkeys) %>%
  dplyr::anti_join(no_miles, by = pkeys) %>%
  dplyr::group_by(Mode) %>%
  dplyr::mutate(Rank = dplyr::min_rank(dplyr::desc(`Mean Miles Between Breakdowns`))) %>%
  dplyr::ungroup()

biggest = cln %>%
  dplyr::group_by(Mode) %>%
  dplyr::top_n(10, `Vehicle/Passenger Car Revenue Miles`) %>%
  dplyr::mutate(`Rank, 10 Biggest` = dplyr::min_rank(dplyr::desc(`Mean Miles Between Breakdowns`))) %>%
  dplyr::ungroup()

cln = cln %>%
  dplyr::left_join(
    dplyr::select(biggest, `NTD ID`, Mode, `Type of Service`, `Rank, 10 Biggest`),
    by = c("NTD ID", "Mode", "Type of Service")
  ) %>%
  dplyr::arrange(Mode, Rank)

readr::write_csv(cln, "breakdowns_2021.csv")
