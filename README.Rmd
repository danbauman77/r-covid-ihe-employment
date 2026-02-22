
```{r}
# ==============================================================================
# Covid-19 Era Higher Education Employment Analysis — IPUMS-CPS
# ==============================================================================


# SETUP

library(ipumsr)
library(tidyverse)
library(scales)
library(openxlsx)

EXPORT_DIR <- "~/GitHub/r-covid-ihe-employment/exports/"
dir.create(EXPORT_DIR, showWarnings = FALSE)


# ALGEBRA

ddi  <- read_ipums_ddi("~/GitHub/r-covid-ihe-employment/download/cps_00005/cps_00005.xml")
data <- read_ipums_micro(ddi)


# CLASSIFY

data <- data %>%
  mutate(
    across(c(YEAR, MONTH, EMPSTAT, IND1990, OCC1990, AHRSWORKT, AGE, RACE, HISPAN, SEX), as.numeric),
    
    
    date = as.Date(sprintf("%04d-%02d-01", YEAR, MONTH)),
    
    
    
    work_status = case_when(
      AHRSWORKT >= 35 & AHRSWORKT < 997 ~ "Full-Time",
      between(AHRSWORKT, 1, 34)         ~ "Part-Time"
    ),
    
    sex_label = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female"
    ),
    
    
    race_label = case_when(
      RACE == 100              ~ "White",
      RACE == 200              ~ "Black",
      RACE == 651              ~ "Asian",
      RACE == 652              ~ "Hawaiian/Pacific Islander",
      between(RACE, 801, 830)  ~ "Two or More Races",
      RACE %in% c(700, 999)    ~ "Unknown"
    ),
    
    
    poc_label = case_when(
      RACE == 100                                                  ~ "White Workers",
      RACE %in% c(200, 300, 651, 652) | between(RACE, 801, 830)    ~ "Workers of Color",
      RACE %in% c(700, 999)                                        ~ "Unknown"
    ),
    
    
    hispanic_label = case_when(
      HISPAN %in% c(0, 901, 902) ~ "Not Hispanic",
      between(HISPAN, 100, 612)  ~ "Hispanic"
    ),
    
    
    age_group = case_when(
      between(AGE, 16, 24) ~ "16-24",
      between(AGE, 25, 29) ~ "25-29",
      between(AGE, 30, 34) ~ "30-34",
      between(AGE, 35, 44) ~ "35-44",
      between(AGE, 45, 54) ~ "45-54",
      between(AGE, 55, 64) ~ "55-64",
      between(AGE, 65, 74) ~ "65-74",
      between(AGE, 75, 99) ~ "75+"
    ),
    
    
    
    # Occupations of interest
    occ_s_of_interest = case_when(
      between(OCC1990, 113, 154) ~ "Instructors (Postsecondary)",
      between(OCC1990, 303, 391) ~ "Administrative Support",
      between(OCC1990, 448, 455) ~ "Cleaning & Building Service",
      between(OCC1990, 434, 444) ~ "Food Preparation & Service"
    ),
    
    
    
    occ_group_l1 = case_when(
      between(OCC1990,   3, 200) ~ "Managerial & Professional Specialty",
      between(OCC1990, 203, 391) ~ "Technical, Sales & Administrative Support",
      between(OCC1990, 405, 469) ~ "Service Occupations",
      between(OCC1990, 473, 498) ~ "Farming, Forestry & Fishing",
      between(OCC1990, 503, 699) ~ "Precision Production, Craft & Repair",
      between(OCC1990, 703, 890) ~ "Operators, Fabricators & Laborers",
      OCC1990 == 905             ~ "Military",
      OCC1990 == 999             ~ "NIU"
    ),
    
    
    
    occ_group_l2 = case_when(
      between(OCC1990,   3,  22) ~ "Executive, Administrative & Managerial",
      between(OCC1990,  23,  37) ~ "Management Related",
      between(OCC1990,  43, 200) ~ "Professional Specialty",
      between(OCC1990, 203, 235) ~ "Technicians & Related Support",
      between(OCC1990, 243, 290) ~ "Sales Occupations",
      between(OCC1990, 303, 391) ~ "Administrative Support, Including Clerical",
      between(OCC1990, 405, 408) ~ "Private Household",
      between(OCC1990, 415, 427) ~ "Protective Service",
      between(OCC1990, 434, 469) ~ "Service, Except Protective & Household",
      between(OCC1990, 473, 476) ~ "Farm Operators & Managers",
      between(OCC1990, 479, 498) ~ "Other Agricultural & Related",
      OCC1990 == 503             ~ "Supervisors of Mechanics & Repairers",
      between(OCC1990, 505, 699) ~ "Mechanics & Repairers, Except Supervisors",
      between(OCC1990, 703, 799) ~ "Machine Operators, Assemblers & Inspectors",
      between(OCC1990, 803, 890) ~ "Transportation & Material Moving"
    ),
    
    
    
    
    occ_group_l3 = case_when(
      OCC1990 == 43              ~ "Architects",
      between(OCC1990,  44,  59) ~ "Engineers",
      between(OCC1990,  64,  68) ~ "Mathematical & Computer Scientists",
      between(OCC1990,  69,  83) ~ "Natural Scientists",
      between(OCC1990,  84,  89) ~ "Health Diagnosing Occupations",
      between(OCC1990,  95,  97) ~ "Health Assessment & Treating Occupations",
      between(OCC1990,  98, 106) ~ "Therapists",
      between(OCC1990, 113, 154) ~ "Teachers, Postsecondary",
      between(OCC1990, 155, 163) ~ "Teachers, Except Postsecondary",
      between(OCC1990, 164, 165) ~ "Librarians, Archivists & Curators",
      between(OCC1990, 166, 173) ~ "Social Scientists & Urban Planners",
      between(OCC1990, 174, 176) ~ "Social, Recreation & Religious Workers",
      between(OCC1990, 178, 179) ~ "Lawyers & Judges",
      between(OCC1990, 183, 200) ~ "Writers, Artists, Entertainers & Athletes",
      between(OCC1990, 203, 208) ~ "Health Technologists & Technicians",
      between(OCC1990, 213, 225) ~ "Technologists & Technicians, Except Health",
      between(OCC1990, 226, 235) ~ "Technicians, Except Health, Engineering & Science",
      between(OCC1990, 243, 256) ~ "Sales Representatives, Finance & Business Services",
      between(OCC1990, 258, 277) ~ "Sales Representatives, Commodities",
      between(OCC1990, 283, 290) ~ "Sales Related Occupations",
      OCC1990 == 303             ~ "Office Supervisors",
      OCC1990 == 308             ~ "Computer & Peripheral Equipment Operators",
      between(OCC1990, 313, 315) ~ "Secretaries, Stenographers & Typists",
      between(OCC1990, 316, 323) ~ "Information Clerks",
      between(OCC1990, 326, 336) ~ "Records Processing, Except Financial",
      between(OCC1990, 337, 344) ~ "Financial Records Processing",
      between(OCC1990, 345, 347) ~ "Duplicating, Mail & Office Machine Operators",
      between(OCC1990, 348, 349) ~ "Communications Equipment Operators",
      between(OCC1990, 354, 357) ~ "Mail & Message Distributing Occupations",
      between(OCC1990, 359, 373) ~ "Material Recording, Scheduling & Distributing",
      between(OCC1990, 375, 378) ~ "Adjusters & Investigators",
      between(OCC1990, 379, 391) ~ "Miscellaneous Administrative Support",
      between(OCC1990, 405, 408) ~ "Private Household Occupations",
      OCC1990 == 415             ~ "Supervisors of Guards",
      OCC1990 == 417             ~ "Fire Fighting, Prevention & Inspection",
      between(OCC1990, 418, 423) ~ "Police & Detectives",
      between(OCC1990, 425, 427) ~ "Guards",
      between(OCC1990, 434, 444) ~ "Food Preparation & Service Occupations",
      between(OCC1990, 445, 447) ~ "Health Service Occupations",
      between(OCC1990, 448, 455) ~ "Cleaning & Building Service, Except Households",
      between(OCC1990, 456, 469) ~ "Personal Service Occupations",
      between(OCC1990, 473, 476) ~ "Farm Operators & Managers",
      between(OCC1990, 479, 484) ~ "Farm Occupations, Except Managerial",
      between(OCC1990, 485, 489) ~ "Related Agricultural Occupations",
      OCC1990 == 496             ~ "Timber, Logging & Forestry Workers",
      OCC1990 == 498             ~ "Fishers, Hunters & Kindred",
      OCC1990 == 503             ~ "Supervisors of Mechanics & Repairers",
      between(OCC1990, 505, 519) ~ "Vehicle & Mobile Equipment Mechanics & Repairers",
      between(OCC1990, 523, 534) ~ "Electrical & Electronic Equipment Repairers",
      between(OCC1990, 535, 549) ~ "Miscellaneous Mechanics & Repairers",
      OCC1990 == 558             ~ "Supervisors of Construction Work",
      between(OCC1990, 559, 599) ~ "Construction Trades, Except Supervisors",
      between(OCC1990, 614, 617) ~ "Extractive Occupations",
      between(OCC1990, 628, 699) ~ "Precision Production Occupations",
      between(OCC1990, 703, 779) ~ "Machine Operators & Tenders, Except Precision",
      between(OCC1990, 783, 789) ~ "Fabricators, Assemblers & Hand Working",
      between(OCC1990, 796, 799) ~ "Production Inspectors, Testers & Samplers",
      between(OCC1990, 803, 815) ~ "Motor Vehicle Operators",
      between(OCC1990, 823, 825) ~ "Rail Transportation Occupations",
      between(OCC1990, 829, 834) ~ "Water Transportation Occupations",
      between(OCC1990, 835, 890) ~ "Transportation Occupations, Except Motor Vehicles"
    ),
    
    
    
    occ_group_l4 = case_when(
      between(OCC1990, 213, 223) ~ "Engineering & Related Technologists & Technicians",
      between(OCC1990, 224, 225) ~ "Science Technicians",
      OCC1990 == 558             ~ "Supervisors of Construction Work",
      between(OCC1990, 563, 599) ~ "Construction Trades, Except Supervisors",
      between(OCC1990, 634, 653) ~ "Precision Metal Working Occupations",
      between(OCC1990, 657, 659) ~ "Precision Woodworking Occupations",
      between(OCC1990, 666, 674) ~ "Precision Textile, Apparel & Furnishings",
      between(OCC1990, 675, 684) ~ "Precision Workers, Assorted Materials",
      between(OCC1990, 686, 688) ~ "Precision Food Production Occupations",
      OCC1990 == 693             ~ "Adjusters & Calibrators",
      between(OCC1990, 694, 699) ~ "Plant & System Operators",
      between(OCC1990, 703, 717) ~ "Metal & Plastic Working Machine Operators",
      between(OCC1990, 719, 724) ~ "Metal & Plastic Processing Machine Operators",
      between(OCC1990, 726, 733) ~ "Woodworking Machine Operators",
      between(OCC1990, 734, 736) ~ "Printing Machine Operators",
      between(OCC1990, 738, 749) ~ "Textile, Apparel & Furnishings Machine Operators",
      between(OCC1990, 753, 779) ~ "Machine Operators, Assorted Materials",
      between(OCC1990, 823, 825) ~ "Rail Transportation Occupations",
      between(OCC1990, 829, 834) ~ "Water Transportation Occupations",
      between(OCC1990, 844, 859) ~ "Material Moving Equipment Operators",
      between(OCC1990, 865, 874) ~ "Helpers, Construction & Extractive Occupations",
      between(OCC1990, 875, 890) ~ "Freight, Stock & Material Handlers"
    )
  )


# HIGHER ED SUBSET

higher_ed <- data %>%
  filter(
    YEAR    %in% 2017:2021,
    AGE     >= 16,
    EMPSTAT %in% c(10, 12),
    IND1990 %in% c(850, 851)
  )


# SUMMARISE


summarise_by <- function(df, grp_col) {
  df %>%
    group_by(date, group = .data[[grp_col]]) %>%
    summarise(
      n_weighted   = sum(COMPWT),
      n_unweighted = n_distinct(CPSIDP, na.rm = TRUE),
      .groups = "drop"
    )
}




instructors_by_status <- higher_ed %>%
  filter(between(OCC1990, 113, 154)) %>%
  group_by(date, work_status) %>%
  summarise(
    n_weighted   = sum(COMPWT),
    n_unweighted = n_distinct(CPSIDP, na.rm = TRUE),
    .groups = "drop"
  )




by_occ_code <- higher_ed %>%
  group_by(date, OCC1990) %>%
  summarise(
    n_weighted   = sum(COMPWT),
    n_unweighted = n_distinct(CPSIDP, na.rm = TRUE),
    .groups = "drop"
  )




# Employment by occupation groups (l1-l4)
by_occ_l1 <- summarise_by(higher_ed, "occ_group_l1")
by_occ_l2 <- summarise_by(higher_ed, "occ_group_l2")
by_occ_l3 <- summarise_by(higher_ed, "occ_group_l3")
by_occ_l4 <- summarise_by(higher_ed, "occ_group_l4")




# Employment by ...
by_race     <- summarise_by(higher_ed, "race_label")
by_poc      <- summarise_by(higher_ed, "poc_label")
by_hispanic <- summarise_by(higher_ed, "hispanic_label")
by_age      <- summarise_by(higher_ed, "age_group")




#  While workers of color represent just a quarter of higher ed's labor force ... [prep]
feb2021_poc_share <- data %>%
  filter(
    YEAR == 2021, MONTH == 2,
    AGE >= 16,
    EMPSTAT %in% c(10, 12),
    IND1990 %in% c(850, 851),
    poc_label != "Unknown",
    !is.na(poc_label)
  ) %>%
  group_by(poc_label) %>%
  summarise(n_weighted = sum(COMPWT), .groups = "drop") %>%
  mutate(share = n_weighted / sum(n_weighted))




#  ... more than half of the workers who lost jobs have been nonwhite [prep]
race_net_change <- data %>%
  filter(
    YEAR    %in% c(2020, 2021),
    MONTH   == 2,
    AGE     >= 16,
    EMPSTAT %in% c(10, 12),
    IND1990 %in% c(850, 851)
  ) %>%
  group_by(YEAR, race_label) %>%
  summarise(n_weighted = sum(COMPWT), .groups = "drop") %>%
  pivot_wider(names_from = YEAR, values_from = n_weighted, names_prefix = "yr_") %>%
  mutate(net_loss = yr_2020 - yr_2021) %>%
  filter(!is.na(race_label), race_label != "Unknown", net_loss > 0) %>%
  mutate(share_of_losses = net_loss / sum(net_loss))




#  The youngest workers saw big drops [prep]
age_net_change <- data %>%
  filter(
    YEAR    %in% c(2020, 2021),
    MONTH   == 2,
    AGE     >= 16,
    EMPSTAT %in% c(10, 12),
    IND1990 %in% c(850, 851)
  ) %>%
  group_by(YEAR, age_group) %>%
  summarise(n_weighted = sum(COMPWT), .groups = "drop") %>%
  pivot_wider(names_from = YEAR, values_from = n_weighted, names_prefix = "yr_") %>%
  mutate(net_change = yr_2021 - yr_2020)





# EXPORT

wb <- createWorkbook()

add_sheet <- function(wb, df, name) {
  addWorksheet(wb, name)
  writeData(wb, name, df)
}

add_sheet(wb, instructors_by_status, "instructors_by_WorkStatus")
add_sheet(wb, by_occ_code,           "By_OCC1990_Code")
add_sheet(wb, by_occ_l1,             "By_OccGroup_L1")
add_sheet(wb, by_occ_l2,             "By_OccGroup_L2")
add_sheet(wb, by_occ_l3,             "By_OccGroup_L3")
add_sheet(wb, by_occ_l4,             "By_OccGroup_L4")
add_sheet(wb, by_race,               "By_Race_Detailed")
add_sheet(wb, by_poc,                "By_POC_Binary")
add_sheet(wb, by_hispanic,           "By_Hispanic")
add_sheet(wb, by_age,                "By_AgeGroup")
add_sheet(wb, feb2021_poc_share,     "Race_Share_Feb2021")
add_sheet(wb, race_net_change,       "Race_Net_Change_Feb20_Feb21")
add_sheet(wb, age_net_change,        "Age_Net_Change_Feb20_Feb21")

saveWorkbook(wb, file.path(EXPORT_DIR, "higher_ed_employment.xlsx"), overwrite = TRUE)




# CHARTS

theme_report <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "gray35", size = 10),
    axis.title       = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    plot.margin      = margin(10, 30, 10, 10)
  )

POC_label_colors <- c(
  "White Workers"    = "blue3",
  "Workers of Color" = "red3"
)

race_label_colors <- c(
  "White"                        = "blue3",
  "Black"                        = "red3",
  "Asian"                        = "green4",
  "Hawaiian/Pacific Islander"    = "purple3",
  "Two or More Races"            = "orangered",
  "American Indian/Aleut/Eskimo" = "turquoise"
)

occ_s_of_interest_colors <- c(
  "Instructors (Postsecondary)" = "blue3",
  "Administrative Support"      = "yellow4",
  "Cleaning & Building Service" = "green4",
  "Food Preparation & Service"  = "red3"
)



# Chart 1 --  While workers of color represent just a quarter of higher ed's labor force ...

chart_poc_share <- feb2021_poc_share %>%
  mutate(dummy = "Higher Ed Workforce") %>%
  ggplot(aes(x = dummy, y = share, fill = fct_rev(poc_label))) +
  geom_col(width = 0.45) +
  geom_text(
    aes(label = paste0(poc_label, "  ", percent(share, accuracy = 1))),
    position = position_stack(vjust = 0.5),
    size = 4, color = "white", fontface = "bold"
  ) +
  coord_flip() +
  scale_x_discrete(expand = c(0.8, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = POC_label_colors) +
  labs(
    title = "While workers of color represent just a quarter of higher ed's labor force ..."
  ) +
  theme_report +
  theme(axis.text = element_blank(), panel.grid = element_blank(), legend.position = "none")

ggsave(
  file.path(EXPORT_DIR, "chart_poc_workforce_share.png"),
  chart_poc_share, width = 8, height = 2, dpi = 150
)




# Chart 2 --  ... more than half of the workers who lost jobs have been nonwhite

race_order <- race_net_change %>% arrange(desc(net_loss)) %>% pull(race_label)

chart_race_losses <- race_net_change %>%
  mutate(dummy = "Job Losses", race_label = factor(race_label, levels = race_order)) %>%
  ggplot(aes(x = dummy, y = share_of_losses, fill = race_label)) +
  geom_col(width = 0.45) +
  geom_text(
    aes(label = case_when(
      share_of_losses >= 0.15 ~ paste0(race_label, "\n", percent(share_of_losses, accuracy = 1)),
      share_of_losses >= 0.05 ~ percent(share_of_losses, accuracy = 1),
      TRUE                    ~ ""
    )),
    position = position_stack(vjust = 0.5),
    size = 3.5, color = "white", fontface = "bold", lineheight = 1.1
  ) +
  coord_flip() +
  scale_x_discrete(expand = c(0.8, 0.8)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = race_label_colors) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "... more than half of the workers who lost jobs have been nonwhite"
  ) +
  theme_report +
  theme(axis.text = element_blank(), panel.grid = element_blank())

ggsave(
  file.path(EXPORT_DIR, "chart_race_job_losses.png"),
  chart_race_losses, width = 8, height = 3.5, dpi = 150
)




# Chart 3 -- The faculty and lower-paid staffers endured significant job losses

chart_occ_trend <- higher_ed %>%
  filter(!is.na(occ_s_of_interest), YEAR == 2020) %>%
  group_by(date, occ_s_of_interest) %>%
  summarise(n_weighted = sum(COMPWT), .groups = "drop") %>%
  ggplot(aes(x = date, y = n_weighted / 1e6, color = occ_s_of_interest)) +
  geom_line(linewidth = 1.1) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = label_number(suffix = "M", accuracy = 0.1), limits = c(0, NA)) +
  scale_color_manual(values = occ_s_of_interest_colors) +
  labs(
    title = "The faculty and lower-paid staffers endured significant job losses"
  ) +
  theme_report

ggsave(
  file.path(EXPORT_DIR, "chart_occ_trend_2020.png"),
  chart_occ_trend, width = 9, height = 5, dpi = 150
)




# Chart 4 -- Youngest workers saw big drops

chart_age <- age_net_change %>%
  filter(!is.na(age_group)) %>%
  ggplot(aes(x = net_change / 1e3, y = fct_rev(age_group), fill = net_change < 0)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, color = "gray30", linewidth = 0.4) +
  scale_x_continuous(labels = label_number(suffix = "K")) +
  scale_fill_manual(values = c("TRUE" = "red3", "FALSE" = "green4"), guide = "none") +
  labs(
    title    = "The youngest workers saw big drops.",
    subtitle = str_wrap(
      "Net job losses were concentrated among employees who were under 29 or between the ages of 65 and 74 (Not seasonally adjusted).",
      width = 85
    )
  ) +
  theme_report

ggsave(
  file.path(EXPORT_DIR, "chart_age_net_change.png"),
  chart_age, width = 7, height = 4, dpi = 150
)




# Chart 5 -- Postsecondary instructor employment by work status

chart_ftpt <- instructors_by_status %>%
  filter(!is.na(work_status)) %>%
  ggplot(aes(x = date, y = n_weighted / 1e6, color = work_status)) +
  geom_line(linewidth = 1.1) +
  scale_x_date(date_labels = "%b '%y", date_breaks = "6 months",
               expand = expansion(add = c(15, 45))) +
  scale_y_continuous(labels = label_number(suffix = "M", accuracy = 0.1), limits = c(0, NA)) +
  scale_color_manual(values = c("Full-Time" = "blue4", "Part-Time" = "red3")) +
  labs(
    title    = "Postsecondary instructor employment by work status",
    subtitle = "2017–2021"
  ) +
  theme_report




ggsave(
  file.path(EXPORT_DIR, "chart_instructors_ftpt.png"),
  chart_ftpt, width = 9, height = 5, dpi = 150
)

message("Outputs written to '", EXPORT_DIR, "~/GitHub/r-covid-ihe-employment/exports/'.")
```

