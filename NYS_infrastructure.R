library(tidyverse)
library(openxlsx)
library(janitor)


states <- read.xlsx("https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2023/10/23/table2_stateprogram.xlsx",
                    sheet = 2)

program_names <- read.xlsx("https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2023/10/23/table2_stateprogram.xlsx",
                           sheet = 1, startRow = 2) %>% 
  clean_names()


county_program_names <- read.xlsx("https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2023/10/23/table1_countyprogram.xlsx",
                                  sheet = 1, startRow = 2) %>% 
  clean_names()

counties <- read.xlsx("https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2023/10/23/table1_countyprogram.xlsx",
                      sheet = 2)



ny_counties <- counties %>% 
  filter(state == "NY") %>% 
  pivot_longer(cols = 12:52, names_to = "program", values_to = "spending") %>% 
  mutate(hud = if_else(program %in% c("hcv",
                                      "s8_project",
                                      "public_hsg_cap",
                                      "public_hsg",
                                      "cdbg_entitlement",
                                      "coc",
                                      "home",
                                      "elderly"
  ),
  T, F),
  spending_per_thou = spending/total_county_pop*1000) %>% 
  left_join(county_program_names, by = c("program" = "variable_name"))

county_sum <- ny_counties %>% 
  group_by(county) %>% 
  summarise(total_spending = sum(spending),
            nonhud_spending = sum(spending[hud == F]),
            spending_thousand = sum(spending)/first(total_county_pop)*1000)


NY <- states %>% 
  filter(state == "NY") %>% 
  pivot_longer(cols = 6:71, names_to = "program", values_to = "spending") %>% 
  mutate(hud = if_else(program %in% c("hcv",
                                      "s8_project",
                                      "public_hsg_cap",
                                      "public_hsg",
                                      "cdbg_entitlement",
                                      "coc",
                                      "home",
                                      "elderly"
                                      ),
                       T, F)) %>% 
  left_join(program_names, by = c("program" = "variable_name"))

NY %>% 
  group_by(hud) %>% 
  summarize(total_spending = sum(spending))


