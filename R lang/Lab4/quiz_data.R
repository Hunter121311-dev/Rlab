library(readr)
library(dplyr)

spec <- read_csv("daily_SPEC_2014.csv.bz2")

spec_wi_br <- spec[spec$`State Name` == "Wisconsin" & spec$`Parameter Name` == "Bromine PM2.5 LC",]

colnames(spec_wi_br)

mean_value <- mean(spec_wi_br$"Arithmetic Mean")

mean_value

means <- spec %>%
  group_by(`Parameter Name`) %>%
  summarise(mean_value = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(mean_value)) %>%
  print(n = 100)

spec %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  group_by(`State Code`, `County Code`, `Site Num`) %>%
  summarise(mean_value = mean(`Arithmetic Mean`, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_value)) %>%
  print(n=10)

abs(
  diff(
    spec %>%
      filter(
        `Parameter Name` == "EC PM2.5 LC TOR",
        `State Name` %in% c("California", "Arizona")
      ) %>%
      group_by(`State Name`) %>%
      summarise(mean_value = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
      pull(mean_value)
  )
)

spec %>%
  filter(`Parameter Name` == "OC PM2.5 LC TOR", Longitude < -100) %>%
  summarise(median(`Arithmetic Mean`, na.rm = TRUE)) %>%
  pull()

library(readxl)
library(dplyr)

aqs <- read_excel("aqs_sites.xlsx")
names(aqs)

count_sites <- aqs %>%
  filter(
    `Land Use` == "RESIDENTIAL",
    `Location Setting` == "SUBURBAN"
  ) %>%
  nrow()

count_sites

aqs %>%
  filter(`Land Use` == "RESIDENTIAL", `Location Setting` == "SUBURBAN") %>%
  nrow()


result <- spec %>%
  mutate(
    `State Code` = as.character(`State Code`),
    `County Code` = as.character(`County Code`),
    `Site Num` = as.character(`Site Num`)
  ) %>%
  filter(`Parameter Name` == "EC PM2.5 LC TOR") %>%
  inner_join(
    aqs %>%
      mutate(
        `State Code` = as.character(`State Code`),
        `County Code` = as.character(`County Code`),
        `Site Number` = as.character(`Site Number`)
      ),
    by = c("State Code", "County Code", "Site Num" = "Site Number"),
    suffix = c(".spec", ".aqs")
  ) %>%
  filter(
    toupper(`Land Use`) == "RESIDENTIAL",
    toupper(`Location Setting`) == "SUBURBAN",
    Longitude.aqs >= -100
  ) %>%
  summarise(median_value = median(`Arithmetic Mean`, na.rm = TRUE)) %>%
  pull(median_value)

result

spec %>%
  filter(`Parameter Name` == "EC PM2.5 LC TOR") %>%
  inner_join(aqs, by = c("State Code", "County Code", "Site Num" = "Site Number")) %>%
  filter(
    grepl("RESIDENTIAL", `Land Use`, ignore.case = TRUE),
    grepl("SUBURBAN", `Location Setting`, ignore.case = TRUE),
    Longitude >= -100
  ) %>%
  summarise(median(`Arithmetic Mean`, na.rm = TRUE)) %>%
  pull()


library(dplyr)

result <- spec %>%
  mutate(
    `State Code` = as.integer(`State Code`),
    `County Code` = as.integer(`County Code`),
    `Site Num` = as.integer(`Site Num`)
  ) %>%
  filter(`Parameter Name` == "EC PM2.5 LC TOR") %>%
  inner_join(
    aqs %>%
      mutate(
        `State Code` = as.integer(`State Code`),
        `County Code` = as.integer(`County Code`),
        `Site Number` = as.integer(`Site Number`)
      ),
    by = c(
      "State Code",
      "County Code",
      "Site Num" = "Site Number"
    ),
    suffix = c(".spec", ".aqs")
  ) %>%
  filter(
    toupper(`Land Use`) == "RESIDENTIAL",
    toupper(`Location Setting`) == "SUBURBAN",
    Longitude.aqs >= -100,
    !is.na(`Arithmetic Mean`)
  ) %>%
  summarise(median_value = median(`Arithmetic Mean`)) %>%
  pull(median_value)

result


library(lubridate)

spec <- spec %>%
  mutate(`Date Local` = ymd(`Date Local`))

result <- spec %>%
  mutate(
    `State Code` = as.integer(`State Code`),
    `County Code` = as.integer(`County Code`),
    `Site Num` = as.integer(`Site Num`)
  ) %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  inner_join(
    aqs %>%
      mutate(
        `State Code` = as.integer(`State Code`),
        `County Code` = as.integer(`County Code`),
        `Site Number` = as.integer(`Site Number`)
      ),
    by = c("State Code", "County Code", "Site Num" = "Site Number"),
    suffix = c(".spec", ".aqs")
  ) %>%
  filter(
    toupper(`Land Use`) == "COMMERCIAL",
    !is.na(`Arithmetic Mean`)
  ) %>%
  mutate(month = month(`Date Local`)) %>%
  group_by(month) %>%
  summarise(mean_value = mean(`Arithmetic Mean`, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_value)) %>%
  slice(1)

result

library(tidyr)

days_over_10 <- spec %>%
  filter(
    `State Code` == "06",
    `County Code` == "065",
    `Site Num` == "8001",
    `Parameter Name` %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")
  ) %>%
  group_by(`Date Local`, `Parameter Name`) %>%
  summarise(mean_value = mean(`Arithmetic Mean`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = `Parameter Name`,
    values_from = mean_value
  ) %>%
  mutate(total = `Sulfate PM2.5 LC` + `Total Nitrate PM2.5 LC`) %>%
  filter(total > 10) %>%
  nrow()

days_over_10