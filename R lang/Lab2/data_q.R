dane <- read.csv("Lab2/q1_data.csv")
head(dane, 2)

tail(dane, 2)

dane[47, ]

sum(is.na(dane$Ozone))

dane_ozone_no_na <- dane[!is.na(dane$Ozone), ]
mean(dane_ozone_no_na$Ozone)

dane_ozone_above_31_and_temp_above_90 <- dane[dane$Ozone > 31 & dane$Temp > 90, ]
mean(dane_ozone_above_31_and_temp_above_90$Solar.R[
  !is.na(
    dane_ozone_above_31_and_temp_above_90$Solar.R
    )
  ]
  )

mean(dane$Temp[dane$Month == 6])

max(dane$Ozone[dane$Month == 5 & !is.na(dane$Ozone)])
