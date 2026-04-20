# ----------------------------
# 1. Wczytanie danych
# ----------------------------

txt_file <- "household_power_consumption.txt"

# Wczytanie danych z archiwum ZIP
power <- read.table(
  txt_file,
  header = TRUE,
  sep = ";",
  na.strings = "?",
  stringsAsFactors = FALSE
)

# ----------------------------
# 2. Ograniczenie danych do 1 i 2 lutego 2007
# ----------------------------

power$Date <- as.Date(power$Date, format = "%d/%m/%Y")

power_sub <- subset(
  power,
  Date == as.Date("2007-02-01") | Date == as.Date("2007-02-02")
)

# Konwersja kolumn numerycznych
numeric_cols <- c(
  "Global_active_power",
  "Global_reactive_power",
  "Voltage",
  "Global_intensity",
  "Sub_metering_1",
  "Sub_metering_2",
  "Sub_metering_3"
)

power_sub[numeric_cols] <- lapply(power_sub[numeric_cols], as.numeric)

# Utworzenie zmiennej datetime
power_sub$Datetime <- strptime(
  paste(power_sub$Date, power_sub$Time),
  format = "%Y-%m-%d %H:%M:%S"
)

# ----------------------------
# 3. Wykres 1
# ----------------------------

png("plot1.png", width = 480, height = 480)

hist(
  power_sub$Global_active_power,
  col = "red",
  main = "Global Active Power",
  xlab = "Global Active Power (kilowatts)",
  border = "black"
)

dev.off()

# ----------------------------
# 4. Wykres 2
# ----------------------------

png("plot2.png", width = 480, height = 480)

plot(
  power_sub$Datetime,
  power_sub$Global_active_power,
  type = "l",
  xlab = "",
  ylab = "Global Active Power (kilowatts)"
)

dev.off()

# ----------------------------
# 5. Wykres 3
# ----------------------------

png("plot3.png", width = 480, height = 480)

plot(
  power_sub$Datetime,
  power_sub$Sub_metering_1,
  type = "l",
  xlab = "",
  ylab = "Energy sub metering"
)

lines(
  power_sub$Datetime,
  power_sub$Sub_metering_2,
  col = "red"
)

lines(
  power_sub$Datetime,
  power_sub$Sub_metering_3,
  col = "blue"
)

legend(
  "topright",
  legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
  col = c("black", "red", "blue"),
  lty = 1,
  bty = "n"
)

dev.off()

# ----------------------------
# 6. Wykres 4
# ----------------------------

png("plot4.png", width = 480, height = 480)

par(mfrow = c(2, 2))

# Panel 1
plot(
  power_sub$Datetime,
  power_sub$Global_active_power,
  type = "l",
  xlab = "",
  ylab = "Global Active Power"
)

# Panel 2
plot(
  power_sub$Datetime,
  power_sub$Voltage,
  type = "l",
  xlab = "datetime",
  ylab = "Voltage"
)

# Panel 3
plot(
  power_sub$Datetime,
  power_sub$Sub_metering_1,
  type = "l",
  xlab = "",
  ylab = "Energy sub metering"
)

lines(
  power_sub$Datetime,
  power_sub$Sub_metering_2,
  col = "red"
)

lines(
  power_sub$Datetime,
  power_sub$Sub_metering_3,
  col = "blue"
)

legend(
  "topright",
  legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
  col = c("black", "red", "blue"),
  lty = 1,
  bty = "n",
  cex = 0.8
)

# Panel 4
plot(
  power_sub$Datetime,
  power_sub$Global_reactive_power,
  type = "l",
  xlab = "datetime",
  ylab = "Global_reactive_power"
)
dev.off()