
library(openxlsx)
library(lubridate)
library(dplyr)
library(reshape2)

# Load and merge the SPF forecasts for GDP recess ---------------------------------------------------------------------------------------------

# Data sources:
# https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/recess
# https://www.philadelphiafed.org/research-and-data/real-time-center/real-time-data/data-files/routput

# No licence terms found on the Philadelphia FED homepage

spf.gdp <- read.xlsx("data-raw/data_SPF/Individual_RECESS.xlsx")
spf.gdp$RECESS1 <- as.numeric(spf.gdp$RECESS1)/100
spf.gdp$RECESS2 <- as.numeric(spf.gdp$RECESS2)/100
spf.gdp$RECESS3 <- as.numeric(spf.gdp$RECESS3)/100
spf.gdp$RECESS4 <- as.numeric(spf.gdp$RECESS4)/100
spf.gdp$RECESS5 <- as.numeric(spf.gdp$RECESS5)/100

colnames(spf.gdp)[1:2] <- c("YEAR.issued", "QUARTER.issued")

# Forecast combination has ID=0
spf.avg <- spf.gdp %>% group_by(YEAR.issued, QUARTER.issued) %>% summarise(ID=0, INDUSTRY="#N/A",
                                                                           RECESS1 = mean(as.numeric(RECESS1), na.rm = TRUE),
                                                                           RECESS2 = mean(as.numeric(RECESS2), na.rm = TRUE),
                                                                           RECESS3 = mean(as.numeric(RECESS3), na.rm = TRUE),
                                                                           RECESS4 = mean(as.numeric(RECESS4), na.rm = TRUE),
                                                                           RECESS5 = mean(as.numeric(RECESS5), na.rm = TRUE))

# Join data frames together and sort by year, quarter and ID
spf.gdp.tmp <- rbind(data.frame(spf.avg),spf.gdp)
spf.gdp <- spf.gdp.tmp[order(spf.gdp.tmp$YEAR.issued, spf.gdp.tmp$QUARTER.issued, spf.gdp.tmp$ID),]

# Add a date format
spf.gdp$DATE.issued <- yq(paste(spf.gdp$YEAR.issued,spf.gdp$QUARTER.issued))



# Load and merge the DGP decline SPF data ---------------------------------------------------------------------------------------------
gdp.raw <- read.xlsx("data-raw/data_SPF/ROUTPUTQvQd.xlsx")

# obtain the last estimation of GDP; start with the entry 1968:Q3, i.e. entry number 87, such that we have a diff for 1968:Q4
gdp <- gdp.raw[87:290, c("DATE", "ROUTPUT19Q3")]
names(gdp) <- c("DATE","gdp.last")
gdp$gdp.last.diff <- c(NA,diff(gdp$gdp.last))
gdp$gdp.last.recess <- as.numeric(gdp$gdp.last.diff < 0)


gdp.triangle <- as.matrix(gdp.raw[c(87:290),c(14:217)])
n.triangle <- dim(gdp.triangle)[1]

# obtain the first entry in each matrix row... there might be an easier implementation out there...
gdp$gdp.first <- sapply(1:n.triangle, function(i) as.numeric(gdp.triangle[i,min(which(!is.na(as.numeric(gdp.triangle[i,]))))]))
gdp$gdp.first.diff <- c(NA, diff(gdp$gdp.first))
gdp$gdp.first.recess <- as.numeric(gdp$gdp.first.diff < 0)

# add a date
gdp$DATE <- yq(gdp$DATE)



###  Merge together the SPF GDP forecasts with realizations ---------------------------------------------------------------------------------------------
spf.gdp.long <- spf.gdp %>% rename("0" = "RECESS1", "1" = "RECESS2", "2" = "RECESS3", "3" = "RECESS4", "4" = "RECESS5") %>%
  melt(d.vars=c("YEAR.issued", "QUARTER.issued", "ID", "INDUSTRY", "DATE.issued"),
       measure.vars=c("0", "1", "2", "3", "4"),
       variable.name="FC.Horizon",
       value.name="Prob.Forecast") %>%
  mutate(DATE.FC.due = DATE.issued + months(3*as.numeric(paste(FC.Horizon)))) %>%
  merge(gdp%>%dplyr::select(DATE,gdp.last.recess,gdp.first.recess), by.x="DATE.FC.due", by.y="DATE")

# Delete NA forecasts!
spf.gdp.long <- spf.gdp.long[!is.na(spf.gdp.long$Prob.Forecast),]



# save files as RData
usethis::use_data(spf.gdp.long,overwrite = TRUE)
# usethis::use_data(spf.gdp,overwrite = TRUE)
# usethis::use_data(gdp, overwrite = TRUE)

