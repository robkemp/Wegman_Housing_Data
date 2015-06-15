library(readxl)
library(stringr)
library(dplyr)
library(tidyr)

muni_2000_orig=read_excel("Raw Data/CityandCountyPopAndHousingEstimates_2010Intercensal.xls")

muni_2000=muni_2000_orig%>%
  gather(variable, value, -CountyFIPS:-Areaname)%>%
  filter(!grepl("0c", variable),!grepl("0x", variable))%>%
  mutate(year=paste0("20",str_sub(variable, -2,-1)),
         var_name=ifelse(grepl("Tp", variable), "totalPopulation",
                        ifelse(grepl("Hp", variable), "householdPopulation",
                        ifelse(grepl("Gqp", variable), "groupQuartersPopulation",
                        ifelse(grepl("Thu", variable), "totalHousingUnits",
                        ifelse(grepl("Ohu", variable), "occupiedHousingUnits",
                        ifelse(grepl("Pph", variable), "personsPerHousehold",
                        ifelse(grepl("Vhu", variable), "vacantHousingUnits",
                        ifelse(grepl("Vr", variable), "vacancyRate", NA)))))))))%>%
  filter(year!="2010")%>%
  select(-variable)%>%
  spread(var_name, value)


muni_2010_orig=read_excel("Raw Data/CityandCountyMaster_v2013.xlsx")

muni_2010=muni_2010_orig%>%
  gather(variable, value, -CountyFIPS:-MuniTotalFlag)%>%
  filter(!grepl("0c", variable),!grepl("0x", variable))%>%
  mutate(year=paste0("20",str_sub(variable, -2,-1)),
         var_name=ifelse(grepl("Tp", variable), "totalPopulation",
                  ifelse(grepl("Hp", variable), "householdPopulation",
                  ifelse(grepl("Gqp", variable), "groupQuartersPopulation",
                  ifelse(grepl("Thu", variable), "totalHousingUnits",
                  ifelse(grepl("Ohu", variable), "occupiedHousingUnits",
                  ifelse(grepl("Pph", variable), "personsPerHousehold",
                  ifelse(grepl("Vhu", variable), "vacantHousingUnits",
                  ifelse(grepl("Vr", variable), "vacancyRate", NA)))))))))%>%
  select(-variable, -MuniTotalFlag)%>%
  spread(var_name, value)


muni_2000_2010= bind_rows(muni_2000, muni_2010)%>%
  select(-MultiCountyPlaceFlag)

write.csv(muni_2000_2010, "Clean Data/ColoradoMuniHousingPopulation_00_10.csv")
save(muni_2000_2010, file="Clean Data/coMuni_00_10.Rdata")

