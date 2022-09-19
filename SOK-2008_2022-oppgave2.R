# HELP SCRIPT FILE  Utfording 2.3 - sok-2008

# Download the data file union_unempl.csv and store it in an easily accessible location, such as a folder on your Desktop or in your personal folder.
# Install any R packages that you need using the command install.packages("package name").
# You will need the following packages for the assignment: 

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readr")
install.packages("plyr")
install.packages("dplyr")


# Set your working directory to the correct folder. 
# Insert your file path for 'YOURFILEPATH'. 
getwd()
#setwd("C:/Users/OddVi/OneDrive/Dokumenter/Studier/SOK-2008/InnleveringerC:/Users/OddVi/OneDrive/Dokumenter/Studier/SOK-2008/Innleveringer")

# You will need the following libraries for the assignment:

library(readr) # fileformat of the dataset
library(ggplot2)     # the ggplot package
library(tidyverse)  # the tidyverse package


# To carry out the assignment, you will need to combine the union_unempl data with map data. 


union<- read_csv("union_unempl.csv") #This loads the data with information about the variables of interest
View(union) #Displays the data
#To combine the unemployment and union data with the map data, we merge on country name. 
#We face two problems here: 1) United Kingdom is called "UK" in the map data, 2) the variable "country" is called "region" in the map data. We need to change this.

#Changing the name of a single observation. The below code changes all observations called "United Kingdom" to "UK" in the union data. 
union$country <- gsub("United Kingdom", "UK", union$country)
View(union) 

# Renaming a variable. The below code renames the variable "Country" to "Region".
names(union)[names(union) == "country"] <- "region"
View(union) 

# Creating a new variable. To create a map showing "Excess coverage", you need to create a new variable. 
# The below code shows how to create a new variable in R. 
union$newvar2<-union$var1 + union$var2 #A sum
union$newvar1<-union$var1 - union$var2 #A difference
union$newvar3<-(union$var1 + union$var2)/2 # A mean value

# You are now ready to create your maps! Follow the tutorial at https://www.youtube.com/watch?v=AgWgPSZ7Gp0 

# The "Coord" variable takes 5 discrete levels. It may therefore be better to use a discrete scale for the coloring. 
# To do this, simply replace "scale_fill_gradient(name="name", low="color1", high="color2", na.value="grey50")" 
# with "scale_fill_brewer(name="name", palette = "Set1")" (or another set you prefer)

# Utfordring 2.3:

#1. Lag kart over Europa som viser 1) arbeidsledighetsrate i ulike land.

# Henter kartdata og slår disse sammen med datasettet "union" til ett datasett
mapdata <- map_data("world")
View(mapdata)
mapdata <- left_join(mapdata, union, by = "region")
View(mapdata)

# Fjerner alle land som er utenfor Europa:
mapdata_1 <- mapdata %>% 
  filter(!is.na(mapdata$unempl))
View(mapdata_1)

# Lager kart for arbeidsledighetsrate "uempl":
Unempl <- ggplot(mapdata_1, aes(x =long, y = lat, group = group)) +
  geom_polygon(aes(fill = unempl), color = "black") +
  scale_fill_gradient(name = "% uemployed", low = "grey", high = "red", na.value = "white") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Unemployment in Europe")
Unempl


#2. Lag kart over Europa som viser 1) fagforeningsdensitet, 2) “Excess coverage”, 
#og 3) Koordinering av lønnsfastsettelse.

# 1. Fagforeningsdensitet
# Lager nytt kart der jeg bruker variablen "density" 
Union_density <- ggplot(mapdata_1, aes(x =long, y = lat, group = group)) +
  geom_polygon(aes(fill = density), color = "black") +
  scale_fill_gradient(name = "Union density in %", low = "grey", high = "red", na.value = "white") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Union density in Europe")
Union_density


# 2. Excess coverage
# Lager ny variabel "excov" som er differansen mellom organisasjonsprosent målt i andelen av arbeidsstyrken og 
# andelen som er omfattet av tariffavtaler.

mapdata_1$excov <- mapdata_1$coverage - mapdata_1$density
mapdata_1

Excess_coverage <- ggplot(mapdata_1, aes(x =long, y = lat, group = group)) +
  geom_polygon(aes(fill = excov), color = "black") +
  scale_fill_gradient(name = "Excess_coverage in %", low = "grey", high = "red", na.value = "white") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Excess_coverage in Europe")
Excess_coverage

# 3. Koordinering av lønnsfastsettelse

Coordination <- ggplot(mapdata_1, aes(x =long, y = lat, group = group)) +
  geom_polygon(aes(fill = coord), color = "black") +
  scale_fill_brewer(name="Coordination level", palette = "Set1") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Coordination of wage bargaining in Europe")
Coordination

#3. Diskuter det du ser i dine grafer ut ifra kapittel 3 i Boeri og van Ours. 
# Kommenter hvordan det kan komme seg at de nordiske landene har sterke fagforeninger 
# og relativt lav arbeidsledighet

# Jeg lager en tabell som oppsummerer dataene for hvert land.
Oppsummering <- mapdata_1 %>% 
  distinct(region, unempl, density, coverage, excov, coord)
View(Oppsummering)

# Er det en sammenheng mellom ledighet ("unempl") og fagforeningenes styrke målt 
# ved organisasjonsprosent("density").
# Av "Oppsummering" ser vi at det er syv land som har en arbeidsledighet på ti prosent eller mer.
# I denne gruppen varierer organisasjonsgraden fra 9,9 % i Tyrkia til 32,5 % i Italia. 
# En liten test på samvariasjon viser at det er en svak negativ sammenheng mellom disse variablene.
test_1 <- cov(mapdata_1$unempl,mapdata_1$density)
test_1


# Er det sammenheng mellom ledighet og hvor stor andel av alle arbeidstakere som er omfattet av 
# tariffavtaler("coverage"?
# Vi ser at "coverage" varierer fra 8,5 % i Tyrkia til 100 % i Italia.
# En tilsvarende test for samvariasjon viser at det er sterkere, men fremdeles svak negativ 
# sammenheng mellom ledighet og omfang av tariffavtaler. 
test_2 <- cov(mapdata_1$unempl,mapdata_1$coverage, use = "complete.obs")
test_2

# Er det en sammenheng mellom ledighet og Excess_coverage (Forskjellen mellom hvor stor andel
# som omfattes av tariffavtalene (coverage) og andelen fagorganiserte (density)?
# Vi ser av tabellen "Oppsummering" at for de ti landene med høyest ledighet spriker det veldig.
# En test på samvariasjonen viser ingen sammenheng.
test_3 <- cov(mapdata_1$unempl,mapdata_1$excov, use = "complete.obs")
test_3

# Det er en rekke årsaker til den nordiske situasjonen med relativt lav ledighet og
# sterke fagforeninger. Jeg kan her nevne noen.
# For det første er det samfunnsmessig aksept for at interesseorganisering på begge sider er
# viktig og riktig. 
# For det andre er samarbeidet mellom arbeidsgivere, arbeidstakere og 
# politiske myndigheter så omfattende at det legger stort ansvar på partene til å komme fram 
# til løsninger som ikke bidrar til økt ledighet. Det er for eksempel sjelden at en
# hovedtariffavtale ender med reallønnsøkning, og sjelden at noen tariffområder kommer bedre
# ut enn andre.
# For det tredje er lønnsdannelsen ganske kontrollert slik at lite av lønnstileggene blir gitt
# utenom oppgjørene. Det er få tariffområder som har lokale lønnsforhandlinger.  



