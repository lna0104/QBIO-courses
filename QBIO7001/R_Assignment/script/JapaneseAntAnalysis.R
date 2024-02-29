library(tidyverse)
library(ggplot2)
library(stringr) 

### Step 1: Data import and tidying -----

antsJapaneseData <- as.tibble(read_csv("./data/ants_Japanese_islands.csv"))

tidyData <- antsJapaneseData |> 
  `colnames<-` (c("Subfamily", "Genus", "Species", "Status", "Exotic", "IslandName", 
                 "IslandGroup", "Archipelago", "BiogeographicRealm", "Lat", "Long")) |>
  mutate_all(~replace(., . == "-", NA)) |>
  mutate(GenusSpecies = paste(Genus, Species, sep="_"))

### Step 2: Extracting information from the data -----

#1. How many data entries are there for the subfamily Formicinae?
  
nrow(tidyData[tidyData$Subfamily == "Formicinae",])  

#2. How many data entries are there for native species on the island of Hokkaido?

nrow(tidyData[tidyData$IslandName == "Hokkaido" & tidyData$Exotic == "Native", ])

#3. What latitudinal range do the species in the data set cover?

range(tidyData$Lat)

#4. How many different ant species does the data set contain?

length(unique(tidyData$GenusSpecies))

#5. Which specific epithets (the second part of the scientific names) are shared by several species?

sharedEpithet <-tidyData |> 
  select(Species, Genus) |>
  distinct() |>
  group_by(Species) |>
  summarise(n = n()) |>
  filter(n>1)

### Step 3: Plotting the number of species per island -----

# Calculate number of islands for each species and exotic
nIslandsPerSpecies <- tidyData |>
  group_by(GenusSpecies, Exotic) |>
  summarise(nIslands = n())

# Plot 
step3Plot <- ggplot(nIslandsPerSpecies) + 
              geom_bar(aes(nIslands)) +
              theme_bw() +
              facet_wrap(vars(Exotic)) + 
              labs(x= "Number of islands", y= "Number of species records")

ggsave(filename = "./plots/step3Plot.pdf", plot = step3Plot)

### Step 4: Combining the data with geographical information -----

# Import Japanese island data
JapaneseIslandData <- read_csv("./data/Japanese_islands.csv")

# Summarise data set containing the number of reported ant species for each of the islands
# Change the name of islands to match between 2 datasets
nSpeciesPerIsland <- tidyData |> 
  group_by(IslandName) |>
  summarise(nSpecies = n()) |>
  mutate(lowerIslandName = tolower(IslandName)) |> #change to lower case
  mutate(modIslandName = str_replace_all(lowerIslandName, "[^[:alnum:]]", "")) #remove all special characters

# Modify the name of islands
modNameJapaneseIsland <- JapaneseIslandData |>
  mutate(lowerName = tolower(Name)) |> #change to lower case
  mutate(modIslandName = str_replace_all(lowerName, "[^[:alnum:]]", "")) #remove all special characters

# Combine 2 datasets
combinedData <- nSpeciesPerIsland |>
  inner_join(modNameJapaneseIsland, by = "modIslandName") |>
  mutate(Density = Population/Area) |>
  select(IslandName, modIslandName, nSpecies, Area, Population, Density)

### Step 5: Plotting species numbers vs. island size -----

myPlot <- ggplot(combinedData) +
                geom_point(aes(x= log(Area), y = nSpecies, col = Density)) +
                theme_bw() +
                labs(x = "log(Island area [km2])", y= "Number of recorded species", color = "Population density")
step5Plot <- myPlot + scale_color_gradient(low="black", high="red")

ggsave(filename = "./plots/step5Plot.pdf", plot = step5Plot)



