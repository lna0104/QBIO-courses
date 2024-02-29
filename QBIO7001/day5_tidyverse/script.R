library(tidyverse)
library(boilrdata)

marsupialsTibble <- as_tibble(marsupials)
marsupialsTibble

marsupials[1:8, "Family"]
marsupialsTibble[1:8, "Family"]

coeffs <- marsupials %>%
  select(Mass, MaxLifeSpan) %>%
  log() %>%
  lm(MaxLifeSpan ~ Mass, data = .) %>%
  coefficients()
coeffs
#> (Intercept)        Mass 
#>      0.0577      0.2682


phytoplankton$chlorophyllA

pivot_longer(phytoplankton$chlorophyllA, 
             -pond, #change all column except pond
             names_to = "date", #change colnames to value in date column
             values_to = "chlorophyllA")

separate(reefFishDiversity, Phyl_name, c("Family", "Genus", "Species"), sep = "_")[1:10,]

data <- reefFishDiversity |> 
  separate(Phyl_name, c("Family", "Genus", "Species"), sep = "_") |>
  unite("FullSpecies", c(Genus, Species), sep = " ", remove = FALSE)

# check:
data[1:10,]

reefFishFST %>% select(c(2, 6:11))

reefFishFST %>% select(2, last_col(5):last_col())

marsupials |> 
  filter(MaxLifeSpan >= 20)



### Exercises

#5.1.
library(readxl)
library(tidyverse)
excel_sheets("./day5_tidyverse/doi_10_5061_dryad_cg464__v20181211/Escape behaviour.xlsx")

behave <- read_excel("./day5_tidyverse/doi_10_5061_dryad_cg464__v20181211/Escape behaviour.xlsx", sheet = "threat_speed-new2")
behaveTibble <- as.tibble(behave)
behaveTibble

colnames(behaveTibble) <- c("Indvidual", "Sex", "Mass", "Beam_diameter", "Trial_number", "Stride_number", 
                         "Maximum_stride_velocity", "Slip", "Time_after_slip", "Strides_after_slip", "Return_to_slip")

behaveTibble$Slip <- as.logical(behaveTibble$Slip)
behaveTibble$Return_to_slip <- as.logical(behaveTibble$Return_to_slip)

#5.2.

behaveTibble <- behave |> as.tibble() |>
  `colnames<-`(c("Indvidual", "Sex", "Mass", "Beam_diameter", "Trial_number", "Stride_number", 
                  "Maximum_stride_velocity", "Slip", "Time_after_slip", "Strides_after_slip", "Return_to_slip"))

behaveTibbleLogical <- behaveTibble |>  
  mutate(Slip <- as.logical(Slip), Return_to_slip <- as.logical(Return_to_slip))

#5.3.
library(boilrdata)

write.csv(senecio, "/Users/lena/Desktop/Courses/QBIO7001/day3_python/data/senecio.csv", row.names =  FALSE)
ePopulation <- senecio |> 
  group_by(Population, Ecotype) |>
  summarise(numberIndPerPop = n(),
            meanHeightPerPop = mean(VegHeight)
            ) 

eEcotype <-  senecio |> 
  group_by(Ecotype) |>
  summarise(numberPerEco = n(),
            meanHeightPerEco = mean(VegHeight)) 


alterEcotype <- ePopulation |>
  group_by(Ecotype) |>
  summarise(numberPopPerEco = n(),
            numberIndPerEco=sum(numberIndPerPop),
            meanHeightPerEco = sum(meanHeightPerPop)/numberPopPerEco)


#5.4.
groupedMarsupials<- marsupials |> 
  group_by(Family) |>
  summarise(n = n(), 
            meanPopSize = mean(PopDensity),
            minMass = min(Mass),
            medianMass = median(Mass),
            maxMass = max(Mass),
            quantileMas = quantile(Mass))

#5.5

marsupialName <- read_csv("./day5_tidyverse/marsupial_common_names.csv", col_names = c("CommonName", "ScientificName"))
marsupials <- marsupials |> 
  mutate(ScientificName = paste(Genus, Species)) |>
  left_join(marsupialName, by = "ScientificName")
