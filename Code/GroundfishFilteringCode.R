## code to filter GEMM data for federally managed species
    ## and add combined fishery sectors
    ## saves new file after changes have been made

library(tidyverse)
library(readxl)


#BEFORE reading in new GEMM excel, make sure data is in the same format as the 2026 released data
    #name your excel "GEMM Data"
    #ignore this message if using data directly from Github

#read in the data
groundfishdata <- read_excel("GEMM Data.xlsx", sheet="Table 3 UPDATE", skip = 2)


#limit data to only include the federally managed species
    #Up to date as of June 2026
federallymanaged <- groundfishdata %>% 
  filter(Grouping == "Black rockfish (Washington)" | 
           Species %in% c("Arrowtooth Flounder", "Aurora Rockfish", "Bank Rockfish",
                        "Big Skate", "Blackgill Rockfish", "Bocaccio Rockfish",
                        "California Scorpionfish", "Canary Rockfish", "Chilipepper Rockfish",
                        "Cowcod Rockfish", "Darkblotched Rockfish", "Dover Sole", "English Sole",
                        "Flathead Sole", "Greenspotted Rockfish", "Greenstriped Rockfish",
                        "Lingcod", "Longnose Skate", "Longspine Thornyhead", "Pacific Cod",
                        "Pacific Hake", "Pacific Ocean Perch Rockfish", "Pacific Sanddab", "Petrale Sole",
                        "Quillback Rockfish (Washington/Oregon)", "Redbanded Rockfish", "Redstripe Rockfish", "Rex Sole",
                        "Rosethorn Rockfish", "Rougheye/Blackspotted Rockfish", "Sablefish", 
                        "Sharpchin Rockfish", "Shortraker Rockfish", "Shortspine Thornyhead", 
                        "Shortspine/Longspine Thornyhead", "Silvergray Rockfish", "Slope Rockfish Unid", 
                        "Pacific Spiny Dogfish", "Splitnose Rockfish", "Squarespot Rockfish", "Starry Rockfish", 
                        "Stripetail Rockfish", "Vermilion Rockfish", "Widow Rockfish", "Yelloweye Rockfish", 
                        "Yellowmouth Rockfish", "Yellowtail Rockfish"))


#add extra column with combined fishery sectors 
    #this code loops through each row and designates a combined sector based on the sector listed
    #any sectors that don't combine keep their original  distinction

federallymanaged$CombinedSector <- "na"

for (n in 1:nrow(federallymanaged)){
  if (
    federallymanaged$Sector[n] == "At-Sea Hake CP" || 
    federallymanaged$Sector[n] == "At-Sea Hake MSCV" || 
    federallymanaged$Sector[n] == "Tribal At-Sea Hake"){
    federallymanaged$CombinedSector[n] <- "At-Sea Hake"
  }
  else if(
    federallymanaged$Sector[n] == "CS - Bottom and Midwater Trawl" ||
    federallymanaged$Sector[n] == "CS - Bottom Trawl" ||
    federallymanaged$Sector[n] == "CS EM - Bottom Trawl"){
    federallymanaged$CombinedSector[n] <- "CS - Bottom Trawl"
  }
  else if(
    federallymanaged$Sector[n] == "California Recreational" ||
    federallymanaged$Sector[n] == "Oregon Recreational" ||
    federallymanaged$Sector[n] == "Washington Recreational"){
    federallymanaged$CombinedSector[n] <- "Recreational"
  }
  else if(
    federallymanaged$Sector[n] == "Combined LE & OA CA Halibut" ||
    federallymanaged$Sector[n] == "LE CA Halibut" ||
    federallymanaged$Sector[n] == "OA CA Halibut"){
    federallymanaged$CombinedSector[n] <- "CA Halibut"
  }
  else if(
    federallymanaged$Sector[n] == "CS - Pot" ||
    federallymanaged$Sector[n] == "CS EM - Pot"){
    federallymanaged$CombinedSector[n] <- "CS - Pot"
  }
  else if(
    federallymanaged$Sector[n] == "LE Fixed Gear DTL - Hook & Line" ||
    federallymanaged$Sector[n] == "LE Fixed Gear DTL - Pot" ||
    federallymanaged$Sector[n] == "LE Sablefish - Hook & Line" ||
    federallymanaged$Sector[n] == "LE Sablefish - Pot"){
    federallymanaged$CombinedSector[n] <- "Limited Entry Non Trawl"
  }
  else if(
    federallymanaged$Sector[n] == "Midwater Hake" ||
    federallymanaged$Sector[n] == "Midwater Hake EM" ||
    federallymanaged$Sector[n] == "Shoreside Hake"){
    federallymanaged$CombinedSector[n] <- "Midwater Hake"
  }
  else if(
    federallymanaged$Sector[n] == "Midwater Rockfish" ||
    federallymanaged$Sector[n] == "Midwater Rockfish EM"){
    federallymanaged$CombinedSector[n] <- "Midwater Rockfish"
  }
  else if(
    federallymanaged$Sector[n] == "OA Fixed Gear - Hook & Line" ||
    federallymanaged$Sector[n] == "OA Fixed Gear - Pot"){
    federallymanaged$CombinedSector[n] <- "Directed Open Access Groundfish"
  }
  else(
    federallymanaged$CombinedSector[n] <- federallymanaged$Sector[n]
  )
}


#add extra column with higher level combined fishery sectors
  #Recreational, Tribal, Research, Commercial groundfish trawl, Commercial non-groundfish,
  #Commercial groundfish non-trawl

federallymanaged$SuperCombined <- "na"

for (n in 1:nrow(federallymanaged)){
  if (
    federallymanaged$Sector[n] == "Tribal Shoreside" || 
    federallymanaged$Sector[n] == "Tribal At-Sea Hake"){
    federallymanaged$SuperCombined[n] <- "Tribal"
  }
  else if(
    federallymanaged$Sector[n] == "At-Sea Hake CP" ||
    federallymanaged$Sector[n] == "At-Sea Hake MSCV" ||
    federallymanaged$Sector[n] == "CS EM - Bottom Trawl" ||
    federallymanaged$Sector[n] == "CS EM - Pot" ||
    federallymanaged$Sector[n] == "CS - Bottom and Midwater Trawl" ||
    federallymanaged$Sector[n] == "CS - Bottom Trawl" ||
    federallymanaged$Sector[n] == "CS - Pot" ||
    federallymanaged$Sector[n] == "CS - Hook & Line" ||
    federallymanaged$Sector[n] == "CS - Midwater" ||
    federallymanaged$Sector[n] == "Limited Entry Trawl" ||
    federallymanaged$Sector[n] == "Midwater Hake" ||
    federallymanaged$Sector[n] == "Midwater Hake EM" ||
    federallymanaged$Sector[n] == "Midwater Rockfish" ||
    federallymanaged$Sector[n] == "Midwater Rockfish EM" ||
    federallymanaged$Sector[n] == "Shoreside Hake"){
    federallymanaged$SuperCombined[n] <- "Commercial Groundfish Trawl"
  }
  else if(
    federallymanaged$Sector[n] == "California Recreational" ||
    federallymanaged$Sector[n] == "Oregon Recreational" ||
    federallymanaged$Sector[n] == "Washington Recreational"){
    federallymanaged$SuperCombined[n] <- "Recreational"
  }
  else if(
    federallymanaged$Sector[n] == "Pink Shrimp" ||
    federallymanaged$Sector[n] == "Directed P Halibut" ||
    federallymanaged$Sector[n] == "OA CA Halibut" ||
    federallymanaged$Sector[n] == "LE CA Halibut" ||
    federallymanaged$Sector[n] == "Incidental" ||
    federallymanaged$Sector[n] == "Combined LE & OA CA Halibut"){
    federallymanaged$SuperCombined[n] <- "Commercial Non-Groundfish"
  }
  else if(
    federallymanaged$Sector[n] == "LE Fixed Gear DTL - Hook & Line" ||
    federallymanaged$Sector[n] == "LE Fixed Gear DTL - Pot" ||
    federallymanaged$Sector[n] == "LE Sablefish - Hook & Line" ||
    federallymanaged$Sector[n] == "LE Sablefish - Pot" ||
    federallymanaged$Sector[n] == "Nearshore" ||
    federallymanaged$Sector[n] == "OA Fixed Gear - Hook & Line" ||
    federallymanaged$Sector[n] == "OA Fixed Gear - Pot"){
    federallymanaged$SuperCombined[n] <- "Commercial Groundfish Non-Trawl"
  }
  else(
    federallymanaged$SuperCombined[n] <- federallymanaged$Sector[n]
  )
}


#Shiny does not like complex column names, so let's rename them
federallymanaged <- federallymanaged %>% 
  rename("DiscardMortality" = "Discard Mortality",
         "TotalCatch" = "Catch (Landings and Discards)",
         "TotalMortality" = "Mortality (Landings and Discard Mortality)")

view(federallymanaged)


#save as .csv file
write.csv(federallymanaged, "ShinyReadyData.csv", row.names=FALSE)





