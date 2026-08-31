
library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(DT)
library(markdown)

#this code is run when the page initializes---------------------------------------------------------------------------
  #before any user input

groundfish <- read.csv("ShinyReadyData.csv")


#this code assigns specific colors to each fine scale sector
  #trying to be colorblind friendly-ish

allSectors <- c("At-Sea Hake", "CA Halibut", "CS - Bottom Trawl", "CS - Hook & Line", 
                "CS - Pot","Directed Open Access Groundfish", 
                "Directed Pacific Halibut" = "Directed P Halibut", 
                "Incidental", "Limited Entry Non Trawl", "Limited Entry Trawl", 
                "Midwater Hake", "Midwater Rockfish", "Nearshore", 
                "Pink Shrimp", "Recreational", "Research", "Tribal Shoreside")
selectSectorColors <- c( "#3D2E36", "#882255", "#332288", "#745FDC", "#3E654B", "#41C46D",
                         "#44AA99", "#5AC3B2","#0B6998", "#88CCEE", "#DDCC77", "#C9AD1E",
                         "#CC6677", "#94162C", "#AA4499", "#CC76BD", "#BD6692")
sectorColors <- setNames(selectSectorColors, allSectors)



#this code assigns specific colors to each coarse scale sector

comboSectors <- c("Federal Trawl", "Federal Fixed Gear", 
                  "Recreational", "Research", "State", "Tribal")
selectComboColors <- c("#44AA99", "#88CCEE", "#DDCC77", "#CC6677", "#9A598F", "#8E3B64")
comboColors <- setNames(selectComboColors, comboSectors)



#this code assigns specific colors for totals, discards, and landings

discardsLandingsColors <- c("#5AC3B2", "#CC6677")
names(discardsLandingsColors) <- c("Landings", "DiscardMortality")
mortalityColors <- c("Total" = "grey85", discardsLandingsColors)



#this code assigns specific colors for recreational breakdown

recStates <- c("Washington Recreational", "Oregon Recreational", 
               "California Recreational")
stateColors <- setNames(c("#88CCEE", "#DDCC77", "#CC6677"),
                        recStates)



# now respond to user input--------------------------------------------------------------------------------------

function(input, output, session) {

  
  
#Make a table output that shows how GEMM sectors are grouped
  output$sectorTable <- DT::renderDT({
    groundfish %>%
      group_by(Sector) %>%
      summarize(CombinedSector = unique(CombinedSector),
                SuperCombined = unique(SuperCombined)) %>%
      rename("GEMM Sector" = Sector,
             "Refined Sector" = CombinedSector,
             "Combined Sector" = SuperCombined) %>%
      datatable(options = list(paging = FALSE))
  })
  

    
#Make our coarse scale sector plot for selecting one species
  output$comboSectorsPlot <- renderPlot({
    
    #this code filters the dataset for only the chosen species
    groundfish %>% 
      filter(Species == input$speciesInput) %>%
      group_by(Year, SuperCombined) %>%
      summarize(Landings = sum(Landings),
                TotalMortality = sum(TotalMortality),
                DiscardMortality = sum(DiscardMortality)) %>%
      ggplot(aes(x = Year, y = TotalMortality, fill = SuperCombined)) +
      geom_bar(position = "stack", stat = "identity") +
      labs(title = paste(input$speciesInput, "Mortality by Fishery Sector"),
           y = "Total Mortality (mt)",
           x = "Year") +
      theme_classic() +
      scale_fill_manual(values = comboColors, name = "Combined Sector") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(expand = c(0,0),
                         limits = c(2002-0.5, max(groundfish$Year)+0.5),
                         breaks = seq(2002, max(groundfish$Year), by = 1)) +
      theme(plot.title = element_text(size = 20, color = "grey15",
                                      margin = margin(b = 20)),
            axis.title = element_text(size = 16, color = "grey15"),
            legend.title = element_text(size = 15, color = "grey15"),
            legend.text = element_text(size = 15, color = "grey15"),
            axis.text.x = element_text(size = 12, angle = 45, 
                                       hjust = 1, vjust = 1,
                                       color = "grey30"),
            axis.text.y = element_text(size = 12, color = "grey30"))
  })

  
   
#make our fine scale sector plot for selecting one species
  output$allSectorsPlot <- renderPlot({
 
  #this code filters the dataset for only the chosen species and feeds it into ggplot
    groundfish %>% 
      filter(Species == input$speciesInput) %>%
      group_by(Year, CombinedSector) %>%
      summarize(Landings = sum(Landings),
                TotalMortality = sum(TotalMortality),
                DiscardMortality = sum(DiscardMortality)) %>%
      ggplot(aes(x = Year, y = TotalMortality, fill = CombinedSector)) +
        geom_bar(position = "stack", stat = "identity") +
        labs(title = paste(input$speciesInput, "Mortality by Fishery Sector"),
             y = "Total Mortality (mt)",
             x = "Year") +
        theme_classic() +
        scale_fill_manual(values = sectorColors, name = "Sector") +
        scale_y_continuous(expand = c(0,0)) +
        scale_x_continuous(expand = c(0,0),
                          limits = c(2002-0.5, max(groundfish$Year)+0.5),
                          breaks = seq(2002, max(groundfish$Year), by = 1)) +
        theme(plot.title = element_text(size = 20, color = "grey15",
                                      margin = margin(b = 20)),
              axis.title = element_text(size = 16, color = "grey15"),
              legend.title = element_text(size = 15, color = "grey15"),
              legend.text = element_text(size = 15, color = "grey15"),
              axis.text.x = element_text(size = 12, angle = 45, 
                                         hjust = 1, vjust = 1,
                                         color = "grey30"),
              axis.text.y = element_text(size = 12, color = "grey30"))
  })

 
   
#Make our plots for choosing one species and one sector
  output$individualSectorPlots <- renderUI({
    req(input$sectorInput)
    
    plotList <- lapply(input$sectorInput, function(sector) {
      plotname <- paste0(sector)
      plotOutput(plotname, height = "300px")
    })
    do.call(tagList, plotList)
  })
  observe({
    req(input$sectorInput, input$speciesInput)

  #this code loops through and finds the desired sector(s) and plots
    for (sector in input$sectorInput) {
      local({
        currentSector <- sector
        plotname <- paste0(sector)
        
        output[[plotname]] <- renderPlot({
          
          total <- groundfish %>% 
            filter(Species == input$speciesInput) %>%
            group_by(Year, CombinedSector) %>%
            summarize(Landings = sum(Landings),
                      TotalMortality = sum(TotalMortality),
                      DiscardMortality = sum(DiscardMortality))
          sector <- total %>% filter(CombinedSector == currentSector) %>%
            pivot_longer(cols=c(DiscardMortality, Landings),
                         values_to = "Value",
                         names_to = "Key")
          ggplot() +
            geom_col(data = total, 
                     aes(x = Year, y = TotalMortality, fill = "Total")) +
            geom_col(data = sector, 
                     aes(x = Year, y = Value, fill = Key)) +
            theme_classic() +
            scale_fill_manual(values = mortalityColors, 
                              labels = c("Total" = "Total Mortality Across Sectors", 
                                         "DiscardMortality" = "Discard Mortality",
                                         "Landings" = "Landings")) +
            scale_y_continuous(expand = c(0,0)) +
            scale_x_continuous(expand = c(0,0),
                               limits = c(2002-0.5, max(groundfish$Year)+0.5),
                               breaks = seq(2002, max(groundfish$Year), by = 1)) +
            labs(title = paste(input$speciesInput, "Mortality"),
                 subtitle = paste(currentSector),
                 y = "Total Mortality (mt)") +
            theme(plot.title = element_text(size = 18, color = "grey15"),
                  plot.subtitle = element_text(size = 14, color = "grey15"),
                  axis.title = element_text(size = 16, color = "grey15"),
                  legend.title = element_text(size = 15, color = "grey15"),
                  legend.text = element_text(size = 15, color = "grey15"),
                  axis.text.x = element_text(size = 12, angle = 45, 
                                             hjust = 1, vjust = 1,
                                             color = "grey30"),
                  axis.text.y = element_text(size = 12, color = "grey30"))
        })
      })
    }
  })

  
  
#Display message for certain sector selections
  output$conditionalMessage <- renderUI({
    messages <- sapply(input$sectorInput, function(sector) {
      if (sector == "Directed P Halibut") {
        "*Directed Pacific Halibut fishery was only observed from 2017-2023, discards are not estimated for other years."
      } else if (sector == "Nearshore") {
        "*Nearshore fishery was only observed from 2003-2024, discards are not estimated for other years."
      } else if (sector == "Pink Shrimp") {
        "*Pink Shrimp fishery was only observed from 2004-2005 and 2007-2024, discards are not estimated for other years."
      } else if (sector == "Directed Open Access Groundfish") {
        "*Directed Open Access Groundfish fishery has been observed since 2003, discards are not estimated for prior years."
      } else {
        NA_character_
      }
    })
    messages <- messages[!is.na(messages)]
    tagList(lapply(messages, function(m) tags$p(tags$em(m))))
  })
  
  
  
#Make a button to download data for a specific species
  output$downloadSpeciesData <- downloadHandler(
    filename = function(){
      paste0(input$speciesInput, "MortalityData.csv")
      },
    content = function(file){
      
      speciesSubset <- groundfish %>% filter(Species == input$speciesInput)
      write.csv(speciesSubset, file, row.names = FALSE)
      
      # # if sector input was provided
      #   req(input$sectorInput)
      #   # also filter to that
      #   speciesSubset <- groundfish %>% filter(Species == input$speciesInput, CombinedSector %in% input$sectorInput)
      #   write.csv(speciesSubset, file, row.names = FALSE)
        
    }
  )


  
#Make a plot for selected species in the recreational sector
  #filter
  output$recMortalityPlot <- renderPlot({
    groundfish %>% 
      filter(Species == input$recSpeciesInput, 
             CombinedSector == "Recreational") %>%
      group_by(Year, Sector) %>%
      summarize(Landings = sum(Landings),
                TotalMortality = sum(TotalMortality),
                DiscardMortality = sum(DiscardMortality)) %>%
  #plot
      ggplot(aes(x = Year, y = TotalMortality, fill = Sector)) +
        geom_bar(position = "stack", 
                 stat = "identity", 
                 width = 0.9) +
        labs(title = paste(input$recSpeciesInput, "Mortality from Recreational Fishery"),
             y = "Total Mortality (mt)",
             x = "Year") +
        theme_classic() +
        scale_y_continuous(expand = c(0,0)) +
        scale_x_continuous(expand = c(0,0), 
                           limits = c(2002-0.5, max(groundfish$Year)+0.5),
                           breaks = seq(2002, max(groundfish$Year), by = 1)) +
        theme(plot.title = element_text(size = 20, color = "grey15",
                                        margin = margin(b = 20)),
              axis.title = element_text(size = 16, color = "grey15"),
              legend.title = element_text(size = 15, color = "grey15"),
              legend.text = element_text(size = 15, color = "grey15"),
              axis.text.x = element_text(size = 12, angle = 45, 
                                         hjust = 1, vjust = 1,
                                         color = "grey30"),
              axis.text.y = element_text(size = 12, color = "grey30")) +
        scale_fill_manual(values = stateColors, name = "State",
                        labels = c("Washington Recreational" = "Washington",
                                   "California Recreational" = "California",
                                   "Oregon Recreational" = "Oregon"))
  })
  
  
  
#Make a plot for selected species in Washington recreational
  #filter
  output$washingtonRecPlot <- renderPlot({
    groundfishRec <- groundfish %>% 
      filter(Species == input$recSpeciesInput, 
             CombinedSector == "Recreational") %>%
      group_by(Year, Sector) %>%
      summarize(Landings = sum(Landings),
                TotalMortality = sum(TotalMortality),
                DiscardMortality = sum(DiscardMortality))
    
    washingtonRec <- groundfishRec %>%
      filter(Sector == "Washington Recreational") %>%
      pivot_longer(cols = c(Landings, DiscardMortality),
                   names_to = "Key",
                   values_to = "Value")
  #plot
    ggplot() +
      geom_col(data = groundfishRec, 
               aes(x = Year, y = TotalMortality, fill = "Total"), 
               width = 0.9) +
      geom_col(data = washingtonRec, 
               aes(x = Year, y = Value, fill = Key), 
               width = 0.9) +
      scale_fill_manual(values = mortalityColors, 
                        labels = c("Total" = "All Recreational", 
                                   "DiscardMortality" = "Discard Mortality",
                                   "Landings" = "Landings")) +
      theme_classic() +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(expand = c(0,0), 
                         limits = c(2002-0.5, max(groundfish$Year)+0.5),
                         breaks = seq(2002, max(groundfish$Year), by = 1)) +
      labs(title = "Washington Recreational Fishery",
           subtitle = paste(input$recSpeciesInput),
           y = "Total Mortality (mt)") +
      theme(plot.title = element_text(size = 18, color = "grey15"),
            plot.subtitle = element_text(size = 16, color = "grey15"),
            axis.title = element_text(size = 16, color = "grey15"),
            legend.title = element_text(size = 15, color = "grey15"),
            legend.text = element_text(size = 15, color = "grey15"),
            axis.text.x = element_text(size = 12, angle = 45, 
                                       hjust = 1, vjust = 1,
                                       color = "grey30"),
            axis.text.y = element_text(size = 12, color = "grey30"))
  })
  

  
#Make a plot for selected species in Oregon recreational  
  #filter
  output$oregonRecPlot <- renderPlot({
    groundfishRec <- groundfish %>% 
      filter(Species == input$recSpeciesInput, 
             CombinedSector == "Recreational") %>%
      group_by(Year, Sector) %>%
      summarize(Landings = sum(Landings),
                TotalMortality = sum(TotalMortality),
                DiscardMortality = sum(DiscardMortality))
    oregonRec <- groundfishRec %>%
      filter(Sector == "Oregon Recreational") %>%
      pivot_longer(cols = c(Landings, DiscardMortality),
                   names_to = "Key",
                   values_to = "Value")
  #plot
    ggplot() +
      geom_col(data = groundfishRec, 
               aes(x = Year, y = TotalMortality, fill = "Total"), 
               width = 0.9) +
      geom_col(data = oregonRec, 
               aes(x = Year, y = Value, fill = Key), 
               width = 0.9) +
      scale_fill_manual(values = mortalityColors, 
                        labels = c("Total" = "All Recreational", 
                                   "DiscardMortality" = "Discard Mortality",
                                   "Landings" = "Landings")) +
      theme_classic() +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(expand = c(0,0), 
                         limits = c(2002-0.5, max(groundfish$Year)+0.5),
                         breaks = seq(2002, max(groundfish$Year), by = 1)) +
      labs(title = "Oregon Recreational Fishery",
           subtitle = paste(input$recSpeciesInput),
           y = "Total Mortality (mt)") +
      theme(plot.title = element_text(size = 18, color = "grey15"),
            plot.subtitle = element_text(size = 16, color = "grey15"),
            axis.title = element_text(size = 16, color = "grey15"),
            legend.title = element_text(size = 15, color = "grey15"),
            legend.text = element_text(size = 15, color = "grey15"),
            axis.text.x = element_text(size = 12, angle = 45, 
                                       hjust = 1, vjust = 1,
                                       color = "grey30"),
            axis.text.y = element_text(size = 12, color = "grey30"))
  })

  
  
#Make a plot for selected species in California recreational
  #filter
  output$californiaRecPlot <- renderPlot({
    groundfishRec <- groundfish %>% 
      filter(Species == input$recSpeciesInput, 
             CombinedSector == "Recreational") %>%
      group_by(Year, Sector) %>%
      summarize(Landings = sum(Landings),
                TotalMortality = sum(TotalMortality),
                DiscardMortality = sum(DiscardMortality))
    californiaRec <- groundfishRec %>%
      filter(Sector == "California Recreational") %>%
      pivot_longer(cols = c(Landings, DiscardMortality),
                   names_to = "Key",
                   values_to = "Value")
  #plot
    ggplot() +
      geom_col(data = groundfishRec, 
               aes(x = Year, y = TotalMortality, fill = "Total"), 
               width = 0.9) +
      geom_col(data = californiaRec, 
               aes(x = Year, y = Value, fill = Key), 
               width = 0.9) +
      scale_fill_manual(values = mortalityColors, 
                        labels = c("Total" = "All Recreational", 
                                   "DiscardMortality" = "Discard Mortality",
                                   "Landings" = "Landings")) +
      theme_classic() +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(expand = c(0,0), 
                         limits = c(2002-0.5, max(groundfish$Year)+0.5),
                         breaks = seq(2002, max(groundfish$Year), by = 1)) +
      labs(title = "California Recreational Fishery",
           subtitle = paste(input$recSpeciesInput),
           y = "Total Mortality (mt)") +
      theme(plot.title = element_text(size = 18, color = "grey15"),
            plot.subtitle = element_text(size = 16, color = "grey15"),
            axis.title = element_text(size = 16, color = "grey15"),
            legend.title = element_text(size = 15, color = "grey15"),
            legend.text = element_text(size = 15, color = "grey15"),
            axis.text.x = element_text(size = 12, angle = 45, 
                                       hjust = 1, vjust = 1,
                                       color = "grey30"),
            axis.text.y = element_text(size = 12, color = "grey30"))    
  })
  
  
  
#Create a link to the sector grouping table from the Mortality plots tab  
  observeEvent(input$gotoSectorTable, {
    updateNavbarPage(session, 
                     inputId = "Menu", 
                     selected = "Home")
    session$onFlushed(function() {
    updateTabsetPanel(session, 
                      inputId = "homeTab", 
                      selected = "SectorGroupings")
    })
  })
}
