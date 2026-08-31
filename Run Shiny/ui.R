
library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)
library(shinythemes)
library(markdown)

fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  h1("U.S. West Coast Groundfish Mortality Visualization Tool"),
  br(), 
  br(),

  navbarPage(title = "Menu", id = "Menu",
             tabPanel(title = "Home", value = "Home",
                      tabsetPanel(id = "homeTab",
                        tabPanel(title = "About", 
                                 value = "About",
                                 includeMarkdown("ShinyAboutPage.Rmd")),
                        tabPanel(title = "Sector Groupings",
                                 value = "SectorGroupings",
                                 HTML("<div style='margin-left: 85px; margin-right: 85px;'>"),
                                 br(),
                                 br(),
                                 h4("To balance ease of visualization and sector differentiation, 
                                    we created two sector groupings. The 'Refined' sectors minimally group GEMM sectors, 
                                    showing more detail, which can be difficult to distinguish in some plots. 
                                    The 'Combined' sectors provide a higher-level summary that is easier to discern in plots."),
                                 br(),
                                 br(),
                                 dataTableOutput(outputId = "sectorTable"),
                                 HTML("</div>")))),
             
             tabPanel(title = "Species Mortality by Sector", value = "SpeciesMortality",
                      h3("Getting started:"),
                      h5("1. Select a species of interest to see which fishery sectors contribute to total mortality."),
                      h5("2. The", em("Combined Sector Mortality Plot"), "categorizes sectors into broader groupings, defined", 
                         actionLink(inputId = "gotoSectorTable", label = "here"), "."),
                      h5("3. For trends in particular sectors, select up to four and navigate to the",
                         em("Individuals Sector Plots"), "tab."),
                      HTML("<hr style='border: none; height: 0.7px; background-color: #B4CFEC;'/>"),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "speciesInput", label = "Species (Select One)", 
                                      choices = c("Arrowtooth Flounder", "Aurora Rockfish", "Bank Rockfish",
                                                  "Big Skate", "Black Rockfish (Washington only)" = "Black Rockfish", 
                                                  "Blackgill Rockfish", "Bocaccio Rockfish",
                                                  "California Scorpionfish", "Canary Rockfish", "Chilipepper Rockfish",
                                                  "Cowcod Rockfish", "Darkblotched Rockfish", "Dover Sole", "English Sole",
                                                  "Flathead Sole", "Greenspotted Rockfish", "Greenstriped Rockfish",
                                                  "Lingcod", "Longnose Skate", "Longspine Thornyhead", 
                                                  "Mixed Thornyheads" = "Shortspine/Longspine Thornyhead", "Pacific Cod",
                                                  "Pacific Hake", "Pacific Ocean Perch Rockfish", "Pacific Sanddab", "Pacific Spiny Dogfish", 
                                                  "Petrale Sole", "Quillback Rockfish (Washington/Oregon)", "Redbanded Rockfish", 
                                                  "Redstripe Rockfish", "Rex Sole",
                                                  "Rosethorn Rockfish", "Rougheye/Blackspotted Rockfish", "Sablefish", 
                                                  "Sharpchin Rockfish", "Shortraker Rockfish", "Shortspine Thornyhead", 
                                                  "Silvergray Rockfish", "Slope Rockfish (Unidentified)" = "Slope Rockfish Unid", 
                                                  "Splitnose Rockfish", "Squarespot Rockfish", "Starry Rockfish", 
                                                  "Stripetail Rockfish", "Vermilion Rockfish", "Widow Rockfish", "Yelloweye Rockfish", 
                                                  "Yellowmouth Rockfish", "Yellowtail Rockfish")),
                          br(),
                          selectizeInput(inputId = "sectorInput", label = "Fishery Sector (Select up to Four)",
                                         choices = c("At-Sea Hake", "CA Halibut", "CS - Bottom Trawl", "CS - Hook & Line", 
                                                     "CS - Pot","Directed Pacific Halibut" = "Directed P Halibut", 
                                                     "Directed Open Access Groundfish", "Incidental", 
                                                     "Limited Entry Non Trawl", "Limited Entry Trawl", 
                                                     "Midwater Hake", "Midwater Rockfish", "Nearshore", "Pink Shrimp", 
                                                     "Recreational", "Research", "Tribal Shoreside"),
                                         multiple = TRUE,
                                           options = list(maxItems=4)),
                          p("To view selected sector(s), go to",em("Individual Sector Plots"), "tab."),
                          br(),
                          br(),
                          downloadButton("downloadSpeciesData", 
                                         "Download Data for this Species and Sector(s)"),
                          p("This will download as a .csv file. All sectors will be included if none are selected above.")
                          ),
                      
                        mainPanel(
                          tabsetPanel(
                            tabPanel(title = "Refined Sector Mortality Plot", 
                                     br(),
                                     plotOutput(outputId = "allSectorsPlot"),
                                     br(), br()),
                            tabPanel(title = "Combined Sector Mortality Plot",
                                     br(),
                                     plotOutput(outputId = "comboSectorsPlot"),
                                     br(), br()),
                            tabPanel(title = "Individual Sector Plots", 
                                     br(),
                                     uiOutput(outputId = "conditionalMessage"),
                                     br(),
                                     uiOutput(outputId = "individualSectorPlots"),
                                     br(), br())
                          )
                        )
                      )
           ),
           tabPanel("Recreational Sector Only", value = "RecreationalSector",
                    h3("Getting started:"),
                    h5("1. Select a species of interest to see states' participation in the recreational fishery."),
                    h5("2. To view the breakdown of landings and discards for each state, navigate to the", 
                       em("Individual State Plots"), "tab."),
                    HTML("<hr style='border: none; height: 0.7px; background-color: #B4CFEC;'/>"),
                    br(),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(inputId = "recSpeciesInput", label = "Species (Select One)",
                                    choices = c("Arrowtooth Flounder", "Aurora Rockfish", "Bank Rockfish",
                                                "Big Skate", "Black Rockfish (Washington only)" = "Black Rockfish", 
                                                "Blackgill Rockfish", "Bocaccio Rockfish",
                                                "California Scorpionfish", "Canary Rockfish", "Chilipepper Rockfish",
                                                "Cowcod Rockfish", "Darkblotched Rockfish", "Dover Sole", "English Sole",
                                                "Flathead Sole", "Greenspotted Rockfish", "Greenstriped Rockfish",
                                                "Lingcod", "Longnose Skate", "Longspine Thornyhead", 
                                                "Mixed Thornyheads" = "Shortspine/Longspine Thornyhead", "Pacific Cod",
                                                "Pacific Hake", "Pacific Ocean Perch Rockfish", "Pacific Sanddab", "Pacific Spiny Dogfish", 
                                                "Petrale Sole", "Quillback Rockfish (Washington/Oregon)", "Redbanded Rockfish", 
                                                "Redstripe Rockfish", "Rex Sole",
                                                "Rosethorn Rockfish", "Rougheye/Blackspotted Rockfish", "Sablefish", 
                                                "Sharpchin Rockfish", "Shortraker Rockfish", "Shortspine Thornyhead", 
                                                "Silvergray Rockfish", "Slope Rockfish (Unidentified)" = "Slope Rockfish Unid", 
                                                "Splitnose Rockfish", "Squarespot Rockfish", "Starry Rockfish", 
                                                "Stripetail Rockfish", "Vermilion Rockfish", "Widow Rockfish", "Yelloweye Rockfish", 
                                                "Yellowmouth Rockfish", "Yellowtail Rockfish")),
                        p(em("Blank plots indicate no recreational fishery data."))),

                      mainPanel(
                        tabsetPanel(
                          tabPanel("Recreational Mortality", 
                                   br(),
                                   plotOutput("recMortalityPlot"),
                                   br()),
                          tabPanel("Individual State Plots", 
                                   br(), 
                                   plotOutput("washingtonRecPlot"),
                                   br(),
                                   plotOutput("oregonRecPlot"),
                                   br(), 
                                   plotOutput("californiaRecPlot"),
                                   br())
                        )
                      )
                    )
           ) 
  )
)         
