# How do each of the files in this repository contribute to the Shiny App?


### In Prep Data folder:

#### GEMM Data.xlsx
> This is the unedited GEMM dataset published in 2026 (as of 7/27/2026). The GroundfishFilteringCode.R file reads in this dataset,
> particularly the catch and mortality data provided in "Table 3." Since the filtering code is based on this dataset, all subsequent
> GEMM datasets to be read into the filtering code and used in the Shiny App should be formatted in **the same way** (aka catch and
> mortality data on the third excel sheet with the table description taking up the first two rows of the sheet).

#### GroundfishFilteringCode.R
> This file filters the data for only federally managed species, as of 2026. This file also creates two combined fishery sector columns.
> One column is for slightly refined fishery sectors based on GEMM sector designation, the other is for higher level summary sectors.
> This file saves a new file named ShinyReadyData.csv that can be used in the Shiny App. The "Run Shiny" folder already has an accurate
> ShinyReadyData.csv as of July 2026. **However**, each year that a new GEMM dataset is released, that new data will need to be fed into
> this file and the new ShinyReadyData.csv will need to replace the old one in the "Run Shiny" folder.


### In Run Shiny folder:

#### manifest.json
> This file is used exclusively to deploy the Shiny App to NOAA's Posit Connect. This manifest includes information like the content’s
> environment dependencies, and tells Connect how to deploy and host the app. **If any of the files or code in this folder are changed
> a new manifest must be created to reflect these changes** This can be done by removing the current manifest.json and setting your 
> R working directory to Run Shiny, and running the following code in your console:
         rsconnect::writeManifest(
             appMode = "shiny",
             appPrimaryDoc = "server.R"
         )

#### ui.R
> This is the user interface portion of the Shiny App. All formatting that the user sees is set up in this file including page titles,
> dropdown selections, where plots appear, etc.

#### server.R
> This is the server portion of the Shiny App. Any selections that the user makes via buttons or dropdowns on the app are taken as an
> input and the server gives the coded output. All code for plots and tables is written here using the ShinyReadyData.csv.

#### ShinyReadyData.csv
> This is the filtered GEMM data that is created by running original GEMM data through GroundfishFilteringCode.R. This file should be
> replaced each time an updated GEMM dataset is released with a new year of data so that the app stays up to date.

#### ShinyAboutPage.Rmd
> This file sets up the about page for the Shiny App. It is used by ui.R to place it in the user interface. This was placed in a separate
> file so as not to not overcrowd the ui.R file.
