# THIS IS THE SCRIPT PACKS UP THE APP AND SENDS IT TO THE SHINY SERVER

# RESOURCES
#https://support.rstudio.com/hc/en-us/articles/229848967-Why-does-my-app-work-locally-but-not-on-shinyapps-io-


# functions to load/install required packages
load_or_install <- function(package_names){
 for(package_name in package_names){
                if(!is_installed(package_name)){install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")}
 library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
 }
}

is_installed <- function(mypkg){ is.element(mypkg, installed.packages()[,1])  }

# load/install functions needed for shiny dashboard
load_or_install(c("rsconnect","shiny","shinydashboard","shinyjqui","shiny","shinydashboard","DT","ggplot2","shinyFiles"))



# run the deployment
rsconnect::deployApp(appDir="App", appTitle="HarvestStrategyTypes")

rsconnect::showLogs(appPath="App", appFile="HarvestStrategyTypes")





