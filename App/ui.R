

# Think it needs these here for the server deployment
# shiny deployment looks for library(), so need that too in order to get the dependencies)
source("R/3c_HelperFunctions_ModelSetup.R")
source.modules("R/")
load_or_install(c("ggplot2","DT","markdown","rmarkdown","shiny","shinydashboard","shinyjqui","shinyFiles"))
library("ggplot2")
library("DT")
library("markdown")
library("rmarkdown")
library("shiny")
library("shinydashboard")
library("shinyjqui")
library("shinyFiles")
library("shinybusy")
library("shinyWidgets")








navbarPage("Harvest Strategy Types", id = "MainTab",


    tabPanel("Explore HCR Types", value= "Explore",


	pageWithSidebar(
	headerPanel("Explore HCR Types"),

	sidebarPanel(
	  selectizeInput("plot.type", "Display Type", choices = c("Rate","Spawners","Catch","Table"), selected="Rate"),
	  #shinyWidgets::autonumericInput(
	 #  	    inputId = "plot.lim",label = "Plot Range",value = 300,decimalPlaces = 0, digitGroupSeparator = ","),
	  numericInput("plot.lim", "Plot Range",  value = 300 ,   width = "100%"),
	  numericInput("run.med", "Run - Median",  value = 100 ,   width = "100%"),
	  numericInput("run.lower", "Run - Lower",  value = 70 ,   width = "100%"),
	  numericInput("run.upper", "Run - Upper",  value = 150 ,   width = "100%"),
	  numericInput("min.er", "MinER",  value = 0 ,  min = 0, max = 100, step = 1, width = "100%"),
	  selectizeInput("rate.type", "Rate Type", choices = c("ER","TotalMort"),selected = "ER"),
	  textInput("units.use","Unit Label",value ="(1000s of Fish)"),
	  textInput("run.label","Run Label",value ="In-season Run Estimate"),
    width = 2
		) # end sidebar
  ,


     mainPanel(


		 tabsetPanel(type = "tabs", id = "display.tab",
        tabPanel("Fixed Rate", value = "FixedRate", plotOutput("plot1",width = "100%", height = "600px"),
                 numericInput("fixed.rate", "Fixed Rate Target",  value = 50 , min = 0 , max = 100, step = 1,  width = "20%")
                 ),
        tabPanel("Fixed Spn", value = "FixedSpn", plotOutput("plot2",width = "100%", height = "600px"),
                 numericInput("fixed.spn", "Fixed Spawner Target",  value = 50 ,width = "20%")),
        tabPanel("Fixed Ct", value = "FixedCt", plotOutput("plot3",width = "100%", height = "600px"),
                          numericInput("fixed.ct", "Fixed Catch Target",  value = 50 ,width = "20%")
        )
        )





		) # end main panel

		) #end page with side bar for model pre-check



	),






	tabPanel("About",

fluidPage(

  titlePanel("About this app"),

  fluidRow(
    column(8,
      includeMarkdown("Markdown/about.md")
    )
  )
)
	  )  # end about tab panel



) # end navbar Page


