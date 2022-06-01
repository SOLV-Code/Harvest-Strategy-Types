

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
library("gridExtra")







navbarPage("Harvest Strategy Types", id = "MainTab",


    tabPanel("Explore HCR Types", value= "Explore",


	pageWithSidebar(
	headerPanel("Explore HCR Types"),

	sidebarPanel(
	  selectizeInput("plot.type", "Display Type", choices = c("Rate","Spawners","Catch","Table"), selected="Rate"),
	  #shinyWidgets::autonumericInput(
	 #  	    inputId = "plot.lim",label = "Plot Range",value = 300,decimalPlaces = 0, digitGroupSeparator = ","),
	  numericInput("plot.lim", "Plot Range",  value = 200 ,   width = "100%"),
	  numericInput("run.med", "Run - Median",  value = 100 ,   width = "100%"),
	  numericInput("run.lower", "Run - Lower (Deduct %)",  value = 15,  min =0,max=100, width = "100%"),
	  numericInput("run.upper", "Run - Upper (Add %)",  value = 30 , min= 0, max = 300,  width = "100%"),
	  #numericInput("min.er", "MinER",  value = 0 ,  min = 0, max = 100, step = 1, width = "100%"),
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
                 numericInput("fixed.ct", "Fixed Catch Target",  value = 50 ,width = "20%")),
        tabPanel("Step", value = "Step",
                 fluidRow(column(12,plotOutput("plot4",width = "100%", height = "600px"))),
                 fluidRow(column(2,numericInput("step1.rp", "Step 1 Ref Pt",  value = 20 ,width = "100%")),
                          column(2,numericInput("step1.rate", "Step 1 Rate",  value = 30 ,width = "100%")),
                          column(2,numericInput("step2.rp", "Step 2 Ref Pt",  value = 50 ,width = "100%")),
                          column(2,numericInput("step2.rate", "Step 2 Rate",  value = 50 ,width = "100%")),
                          column(2,numericInput("step3.rp", "Step 3 Ref Pt",  value = 170 ,width = "100%")),
                          column(2,numericInput("step3.rate", "Step 3 Rate",  value = 70 ,width = "100%"))
                )# end fluid row
        ), # end tabpanel
        tabPanel("Ice Hockey Stick", value = "IceHockeyStick",
                 fluidRow(column(12,plotOutput("plot5",width = "100%", height = "600px"))),
                 fluidRow(column(3,numericInput("ice.rp1", "Lower Ref Pt",  value = 20 ,width = "100%")),
                          column(3,numericInput("ice.rp2", "Upper Ref Pt",  value = 50 ,width = "100%")),
                          column(3,numericInput("ice.rate", "Cap on Rate",  value = 50 ,width = "100%"))
                 )# end fluid row
        ), # end tabpanel
        tabPanel("Field Hockey Stick", value = "FieldHockeyStick",
                 fluidRow(column(12,plotOutput("plot6",width = "100%", height = "600px"))),
                 fluidRow(column(3,numericInput("field.rp1", "Spn Target",  value = 50 ,width = "100%")),
                          column(3,numericInput("field.rate", "Cap on Rate",  value = 65 ,width = "100%"))
                 )# end fluid row
        ) # end tabpanel
		 ) # end tabset panel


		) # end main panel

		) #end page with side bar for model pre-check



	),






	tabPanel("About",

fluidPage(

  titlePanel("About this app"),

  fluidRow(
    column(10,
      includeMarkdown("Markdown/about.md")
    )
  )
)
	  )  # end about tab panel



) # end navbar Page


