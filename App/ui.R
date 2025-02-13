

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

           tabPanel("About",

                    fluidPage(

                      titlePanel("App to Explore Salmon Harvest Strategy Types"),

                      fluidRow(
                        column(10,
                               includeMarkdown("Markdown/about.md")
                        )
                      )
                    )
           ),  # end about tab panel


    tabPanel("Explore HCR Types", value= "Explore",


	pageWithSidebar(
	headerPanel("Explore HCR Types"),

	sidebarPanel(
	  selectizeInput("plot.type", "Display Type", choices = c("Rate","Spawners","Catch","Table"), selected="Rate"),
	  #shinyWidgets::autonumericInput(
	 #  	    inputId = "plot.lim",label = "Plot Range",value = 300,decimalPlaces = 0, digitGroupSeparator = ","),
	  numericInput("plot.lim", "Plot Range",  value = 600 ,   width = "100%"),
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
        tabPanel("Fixed Spawners", value = "FixedSpn", plotOutput("plot2",width = "100%", height = "600px"),
                 numericInput("fixed.spn", "Fixed Spawner Target",  value = 50 ,width = "20%")),
        tabPanel("Fixed Catch", value = "FixedCt", plotOutput("plot3",width = "100%", height = "600px"),
                 numericInput("fixed.ct", "Fixed Catch Target",  value = 50 ,width = "20%")),
        tabPanel("Stepped ER", value = "Step",
                 fluidRow(column(12,plotOutput("plot4",width = "100%", height = "600px"))),
                 fluidRow(column(2,numericInput("step1.rp", "Step 1 Ref Pt",  value = 20 ,width = "100%")),
                          column(2,numericInput("step1.rate", "Step 1 Rate",  value = 30 ,width = "100%")),
                          column(2,numericInput("step2.rp", "Step 2 Ref Pt",  value = 50 ,width = "100%")),
                          column(2,numericInput("step2.rate", "Step 2 Rate",  value = 50 ,width = "100%")),
                          column(2,numericInput("step3.rp", "Step 3 Ref Pt",  value = 170 ,width = "100%")),
                          column(2,numericInput("step3.rate", "Step 3 Rate",  value = 70 ,width = "100%"))
                )# end fluid row
        ), # end tabpanel
        tabPanel("Sloped ER", value = "SlopedER",
                 fluidRow(column(12,plotOutput("plot9",width = "100%", height = "600px"))),
                 splitLayout(numericInput("slopeder.rp1", "RP1",  value = 50 ,width = "100%"),
                             numericInput("slopeder.rate1", "Rate1",  value = 0 ,width = "100%"),
                             numericInput("slopeder.rp2", "RP2",  value = 150 ,width = "100%"),
                             numericInput("slopeder.rate2", "Rate2",  value = 25 ,width = "100%"),
                             numericInput("slopeder.rp3", "RP3",  value = 300 ,width = "100%"),
                             numericInput("slopeder.rate3", "Rate3",  value = 30 ,width = "100%"),
                             numericInput("slopeder.rp4", "RP4",  value = 400 ,width = "100%"),
                             numericInput("slopeder.rate4", "Rate4",  value = 45 ,width = "100%"),
                             cellWidths = "10%")
        ), # end tabpanel
        tabPanel("Stepped Spawners", value = "StepSpn",
                 fluidRow(column(12,plotOutput("plot7",width = "100%", height = "600px"))),
                 fluidRow(column(2,numericInput("stepspn1.rp", "Step 1 Ref Pt",  value = 50 ,width = "100%")),
                          column(2,numericInput("stepspn1.target", "Step 1 Spn",  value = 50 ,width = "100%")),
                          column(2,numericInput("stepspn2.rp", "Step 2 Ref Pt",  value = 300 ,width = "100%")),
                          column(2,numericInput("stepspn2.target", "Step 2 Spn",  value = 150 ,width = "100%"))
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
        ), # end tabpanel
        tabPanel("Field Hockey Stick with ER Floor", value = "FieldHockeyStickwFloor",
                 fluidRow(column(12,plotOutput("plot8",width = "100%", height = "600px"))),
                 fluidRow(column(3,numericInput("fieldwfloor.rp1", "Spn Target",  value = 50 ,width = "100%")),
                          column(3,numericInput("fieldwfloor.rate", "Cap on Rate",  value = 65 ,width = "100%")),
                          column(3,numericInput("fieldwfloor.floor", "Min ER",  value = 10 ,width = "100%"))
                 )# end fluid row
        ) # end tabpanel
		 ) # end tabset panel


		) # end main panel

		) #end page with side bar for model pre-check



	), # end Explore HCR tab panel



    tabPanel("Compare 2 HCR", value= "Compare",


	pageWithSidebar(
	headerPanel("Compare 2 HCR"),

	sidebarPanel(
	  selectizeInput("plot.type.comp", "Display Type", choices = c("Rate","Spawners","Catch"), selected="Rate"),
	  #shinyWidgets::autonumericInput(
	 #  	    inputId = "plot.lim",label = "Plot Range",value = 300,decimalPlaces = 0, digitGroupSeparator = ","),
	  numericInput("plot.lim.comp", "Plot Range",  value = 600 ,   width = "100%"),
	  numericInput("run.med.comp", "Run - Median",  value = 100 ,   width = "100%"),
	  numericInput("run.lower.comp", "Run - Lower (Deduct %)",  value = 15,  min =0,max=100, width = "100%"),
	  numericInput("run.upper.comp", "Run - Upper (Add %)",  value = 30 , min= 0, max = 300,  width = "100%"),
	  #numericInput("min.er", "MinER",  value = 0 ,  min = 0, max = 100, step = 1, width = "100%"),
	  selectizeInput("rate.type.comp", "Rate Type", choices = c("ER","TotalMort"),selected = "ER"),
	  textInput("units.use.comp","Unit Label",value ="(1000s of Fish)"),
	  textInput("run.label.comp","Run Label",value ="In-season Run Estimate"),
    width = 2
		) # end sidebar
  ,


     mainPanel(


	tabsetPanel(type = "tabs", id = "display.tab.comp",
        tabPanel("HCR 1", value = "HCR 1",

       textInput("hcr1.label","HCR 1 Label",value ="HCR 1"),
      selectizeInput("hcr1.type", "Harvest Rule Type", choices = c("FixedRate",
			   "FixedSpn","StepSpn","FixedCt","Step","SlopedER","IceHockeyStick","FieldHockeyStick",
			   "FieldHockeyStickwFloor"),selected="FixedSpn"),

			conditionalPanel(condition = "input['hcr1.type'] == 'FixedRate'",
		                 numericInput("fixed.rate.hcr1", "Fixed Rate Target",  value = 50 , min = 0 , max = 100, step = 1)
                 ),
			conditionalPanel(condition = "input['hcr1.type'] == 'FixedSpn'",
		                 numericInput("fixed.spn.hcr1", "Fixed Spawner Target",  value = 70 , step = 1)
                 ),

			conditionalPanel(condition = "input['hcr1.type'] == 'StepSpn'",
			                 fluidRow(column(2,numericInput("stepspn1.rp.hcr1", "Step 1 Ref Pt",  value = 50 ,width = "100%")),
			                          column(2,numericInput("stepspn2.rp.hcr1", "Step 2 Ref Pt",  value = 300 ,width = "100%"))
			                 ),
			                 fluidRow(column(2,numericInput("stepspn1.target.hcr1", "Step 1 Spn",  value = 50 ,width = "100%")),
			                          column(2,numericInput("stepspn2.target.hcr1", "Step 2 Spn",  value = 150 ,width = "100%"))
			                 )
			),

			conditionalPanel(condition = "input['hcr1.type'] == 'FixedCt'",
			                 numericInput("fixed.ct.hcr1", "Fixed Catch Target",  value = 50 ,width = "20%")
			),


			conditionalPanel(condition = "input['hcr1.type'] == 'Step'",
			                 fluidRow(column(2,numericInput("step1.rp.hcr1", "Step 1 Ref Pt",  value = 20 ,width = "100%")),
			                          column(2,numericInput("step2.rp.hcr1", "Step 2 Ref Pt",  value = 50 ,width = "100%")),
			                          column(2,numericInput("step3.rp.hcr1", "Step 3 Ref Pt",  value = 170 ,width = "100%"))
			                 ),
			                 fluidRow(column(2,numericInput("step1.rate.hcr1", "Step 1 Rate",  value = 30 ,width = "100%")),
			                          column(2,numericInput("step2.rate.hcr1", "Step 2 Rate",  value = 50 ,width = "100%")),
			                          column(2,numericInput("step3.rate.hcr1", "Step 3 Rate",  value = 70 ,width = "100%"))
			                 )

			),

			conditionalPanel(condition = "input['hcr1.type'] == 'SlopedER'",
			                 splitLayout(numericInput("slopeder.rp1.hcr1", "RP1",  value = 50 ,width = "100%"),
			                             numericInput("slopeder.rp2.hcr1", "RP2",  value = 150 ,width = "100%"),
			                             numericInput("slopeder.rp3.hcr1", "RP3",  value = 300 ,width = "100%"),
			                             numericInput("slopeder.rp4.hcr1", "RP4",  value = 400 ,width = "100%"),
			                             cellWidths = "10%"),
			                 splitLayout(numericInput("slopeder.rate1.hcr1", "Rate1",  value = 0 ,width = "100%"),
			                             numericInput("slopeder.rate2.hcr1", "Rate2",  value = 25 ,width = "100%"),
			                             numericInput("slopeder.rate3.hcr1", "Rate3",  value = 30 ,width = "100%"),
			                             numericInput("slopeder.rate4.hcr1", "Rate4",  value = 45 ,width = "100%"),
			                             cellWidths = "10%")
          ),
			conditionalPanel(condition = "input['hcr1.type'] == 'IceHockeyStick'",
			                 fluidRow(column(2,numericInput("ice.rp1.hcr1", "Lower Ref Pt",  value = 20 ,width = "100%")),
			                          column(2,numericInput("ice.rp2.hcr1", "Upper Ref Pt",  value = 50 ,width = "100%")),
			                          column(2,numericInput("ice.rate.hcr1", "Cap on Rate",  value = 50 ,width = "100%"))
			                 )
			),
			conditionalPanel(condition = "input['hcr1.type'] == 'FieldHockeyStick'",
			                 fluidRow(column(2,numericInput("field.rp1.hcr1", "Spn Target",  value = 50 ,width = "100%")),
			                          column(2,numericInput("field.rate.hcr1", "Cap on Rate",  value = 65 ,width = "100%"))
			                 )
			),
			conditionalPanel(condition = "input['hcr1.type'] == 'FieldHockeyStickwFloor'",
			                 fluidRow(column(2,numericInput("fieldwfloor.rp1.hcr1", "Spn Target",  value = 50 ,width = "100%")),
			                          column(2,numericInput("fieldwfloor.rate.hcr1", "Cap on Rate",  value = 65 ,width = "100%")),
			                          column(2,numericInput("fieldwfloor.floor.hcr1", "Min ER",  value = 10 ,width = "100%"))
			                 )
			),


			selectizeInput("hcr1.line.col", "Line Color", choices = c("darkblue","red","darkgrey"),
			               selected="darkgrey"),
			selectizeInput("hcr1.line.type", "Line Type", choices = 1:4,
			               selected=1)

			) , # end HCR 1 tab panel

        tabPanel("HCR 2", value = "HCR 2",

       textInput("hcr2.label","HCR 2 Label",value ="HCR 2"),


       selectizeInput("hcr2.type", "Harvest Rule Type", choices = c("FixedRate",
                                                                    "FixedSpn","StepSpn","FixedCt","Step","SlopedER","IceHockeyStick","FieldHockeyStick",
                                                                    "FieldHockeyStickwFloor"),
                      selected="FieldHockeyStickwFloor"),

       conditionalPanel(condition = "input['hcr2.type'] == 'FixedRate'",
                        numericInput("fixed.rate.hcr2", "Fixed Rate Target",  value = 50 , min = 0 , max = 100, step = 1)
       ),
       conditionalPanel(condition = "input['hcr2.type'] == 'FixedSpn'",
                        numericInput("fixed.spn.hcr2", "Fixed Spawner Target",  value = 50 )
       ),

       conditionalPanel(condition = "input['hcr2.type'] == 'StepSpn'",
                        fluidRow(column(2,numericInput("stepspn1.rp.hcr2", "Step 1 Ref Pt",  value = 50 ,width = "100%")),
                                 column(2,numericInput("stepspn2.rp.hcr2", "Step 2 Ref Pt",  value = 300 ,width = "100%"))
                        ),
                        fluidRow(column(2,numericInput("stepspn1.target.hcr2", "Step 1 Spn",  value = 50 ,width = "100%")),
                                 column(2,numericInput("stepspn2.target.hcr2", "Step 2 Spn",  value = 150 ,width = "100%"))
                        )
       ),

       conditionalPanel(condition = "input['hcr2.type'] == 'FixedCt'",
                        numericInput("fixed.ct.hcr2", "Fixed Catch Target",  value = 50 ,width = "20%")
       ),


       conditionalPanel(condition = "input['hcr2.type'] == 'Step'",
                        fluidRow(column(2,numericInput("step1.rp.hcr2", "Step 1 Ref Pt",  value = 20 ,width = "100%")),
                                 column(2,numericInput("step2.rp.hcr2", "Step 2 Ref Pt",  value = 50 ,width = "100%")),
                                 column(2,numericInput("step3.rp.hcr2", "Step 3 Ref Pt",  value = 170 ,width = "100%"))
                        ),
                        fluidRow(column(2,numericInput("step1.rate.hcr2", "Step 1 Rate",  value = 30 ,width = "100%")),
                                 column(2,numericInput("step2.rate.hcr2", "Step 2 Rate",  value = 50 ,width = "100%")),
                                 column(2,numericInput("step3.rate.hcr2", "Step 3 Rate",  value = 70 ,width = "100%"))
                        )

       ),

       conditionalPanel(condition = "input['hcr2.type'] == 'SlopedER'",
                        splitLayout(numericInput("slopeder.rp1.hcr2", "RP1",  value = 50 ,width = "100%"),
                                    numericInput("slopeder.rp2.hcr2", "RP2",  value = 150 ,width = "100%"),
                                    numericInput("slopeder.rp3.hcr2", "RP3",  value = 300 ,width = "100%"),
                                    numericInput("slopeder.rp4.hcr2", "RP4",  value = 400 ,width = "100%"),
                                    cellWidths = "10%"),
                        splitLayout(numericInput("slopeder.rate1.hcr2", "Rate1",  value = 0 ,width = "100%"),
                                    numericInput("slopeder.rate2.hcr2", "Rate2",  value = 25 ,width = "100%"),
                                    numericInput("slopeder.rate3.hcr2", "Rate3",  value = 30 ,width = "100%"),
                                    numericInput("slopeder.rate4.hcr2", "Rate4",  value = 45 ,width = "100%"),
                                    cellWidths = "10%")
       ),
       conditionalPanel(condition = "input['hcr2.type'] == 'IceHockeyStick'",
                        fluidRow(column(2,numericInput("ice.rp1.hcr2", "Lower Ref Pt",  value = 20 ,width = "100%")),
                                 column(2,numericInput("ice.rp2.hcr2", "Upper Ref Pt",  value = 50 ,width = "100%")),
                                 column(2,numericInput("ice.rate.hcr2", "Cap on Rate",  value = 50 ,width = "100%"))
                        )
       ),
       conditionalPanel(condition = "input['hcr2.type'] == 'FieldHockeyStick'",
                        fluidRow(column(2,numericInput("field.rp1.hcr2", "Spn Target",  value = 50 ,width = "100%")),
                                 column(2,numericInput("field.rate.hcr2", "Cap on Rate",  value = 65 ,width = "100%"))
                        )
       ),
       conditionalPanel(condition = "input['hcr2.type'] == 'FieldHockeyStickwFloor'",
                        fluidRow(column(2,numericInput("fieldwfloor.rp1.hcr2", "Spn Target",  value = 50 ,width = "100%")),
                                 column(2,numericInput("fieldwfloor.rate.hcr2", "Cap on Rate",  value = 65 ,width = "100%")),
                                 column(2,numericInput("fieldwfloor.floor.hcr2", "Min ER",  value = 10 ,width = "100%"))
                        )
       ),

			selectizeInput("hcr2.line.col", "Line Color", choices = c("darkblue","red","darkgrey"),
			               selected="darkblue"),
			selectizeInput("hcr2.line.type", "Line Type", choices = 1:4,
			               selected=2)
                 ),
        tabPanel("Plot", value = "Plot", plotOutput("plotComp",width = "100%", height = "600px")
                 ),
		 ) # end tabset panel


		) # end main panel

		) #end page with side bar for model pre-check



	) # End COmpare 2 HCR









) # end navbar Page


