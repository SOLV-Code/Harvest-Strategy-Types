

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









navbarPage("Harvest Strategy Types", id = "MainTab",


	 tabPanel("Disclaimer",

fluidPage(

  titlePanel("Disclaimer"),

  fluidRow(
    column(8,
	  includeMarkdown("Markdown/disclaimer.md")
    )
  )
)



	  ),  # end disclaimer tab panel






#######
tabPanel("General Settings", value= "general.settings",

				 tags$h4("Data Treatment Settings"),
				 	checkboxInput("cov.rescale", label="SibReg Complex: Rescale Covariates?", value = TRUE ),
				 tags$h4("Display Settings"),
				 numericInput("table.decimals", label=h5("Number of Decimals shown in tables and figures (NOT YET LINKED)"),
				 						 value = 0 , min = 0, max = 10, step = 1,   width = "40%"),
				 textInput("axis.label", label=h5("Forecasted Variable"), value = "Abundance", width = "40%"),
				 checkboxInput("show.equ","Show model equations in figures (not linked yet)",value=FALSE)
				 #uiOutput("axis.label.sel")

),  # end  general settings panel







#################### MODEL PRE CHECK ######################################


    tabPanel("Explore", value= "precheck",

             
	pageWithSidebar(
	headerPanel("Explore Models"),

	sidebarPanel(
	  add_busy_spinner(spin = "fading-circle", position = "full-page"),
		uiOutput("model.menu.precheck"),
		tags$hr(style = "border-top: 1px solid #000000;"),
		conditionalPanel(condition = "input['model.use.precheck'] == 'ReturnRate'",
										 uiOutput("pred.var.precheck.menu"),
										 selectizeInput("rate.avg.precheck", "Rate: Avg", choices = c("wtmean","mean", "median"), selected="wtmean"),
										 numericInput("last.n.precheck", "Rate: Last n obs",  value = 100 , min = 1, max = 100, step = 1,   width = "50%")
		), # end conditional panel for return rate
		conditionalPanel(condition = "input['model.use.precheck'] == 'TimeSeriesArima' || input['model.use.precheck'] == 'TimeSeriesExpSmooth'",
										 uiOutput("boxcox.precheck.menu")
		),
		conditionalPanel(condition = "input['model.use.precheck'] == 'SibRegKalman'",
										 uiOutput("intavg.precheck.menu")
		),
		conditionalPanel(condition = "input['model.use.precheck'] == 'SibRegComplex'",
										 uiOutput("complex.precheck.menu3"),
										 uiOutput("complex.precheck.menu1"),
										 uiOutput("complex.precheck.menu2")

		),
		conditionalPanel(condition = "input['model.use.precheck'] == 'SibRegPooledSimple' || input['model.use.precheck'] == 'SibRegPooledLogPower'",
										 uiOutput("max.pool.precheck.menu")
		),
		conditionalPanel(condition = "input['model.use.precheck'] == 'Naive'",
										 uiOutput("avgyrs.precheck.menu")
		),

		#numericInput("fc.yr", "FC Year", value=2018),  # comes from data file for now
		# slider below is for now changed to only give start year, then add the end year as 1-fc.yr on the server side
		tags$hr(style = "border-top: 1px solid #000000;"),
		uiOutput("ages.menu.precheck"),
		sliderInput("yr.range.precheck", "Start (Run Years)",sep="",min = 1960, max = 2020, value = 1975,animate=TRUE),
		tags$hr(style = "border-top: 1px solid #000000;"),
		selectizeInput("interval.type.precheck", "Interval Type", choices = c("Retrospective","Prediction","Bootstrap"), selected="Retrospective"),
		sliderInput("min.retroyrs.explore", "Min Yrs for Retro", sep="",min = 5, max = 35, value = 15,animate=FALSE),
		numericInput("boot.n.precheck", "Interval Sample",  value = 100 , min = 10, max = 1000, step = 10,   width = "50%"),
		selectizeInput("boot.type.precheck", "Bootstrap Type", choices = c("meboot","stlboot"), selected="meboot"),
		tags$hr(style = "border-top: 1px solid #000000;"),
		downloadButton("downloadPreCheckRep", "Download PDf report")
		#actionButton("create.precheck.summary.withoutage", "Create PDF Report")

		) # end sidebar
  ,


     mainPanel(


		 tabsetPanel(type = "tabs",
                  tabPanel("Fits & Point Forecast", plotOutput("precheck.plot.fitandfc",width = "100%", height = "600px")),
				  tabPanel("Forecast Plot", plotOutput("precheck.plot.intervals",width = "100%", height = "600px")),
				  tabPanel("Forecast Table",   DT::dataTableOutput("table.explore.fc")#,
							#downloadButton("download.table.explore.fc","Download")
							),
				tabPanel("Diagnostics",
				tabsetPanel(type = "tabs",
				  tabPanel("Obs vs. Fitted",plotOutput("precheck.plot.fitvsobs",width = "100%", height = "600px") ),
				  tabPanel("Residual vs. Fitted",plotOutput("precheck.plot.residvsfitted",width = "100%", height = "600px") ),
				  tabPanel("Residual Pattern",plotOutput("precheck.plot.resid_ts",width = "100%", height = "600px") ),
                  tabPanel("Residual Histogram",plotOutput("precheck.plot.resid_hist",width = "100%", height = "600px") ),
				  tabPanel("Residual QQ Norm",plotOutput("precheck.plot.resid_qq",width = "100%", height = "600px") ),
				  tabPanel("Model-Specific",
				  				 h4("Only works for Model type = SibRegKalman" , align = "left"),
				  				 plotOutput("precheck.modeldiagnostic",width = "100%", height = "600px") ),
				  #conditionalPanel(condition = "input['model.use.precheck'] == 'SibRegComplex'",
				  						 tabPanel("Model Selection",
										  h4("Only works for Model type = SibRegComplex" , align = "left"),
				  						 				 uiOutput("ages.menu.model.selection"),
				  						 				 DT::dataTableOutput("table.explore.model.selection")#,
				  								 				# downloadButton("download.explore.model.selection","Download")
				  						 				 )
				  					#))
						)),
  				  tabPanel("Bootstrapped Series",plotOutput("precheck.plot.boots.sample",width = "100%", height = "600px") )
				  )




		) # end main panel

		) #end page with side bar for model pre-check



	),






####################################



	 tabPanel("Compare" , value= "compare",

	pageWithSidebar(
	headerPanel("Compare Models"),

	sidebarPanel(width = 3,

		# https://stackoverflow.com/questions/43973863/keep-datatable-sort-between-tabs-in-shiny
		# try to link table sorting to the plot order

		#actionButton("addmodel.compare", "x Add a Model"),
		#actionButton("resetmodel.compare", "x Reset Models"),
		add_busy_spinner(spin = "fading-circle"),
		tags$p("Note: Running a new batch of models takes a little time"),
		selectizeInput("compare.ageclass", "Age Class", choices = ages.menu.list.multi, selected=ages.menu.list[1]),
		selectizeInput("compare.plotsort", "Sort Plot By", choices = c("AvgRank","Forecast"), selected="AvgRank"),
		selectizeInput("retrotype.compare", "Performance Measure Type", choices = retro.types, selected=retro.types[1]),
		selectizeInput("interval.type.compare", "Interval Type", choices = c("Retrospective","Prediction","Bootstrap"), selected="Retrospective"),
		numericInput("boot.n.compare", "Interval Sample",  value = 100 , min = 10, max = 1000, step = 10,   width = "50%"),
		selectizeInput("boot.type.compare", "Bootstrap Type", choices = c("meboot","stlboot"), selected="meboot"),
		#numericInput("fc.yr", "FC Year", value=2018),  # comes from data file for now
		# slider below is for now changed to only give start year, then add the end year as 1-fc.yr on the server side
		sliderInput("yr.range.compare", "Start (Run Years)", sep="",min = 1960, max = 2020, value = 1975,animate=FALSE),
		sliderInput("min.retroyrs.compare", "Min Yrs for Retro", sep="",min = 5, max = 35, value = 15,animate=FALSE),
		checkboxGroupInput("compare.pm", label="Perf. Measures for Model Ranking",
				choices=c("MRE","MAE","MPE","MAPE","MASE","RMSE")   ,
					selected = c("MRE","MAE","MPE","MAPE","MASE","RMSE") , inline = TRUE),
		checkboxInput("rel.bol","Use Scaled Ranking",value=FALSE)
		#downloadButton("downloadComparisonTxt", "x Download text file"),
		#downloadButton("downloadComparisonCsv", "x Download csv files"),
		#downloadButton("downloadComparisonRep", "x Download pdf report"),

		#actionButton("create.precheck.summary.withoutage", "Create PDF Report")



		) # end sidebar
  ,


     mainPanel(


		 tabsetPanel(type = "tabs",

				  tabPanel("Settings",
							tabsetPanel(type = "tabs",
								tabPanel("N1",
												 tags$h4("Naive Model"),
												 tags$h5("All Data Types"),
												checkboxInput("m1.use","Include this model",value=TRUE),
											  textInput("m1.name", "Model Label", value = "Naive3", width = "40%"),
											  selectizeInput("m1.modeltype", "Model Type", choices = "Naive", selected="Naive", width = "40%"),
											  numericInput("m1.avgyrs", label=h5("Avg Years (Naive Models)"), value = 3 , min = 1, max = 10, step = 1,   width = "40%"),
												#),
												#column(6,
												# checkboxInput("m1.boxcox","Box-Cox Transf. - Time Series Models",value=FALSE),
											 # numericInput("m1.kfyear", label=h5("Avg Years for time-varying par (Kalman Filter Models)"), value = NULL , min = 1, max = 50, step = 1,   width = "40%")
											  #)
												)	,
								tabPanel("N2",
												 tags$h4("Naive Model"),
												 tags$h5("All Data Types"),
												 checkboxInput("m2.use","Include this model",value=TRUE),
											  textInput("m2.name", "Model Label", value = "Naive5", width = "40%"),
											  selectizeInput("m2.modeltype", "Model Type", choices = "Naive", selected=model.types[1], width = "40%"),
											  numericInput("m2.avgyrs", label=h5("Avg Years (Naive Models)"), value = 5 , min = 1, max = 10, step = 1,   width = "40%"),
											  #checkboxInput("m2.boxcox","Box-Cox Transf. - Time Series Models",value=FALSE),
											  #numericInput("m2.kfyear", label=h5("Avg Years for time-varying par (Kalman Filter Models)"), value = NULL , min = 1, max = 50, step = 1,   width = "40%")
											  ),
								tabPanel("TS1",
												 tags$h4("Time Series Model"),
												 tags$h5("All Data Types"),
												 checkboxInput("m6.use","Include this model",value=TRUE),
												 textInput("m6.name", "Model Label", value = "TSArimaBC", width = "40%"),
												 selectizeInput("m6.modeltype", "Model Type", choices = c("TimeSeriesArima","TimeSeriesExpSmooth"), selected="TimeSeriesArima", width = "40%"),
												 #numericInput("m6.avgyrs", label=h5("Avg Years (Naive Models)"), value = NULL , min = 1, max = 10, step = 1,   width = "40%"),
												 checkboxInput("m6.boxcox","Box-Cox Transf. - Time Series Models",value=TRUE)#,
												 #numericInput("m6.kfyear", label=h5("Avg Years for time-varying par (Kalman Filter Models)"), value = NULL, min = 1, max = 50, step = 1,   width = "40%")
								),
								tabPanel("TS2",
												 tags$h4("Time Series Model"),
												 tags$h5("All Data Types"),
													checkboxInput("m7.use","Include this model",value=TRUE),
												 textInput("m7.name", "Model Label", value = "TSArimaNoBC", width = "40%"),
												 selectizeInput("m7.modeltype", "Model Type", choices = c("TimeSeriesArima","TimeSeriesExpSmooth"), selected="TimeSeriesArima", width = "40%"),
												 #numericInput("m7.avgyrs", label=h5("Avg Years (Naive Models)"), value = NULL , min = 1, max = 10, step = 1,   width = "40%"),
												 checkboxInput("m7.boxcox","Box-Cox Transf. - Time Series Models",value=FALSE)#,
												 #numericInput("m7.kfyear", label=h5("Avg Years for time-varying par (Kalman Filter Models)"), value = NULL , min = 1, max = 50, step = 1,   width = "40%")
								),
								tabPanel("TS3",
												 tags$h4("Time Series Model"),
												 tags$h5("All Data Types"),
												checkboxInput("m8.use","Include this model",value=TRUE),
												 textInput("m8.name", "Model Label", value = "TSExpSmoothBC", width = "40%"),
												 selectizeInput("m8.modeltype", "Model Type", choices = c("TimeSeriesArima","TimeSeriesExpSmooth"), selected="TimeSeriesExpSmooth", width = "40%"),
												 #numericInput("m8.avgyrs", label=h5("Avg Years (Naive Models)"), value = NULL , min = 1, max = 10, step = 1,   width = "40%"),
												 checkboxInput("m8.boxcox","Box-Cox Transf. - Time Series Models",value=TRUE)#,
												 #numericInput("m8.kfyear", label=h5("Avg Years for time-varying par (Kalman Filter Models)"), value = NULL , min = 1, max = 50, step = 1,   width = "40%")
								),
								tabPanel("TS4",
													tags$h4("Time Series Model"),
													tags$h5("All Data Types"),
													checkboxInput("m9.use","Include this model",value=TRUE),
												 textInput("m9.name", "Model Label", value = "TSExpSmoothNoBC", width = "40%"),
												 selectizeInput("m9.modeltype", "Model Type", choices = c("TimeSeriesArima","TimeSeriesExpSmooth"), selected="TimeSeriesExpSmooth", width = "40%"),
												 #numericInput("m9.avgyrs", label=h5("Avg Years (Naive Models)"), value = NULL , min = 1, max = 10, step = 1,   width = "40%"),
												 checkboxInput("m9.boxcox","Box-Cox Transf. - Time Series Models",value=FALSE)#,
												 #numericInput("m9.kfyear", label=h5("Avg Years for time-varying par (Kalman Filter Models)"), value = NULL , min = 1, max = 50, step = 1,   width = "40%")
								),
								tabPanel("Sib1",
												 tags$h4("Sibling Regression Model"),
												 tags$h5("Only works if your data has age classes"),
												 checkboxInput("m3.use","Include this model",value=FALSE),
											  textInput("m3.name", "Model Label", value = "SibRegSimple", width = "40%"),
											  selectizeInput("m3.modeltype", "Model Type", choices = c("SibRegSimple","SibRegLogPower","SibRegKalman","SibRegPooledSimple","SibRegPooledLogPower"), selected="SibRegSimple", width = "40%"),
											  numericInput("m3.kfyear", label=h5("Kalman Filter: Avg Years for time-varying par"), value = NULL , min = 1, max = 50, step = 1,   width = "40%"),
												numericInput("m3.max.pool", label=h5("SibReg Pooled (Simple or Log Power): Max cohorts to pool"), value = 3, min = 2, max = 5, step = 1,   width = "50%")
												),
								tabPanel("Sib2",
												 tags$h4("Sibling Regression Model"),
												 tags$h5("Only works if your data has age classes"),
												 checkboxInput("m4.use","Include this model",value=FALSE),
											  textInput("m4.name", "Model Label", value = "SibRegLogPower", width = "40%"),
											  selectizeInput("m4.modeltype", "Model Type", choices = c("SibRegSimple","SibRegLogPower","SibRegKalman","SibRegPooledSimple","SibRegPooledLogPower"), selected="SibRegLogPower", width = "40%"),
											  numericInput("m4.kfyear", label=h5("Avg Years for time-varying par (applies to Kalman Filter Models only)"), value = NULL , min = 1, max = 50, step = 1,   width = "40%"),
												 numericInput("m4.max.pool", label=h5("SibReg Pooled (Simple or Log Power): Max cohorts to pool"), value = 3, min = 2, max = 5, step = 1,   width = "50%")
											  ),
								tabPanel("Sib3",
												 tags$h4("Sibling Regression Model"),
												 tags$h5("Only works if your data has age classes"),
												 checkboxInput("m5.use","Include this model",value=FALSE),
											  textInput("m5.name", "Model Label", value = "SibRegKalman", width = "40%"),
											  selectizeInput("m5.modeltype", "Model Type", choices = c("SibRegSimple","SibRegLogPower","SibRegKalman","SibRegPooledSimple","SibRegPooledLogPower"), selected="SibRegKalman", width = "40%"),
											  numericInput("m5.kfyear", label=h5("Avg Years for time-varying par (applies to Kalman Filter Models only)"), value = 5 , min = 1, max = 50, step = 1,   width = "40%"),
												 numericInput("m5.max.pool", label=h5("SibReg Pooled (Simple or Log Power): Max cohorts to pool"), value = 3, min = 2, max = 5, step = 1,   width = "50%")
											  ),
								tabPanel("SibCov",
												 tags$h4("Sibling Regression Model with Covariates"),
												 tags$h5("Only works if your data has age classes"),
												 checkboxInput("m12.use","Include this model",value=FALSE),
												 textInput("m12.name", "Model Label", value = "SibRegComplex", width = "40%"),
												 selectizeInput("m12.modeltype", "Model Type", choices = c("SibRegComplex"), selected="SibRegComplex", width = "40%"),
												 numericInput("m12.tol.AIC", label=h5("SibReg Complex: Tolerance AIC [1-0]"), value = 0.75, min = 0, max = 1, step = 0.1,   width = "25%"),
			  								numericInput("m12.tol.r.sq", label=h5("SibReg Complex: Tolerance R2 [0-1]"), value = 0.02, min = 0, max = 1, step = 0.1,   width = "25%")
								),
								tabPanel("Rate",
												 tags$h4("Return Rate (Mechanistic) Model"),
												 tags$h5("Only works if your data has age classes and predictor ('Pred_XYZ') variables"),
												 tags$h5("(e.g. juvenile outmigration or hatchery releases)"),
												 checkboxInput("m11.use","Include this model",value=FALSE),
												 textInput("m11.name", "Model Label", value = "ReturnRate1", width = "40%"),
												 selectizeInput("m11.modeltype", "Model Type", choices = c("ReturnRate"), selected="ReturnRate", width = "40%"),
												 uiOutput("m11.pred.var.menu"),
												 selectizeInput("m11.rate.avg", "Avg", choices = c("wtmean","mean", "median"), selected="wtmean"),
												 numericInput("m11.last.n", "Last n obs",  value = 100 , min = 1, max = 100, step = 1,   width = "50%")
								),


							tabPanel("Any",checkboxInput("m10.use","Include this model",value=FALSE),
											  textInput("m10.name", "Model Label", value = NULL, width = "40%"),
											  selectizeInput("m10.modeltype", "Model Type", choices = model.types, selected=NULL, width = "40%"),
											  numericInput("m10.avgyrs", label=h5("Avg Years (Naive Models)"), value = NULL , min = 1, max = 10, step = 1,   width = "40%"),
											  checkboxInput("m10.boxcox","Box-Cox Transf. - Time Series Models",value=FALSE),
											  numericInput("m10.kfyear", label=h5("Avg Years for time-varying par (applies to Sibling Regression with Kalman Filter Models)"),
											  						 value = NULL , min = 1, max = 50, step = 1,   width = "40%"),
											 uiOutput("m10.pred.var.menu"),
											 selectizeInput("m10.rate.avg", "Rate: Avg", choices = c("wtmean","mean", "median"), selected="wtmean"),
											 numericInput("m10.last.n", "Rate: Last n obs",  value = 100 , min = 1, max = 100, step = 1,   width = "50%"),
											 numericInput("m10.tol.AIC", label=h5("SibReg Complex: Tolerance AIC [1-0]"), value = 0.75, min = 0, max = 1, step = 0.1,   width = "25%"),
											 numericInput("m10.tol.r.sq", label=h5("SibReg Complex: Tolerance R2 [0-1]"), value = 0.02, min = 0, max = 1, step = 0.1,   width = "25%")

											 )


								 ) # end nested tabsetpanel
								 ), # end tab panel for settings
                  tabPanel("Point Forecasts",  DT::dataTableOutput("table.multi.ptfc"),
							downloadButton("download.ptfc.table.merged","Download")
							),

                  tabPanel("Intervals",  DT::dataTableOutput("compare.int.table")
							),



	tabPanel("Model Selection", DT::dataTableOutput("table.bestmodels"),
								downloadButton("download.bestmodels.table","Download")
							),
				 # tabPanel("x Pt FC w/ Best Models",  DT::dataTableOutput("table.bestmodel.fc")) ,
				  #tabPanel("x Bootstrap",  DT::dataTableOutput("table.bestmodel.fc.boot"))
				tabPanel("FC Plot - By Age",
									h2(textOutput("compare.ageclass"), align = "center"),
									plotOutput("compare.ptfc",width = "100%", height = "600px")	),

				tabPanel("Ranking Details",
				tabsetPanel(type = "tabs",

				 tabPanel("Rank - Across Ages", DT::dataTableOutput("table.cumul.ranking")) ,

				  tabPanel("Perf. - By Age",
									#h2(textOutput("compare.ageclass"), align = "center"), # crashes the data loading???? DTpackage issue???? - using caption for now
									DT::dataTableOutput("table.retropm")) ,
				  tabPanel("Rank - By Age",
									#h2(textOutput("compare.ageclass"), align = "center"), # crashes the data loading???? DTpackage issue????  - using caption for now
									DT::dataTableOutput("table.ranking"))
							)),


	tabPanel("Report",  value= "custom.report",

					 fluidPage(

					 	titlePanel("Report Download"),

					 	fluidRow(
					 		column(8,
					 					 tags$h2("Standard Reports"),
					 					 tags$h4("Short Report in MS Word: Summary of forecasts and model ranks"),
					 					 selectizeInput("shortreport.plotsort", "Sort Plots By", choices = c("AvgRank","Forecast"), selected="AvgRank"),
					 					 downloadButton("downloadComparisonRepWordShort", "Download SHORT MSWord report"),
					 					 tags$hr(),
					 					 tags$h4("Long Report in MS Word: Also includes detailed forecast and ranking tables by age"),
					 					 selectizeInput("longreport.plotsort", "Sort Plots By", choices = c("AvgRank","Forecast"), selected="AvgRank"),
					 					 downloadButton("downloadComparisonRepWordLong", "Download LONG MSWord report")


					 		)
					 	)
					 )



	)  # end custom reports tab panel






				  )




		) # end main panel

		) #end page with side bar for model comparison

	),


######### CUSTOM REPORTS	#############







####################################


	 tabPanel("Help",  value= "help.panel",

fluidPage(

  titlePanel("Help Page"),

  fluidRow(
    column(8,
	  includeMarkdown("Markdown/help.md")
    )
  )
)



	  ),  # end Help tab panel

	tabPanel("About",

fluidPage(

  titlePanel("About ForecastR"),

  fluidRow(
    column(8,
      includeMarkdown("Markdown/about.md")
    )
  )
)
	  )  # end about tab panel



) # end navbar Page


