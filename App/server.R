
# INSERT PASSWORD STEP (CHECK WITH JOHN SON)


function(input, output, session) {



source("R/3c_HelperFunctions_ModelSetup.R")
source.modules("R/")




# Think it needs these here for the server deployment
# These are all shiny specific (not needed in the package)
load_or_install(c("ggplot2","DT","markdown","rmarkdown","shiny","shinydashboard","shinyjqui","shinyFiles","dplyr"))
library("ggplot2")
library("DT")
library("markdown")
library("rmarkdown")
library("shiny")
library("shinydashboard")
library("shinyjqui")
library("shinyFiles")
library("dplyr")
library("purrr")
library("tidyr")
library("tibble")
library("readr")
library("shinyWidgets")




	volumes.use <- getVolumes()
	shinyDirChoose(input,"repdir",roots=getVolumes())
	reports.path.use <- reactive(input$repdir)
	output$reports.path.use <- renderPrint({   parseDirPath(roots=volumes.use, selection =reports.path.use())  })





	hcr.calc <- reactive({

	    run.vec <- seq(0,input$plot.lim,length.out = 100)

      print(run.vec)

			hcr.out <- data.frame(Run = run.vec ,ER = rep(0.5,length(run.vec)))
			hcr.out$Spn <- hcr.out$Run * (1-hcr.out$ER)
			hcr.out$Ct <- hcr.out$Run * hcr.out$ER


			print(hcr.out)


	  return(hcr.out)

					})




	 output$table.hcr<- DT::renderDataTable(
	 				DT::datatable(hcr.calc(), extensions = 'Buttons',
	 											options = list(paging = FALSE ,
	 																		 dom = 'Bfrtip',	buttons =  list(
	 																		 	list(extend = 'csv', filename = "HCR_Summary"))))
					)





	output$plot.hcr <- renderPlot({

				hcr.in <- hcr.calc()
        print(hcr.in)

        plot(hcr.in$Run,hcr.in$ER,type="l",bty="n")



					})










}

