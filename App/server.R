
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


	  run.vec <- sort(c(seq(0,input$plot.lim,length.out = 97),
	                    input$run.med,
	                    max(c(0,input$run.med * (1 - input$run.lower/100))) ,
	                    input$run.med * (1 + input$run.upper/100)
	                   ))



	    if(input$display.tab == "FixedRate"){

			hcr.out <- data.frame(Run = run.vec ,ER = rep(input$fixed.rate,length(run.vec)))
			hcr.out$Spn <- hcr.out$Run * (1-hcr.out$ER/100)
			hcr.out$Ct <- hcr.out$Run * hcr.out$ER/100
	    }



	  if(input$display.tab == "FixedSpn"){

	    hcr.out <- data.frame(Run = run.vec ,Spn = rep(input$fixed.spn,length(run.vec)))
	    below.idx <- (hcr.out$Run - hcr.out$Spn) <0
	    hcr.out$Spn[below.idx] <- hcr.out$Run[below.idx]
	    hcr.out$Ct <- hcr.out$Run - hcr.out$Spn
	    hcr.out$Ct[hcr.out$Ct<0] <- 0
	    hcr.out$ER <- round(hcr.out$Ct /  hcr.out$Run *100)

	    #print(hcr.out)

	  }


	  if(input$display.tab == "FixedCt"){


	    hcr.out <- data.frame(Run = run.vec ,Ct = rep(input$fixed.ct,length(run.vec)))
	    below.idx <- (hcr.out$Run - hcr.out$Ct) <0
	    hcr.out$Ct[below.idx] <- hcr.out$Run[below.idx]
	    hcr.out$Spn <- hcr.out$Run - hcr.out$Ct
	    hcr.out$Spn[hcr.out$Spn<0] <- 0
	    hcr.out$ER <- round(hcr.out$Ct /  hcr.out$Run *100)

	    #print(hcr.out)

	  }


	  if(input$display.tab == "Step"){


	    step0.idx <- run.vec < input$step1.rp
	    step1.idx <- run.vec >= input$step1.rp & run.vec < input$step2.rp
      step2.idx <- run.vec >= input$step2.rp & run.vec < input$step3.rp
      step3.idx <- run.vec >= input$step3.rp

      hcr.out <- data.frame(Run = run.vec ,ER = NA)
      hcr.out$ER[step0.idx] <- 0
      hcr.out$ER[step1.idx] <- input$step1.rate
      hcr.out$ER[step2.idx] <- input$step2.rate
      hcr.out$ER[step3.idx] <- input$step3.rate

      hcr.out$Spn <- hcr.out$Run * (1-hcr.out$ER/100)
      hcr.out$Ct <- hcr.out$Run * hcr.out$ER/100

	    #print(hcr.out)

	  }



	  if(input$display.tab == "IceHockeyStick"){


	    low.idx <- run.vec <= input$ice.rp1
	    high.idx <- run.vec >= input$ice.rp2
	    mid.idx <- !low.idx & ! high.idx

	    ice.slope <-  input$ice.rate / (input$ice.rp2 - input$ice.rp1)

	    hcr.out <- data.frame(Run = run.vec ,ER = NA)

	    hcr.out$ER[low.idx] <- 0
	    hcr.out$ER[high.idx] <- input$ice.rate
	    hcr.out$ER[mid.idx] <- (run.vec[mid.idx] -  input$ice.rp1) * ice.slope

	    hcr.out$Spn <- hcr.out$Run * (1-hcr.out$ER/100)
	    hcr.out$Ct <- hcr.out$Run * hcr.out$ER/100

	    print(hcr.out)

	  }


	  if(input$display.tab == "FieldHockeyStick"){


	    above.cutback.idx <- run.vec >= input$field.rp1
	    fixed.spn.idx <-  run.vec * (1-input$field.rate/100) >= input$field.rp1

	    hcr.out <- data.frame(Run = run.vec ,ER = NA)

	    hcr.out$ER[above.cutback.idx] <- input$field.rate
	    hcr.out$Spn[above.cutback.idx]  <- hcr.out$Run[above.cutback.idx]   * (1-hcr.out$ER[above.cutback.idx]  /100)
	    hcr.out$Ct[above.cutback.idx]  <- hcr.out$Run[above.cutback.idx]   * hcr.out$ER[above.cutback.idx]  /100

	    hcr.out$Spn[fixed.spn.idx] <- input$field.rp1
	    #hcr.out$Spn[!fixed.spn.idx & !above.cutback.idx] <- hcr.out$Run[!fixed.spn.idx & !above.cutback.idx]

	    hcr.out$Ct[fixed.spn.idx] <- hcr.out$Run[fixed.spn.idx] - hcr.out$Spn[fixed.spn.idx]
	    hcr.out$ER[fixed.spn.idx] <- round(hcr.out$Ct[fixed.spn.idx] / hcr.out$Run[fixed.spn.idx]*100)
	    print(hcr.out)

	  }


			return(hcr.out)

					})




	 output$table.hcr<- DT::renderDataTable(
	 				DT::datatable(hcr.calc(), extensions = 'Buttons',
	 											options = list(paging = FALSE ,
	 																		 dom = 'Bfrtip',	buttons =  list(
	 																		 	list(extend = 'csv', filename = "HCR_Summary"))))
					)



	 # to show same plot on diff panels, need to create copies
	 #https://stackoverflow.com/a/44242503

	 output$plot6 <- output$plot5 <- output$plot4 <- output$plot3 <- output$plot2 <- output$plot1 <- renderPlot({

				hcr.in <- hcr.calc()
        #print(hcr.in)

        if(input$plot.type == "Rate"){
              y.label <- paste(input$rate.type,"(%)")
              plot(hcr.in$Run,hcr.in$ER,type="l",bty="n",axes=FALSE,lwd = 5,col="darkblue",
                  xlab = paste(input$run.label,input$units.use), ylab =y.label,ylim = c(0,100),
                  cex.lab= 1.5,col.lab="darkblue")
	          }


       if(input$plot.type == "Spawners"){
        y.label <- paste("Spawner Target",input$units.use)
        plot(hcr.in$Run,hcr.in$Spn,type="l",bty="n",axes=FALSE,lwd = 5,col="darkblue",
             xlab = paste(input$run.label,input$units.use), ylab =y.label,
             cex.lab= 1.5,col.lab="darkblue")
        	}

        if(input$plot.type == "Catch"){
          y.label <- paste("Catch Target",input$units.use)
          plot(hcr.in$Run,hcr.in$Ct,type="l",bty="n",axes=FALSE,lwd = 5,col="darkblue",
               xlab = paste(input$run.label,input$units.use), ylab =y.label,
               cex.lab= 1.5,col.lab="darkblue")
        }


        axis(1, cex.axis=1.5,col.axis = "darkblue")
        axis(2, cex.axis=1.5,col.axis = "darkblue",las=2)






        abline(v = input$run.med,col = "red", lwd=3)
        abline(v = max(c(0,input$run.med * (1 - input$run.lower/100))) ,col = "red", lwd=3,lty=2)
        abline(v = input$run.med * (1 + input$run.upper/100) ,col = "red", lwd=3,lty=2)

        title(main = input$plot.type,col.main="darkblue",font.main=2,cex.main=2)

					})










}

