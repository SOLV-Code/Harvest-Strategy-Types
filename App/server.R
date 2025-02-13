
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
library("gridExtra")



	volumes.use <- getVolumes()
	shinyDirChoose(input,"repdir",roots=getVolumes())
	reports.path.use <- reactive(input$repdir)
	output$reports.path.use <- renderPrint({   parseDirPath(roots=volumes.use, selection =reports.path.use())  })





	hcr.calc <- reactive({


	  run.vec <- c(input$run.med,
	                    max(c(0,input$run.med * (1 - input$run.lower/100))) ,
	                    input$run.med * (1 + input$run.upper/100),
	                    seq(0,input$plot.lim,length.out = 97)
	                   )




	    if(input$display.tab == "FixedRate"){
    			hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$display.tab ,
			        hcr.settings = list(fixed.rate = input$fixed.rate))
			    }
	  if(input$display.tab == "FixedSpn"){
	       hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$display.tab ,
	            hcr.settings = list(fixed.spn = input$fixed.spn))
	 	  }

	  if(input$display.tab == "StepSpn"){

	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$display.tab ,
	                       hcr.settings = list(stepspn1.rp=input$stepspn1.rp,
	                                           stepspn2.rp=input$stepspn2.rp,
	                                           stepspn1.target=input$stepspn1.target,
	                                           stepspn2.target=input$stepspn2.target))
	  }


	  if(input$display.tab == "FixedCt"){
	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$display.tab ,
	                       hcr.settings = list(fixed.ct= input$fixed.ct))
	     	  }


	  if(input$display.tab == "Step"){
        hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$display.tab ,
	                       hcr.settings = list(step1.rp=input$step1.rp,
	                                           step2.rp=input$step2.rp,
	                                           step3.rp=input$step3.rp,
	                                           step1.rate=input$step1.rate,
	                                           step2.rate=input$step2.rate,
	                                           step3.rate=input$step3.rate
	                                           ))
  	  }


	  if(input$display.tab == "SlopedER"){

	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$display.tab ,
	                       hcr.settings = list(slopeder.rp1=input$slopeder.rp1,
	                                           slopeder.rp2=input$slopeder.rp2,
	                                           slopeder.rp3=input$slopeder.rp3,
	                                           slopeder.rp4 = input$slopeder.rp4,
	                                           slopeder.rate1=input$slopeder.rate1,
	                                           slopeder.rate2=input$slopeder.rate2,
	                                           slopeder.rate3=input$slopeder.rate3,
	                                           slopeder.rate4 =input$slopeder.rate4
	                       ))

	  }


	  if(input$display.tab == "IceHockeyStick"){

	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$display.tab ,
	                       hcr.settings = list(ice.rp1=input$ice.rp1,
	                                           ice.rp2=input$ice.rp2,
	                                           ice.rate=input$ice.rate))


	  }

	  if(input$display.tab == "FieldHockeyStick"){

	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$display.tab ,
	                       hcr.settings = list(field.rp1=input$field.rp1,
	                                           field.rate=input$field.rate))
	  }

	  if(input$display.tab == "FieldHockeyStickwFloor"){


	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$display.tab ,
	                       hcr.settings = list(fieldwfloor.rp1=input$fieldwfloor.rp1,
	                                           fieldwfloor.rate=input$fieldwfloor.rate,
	                                           fieldwfloor.floor = input$fieldwfloor.floor ))

	  }



     hcr.out <- hcr.out %>% select(Run,ER,Spn,Ct)

			return(hcr.out)

					})




	# output$table.hcr<- DT::renderDataTable(
	# 				DT::datatable(hcr.calc(), extensions = 'Buttons',
	# 											options = list(paging = FALSE ,
	# 																		 dom = 'Bfrtip',	buttons =  list(
	# 																		 	list(extend = 'csv', filename = "HCR_Summary"))))
	#				)



	 # to show same plot on diff panels, need to create copies
	 #https://stackoverflow.com/a/44242503

	output$plot9 <- output$plot8 <-	output$plot7 <-  output$plot6 <- output$plot5 <- output$plot4 <- output$plot3 <- output$plot2 <- output$plot1 <- renderPlot({

				hcr.in <- hcr.calc()
        #print(hcr.in)

	if(input$plot.type != "Table"){


	  plot.sub <-  hcr.in[1:3,] %>% as.data.frame()


	  hcr.in <- hcr.in %>% arrange(Run)


        if(input$plot.type == "Rate"){
              y.label <- paste(input$rate.type,"(%)")
              plot(hcr.in$Run,hcr.in$ER,type="l",bty="n",axes=FALSE,lwd = 5,col="darkblue",
                  xlab = paste(input$run.label,input$units.use), ylab =y.label,ylim = c(0,100),
                  cex.lab= 1.5,col.lab="darkblue")
	          }


       if(input$plot.type == "Spawners"){
        y.label <- paste("Spawner Target",input$units.use)
        plot(hcr.in$Run,hcr.in$Spn,type="l",bty="n",axes=FALSE,lwd = 5,col="darkblue",
             xlab = paste(input$run.label,input$units.use), ylab =y.label, ylim = c(0,input$plot.lim),
             cex.lab= 1.5,col.lab="darkblue")
        	}

        if(input$plot.type == "Catch"){
          y.label <- paste("Catch Target",input$units.use)
          plot(hcr.in$Run,hcr.in$Ct,type="l",bty="n",axes=FALSE,lwd = 5,col="darkblue",
               xlab = paste(input$run.label,input$units.use), ylab =y.label, ylim = c(0,input$plot.lim),
               cex.lab= 1.5,col.lab="darkblue")
        }


        axis(1, cex.axis=1.5,col.axis = "darkblue")
        axis(2, cex.axis=1.5,col.axis = "darkblue",las=2)



        # shaded area version
        #rect.col <- alpha("firebrick1", 0.2)
        #rect(plot.sub$Run[2],par("usr")[3],plot.sub$Run[3],par("usr")[4],col=rect.col ,border = rect.col )

        # 3 lines version
        abline(v = plot.sub$Run[1],col = "red", lwd=3)
        abline(v = plot.sub$Run[2] ,col = "red", lwd=3,lty=2)
        abline(v = plot.sub$Run[3] ,col = "red", lwd=3,lty=2)





        if(input$plot.type == "Rate"){var.pt <- "ER"}
        if(input$plot.type == "Spawners"){var.pt <- "Spn"}
        if(input$plot.type == "Catch"){var.pt <- "Ct"}




        # ref lines
        segments(par("usr")[1],plot.sub[1,var.pt],plot.sub$Run[1],plot.sub[1,var.pt],col="red",lwd=2)
        segments(par("usr")[1],plot.sub[2,var.pt],plot.sub$Run[2],plot.sub[2,var.pt],col="red",lwd=2,lty=2)
        segments(par("usr")[1],plot.sub[3,var.pt],plot.sub$Run[3],plot.sub[3,var.pt],col="red",lwd=2, lty=2)




       if(input$plot.type == "Rate"){lines(hcr.in$Run,hcr.in$ER,type="l",bty="n",lwd = 5,col="darkblue")  }
        if(input$plot.type == "Spawners"){lines(hcr.in$Run,hcr.in$Spn,type="l",bty="n",lwd = 5,col="darkblue") }
        if(input$plot.type == "Catch"){lines(hcr.in$Run,hcr.in$Ct,type="l",bty="n",lwd = 5,col="darkblue")}


        # ref points
        points(plot.sub$Run[1],plot.sub[1,var.pt],pch=21,col="red",cex=2,lwd=3,bg="firebrick1")
        points(plot.sub$Run[2],plot.sub[2,var.pt],pch=21,col="red",cex=2,lwd=3,bg="white")
        points(plot.sub$Run[3],plot.sub[3,var.pt],pch=21,col="red",cex=2,lwd=3,bg="white")

        title(main = input$plot.type,col.main="darkblue",font.main=2,cex.main=2)

        } # end if plot


				if(input$plot.type == "Table"){


				  table.in <- hcr.in[1:3,] %>% as.data.frame() %>% arrange(Run) %>% round()
				  table.in <- t(table.in) %>% as.data.frame()
				  names(table.in) <- c("Lower","Mid","Upper")
				  print(table.in)


				  tt <- ttheme_default(base_size=34)

				  grid.table( table.in,theme = tt)


				}

					})

########################################
# COMPARISON CALCS





	comp.hcr1.calc <- reactive({

	  run.vec <- c(input$run.med,
	               max(c(0,input$run.med * (1 - input$run.lower/100))) ,
	               input$run.med * (1 + input$run.upper/100),
	               seq(0,input$plot.lim,length.out = 97)
	  )


	  if(input$hcr1.type == "Fixed Rate"){

	    hcr.out <- data.frame(Run = run.vec ,ER = rep(input$fixed.rate.hcr1,length(run.vec)))
	    hcr.out$Spn <- hcr.out$Run * (1-hcr.out$ER/100)
	    hcr.out$Ct <- hcr.out$Run * hcr.out$ER/100
	  }

	  if(input$hcr1.type == "Fixed Spn"){

	    hcr.out <- data.frame(Run = run.vec ,Spn = rep(input$fixed.spn.hcr1,length(run.vec)))
	    below.idx <- (hcr.out$Run - hcr.out$Spn) <0
	    hcr.out$Spn[below.idx] <- hcr.out$Run[below.idx]
	    hcr.out$Ct <- hcr.out$Run - hcr.out$Spn
	    hcr.out$Ct[hcr.out$Ct<0] <- 0
	    hcr.out$ER <- round(hcr.out$Ct /  hcr.out$Run *100)
	    hcr.out$ER[is.na(hcr.out$ER)] <- 0
	  	  }

	  print("hcr1")
	  print(hcr.out)

	  }) # end HCR 1 calcs



	comp.hcr2.calc <- reactive({

	  run.vec <- c(input$run.med,
	               max(c(0,input$run.med * (1 - input$run.lower/100))) ,
	               input$run.med * (1 + input$run.upper/100),
	               seq(0,input$plot.lim,length.out = 97)
	  )


	  if(input$hcr2.type == "Fixed Rate"){

	    hcr.out <- data.frame(Run = run.vec ,ER = rep(input$fixed.rate.hcr2,length(run.vec)))
	    hcr.out$Spn <- hcr.out$Run * (1-hcr.out$ER/100)
	    hcr.out$Ct <- hcr.out$Run * hcr.out$ER/100
	  }

	  if(input$hcr2.type == "Fixed Spn"){

	    hcr.out <- data.frame(Run = run.vec ,Spn = rep(input$fixed.spn.hcr2,length(run.vec)))
	    below.idx <- (hcr.out$Run - hcr.out$Spn) <0
	    hcr.out$Spn[below.idx] <- hcr.out$Run[below.idx]
	    hcr.out$Ct <- hcr.out$Run - hcr.out$Spn
	    hcr.out$Ct[hcr.out$Ct<0] <- 0
	    hcr.out$ER <- round(hcr.out$Ct /  hcr.out$Run *100)
	    hcr.out$ER[is.na(hcr.out$ER)] <- 0
	  }

	  print("hcr2")
	  print(hcr.out)


	}) # end HCR 2 calcs





	output$plotComp <- renderPlot({

	  hcr1.in <- comp.hcr1.calc()
	  hcr1.in <- hcr1.in %>% arrange(Run)

	  hcr2.in <- comp.hcr2.calc()
	  hcr2.in <- hcr2.in %>% arrange(Run)

	  print("lty")
	  print(input$hcr1.line.type)

    plot(hcr1.in$Run,hcr1.in$Spn,type="l",col= input$hcr1.line.col,lty=as.numeric(input$hcr1.line.type))
    lines(hcr2.in$Run,hcr2.in$Spn,type="l",col= input$hcr2.line.col,lty=as.numeric(input$hcr2.line.type))


	})


}

