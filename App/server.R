
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
	                    seq(0,input$plot.lim,length.out = 197)
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
	                                           fieldwfloor.floor = input$fieldwfloor.floor,
	                                           floor.type = input$floor.type))

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


        axis(1, cex.axis=1.5,col.axis = "darkblue",col= "darkblue")
        axis(2, cex.axis=1.5,col.axis = "darkblue",col= "darkblue",las=2)



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
				  #print(table.in)


				  tt <- ttheme_default(base_size=34)

				  grid.table( table.in,theme = tt)


				}

					})

########################################
# COMPARISON CALCS





	comp.hcr1.calc <- reactive({

	  run.vec <- c(input$run.med.comp,
	               max(c(0,input$run.med.comp * (1 - input$run.lower.comp/100))) ,
	               input$run.med.comp * (1 + input$run.upper.comp/100),
	               seq(0,input$plot.lim.comp,length.out = 197)
	  )



	  if(input$hcr1.type == "FixedRate"){
	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr1.type ,
	                       hcr.settings = list(fixed.rate = input$fixed.rate.hcr1))
	  }
	  if(input$hcr1.type == "FixedSpn"){
	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr1.type ,
	                       hcr.settings = list(fixed.spn = input$fixed.spn.hcr1))
	  }

	  if(input$hcr1.type == "StepSpn"){

	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr1.type ,
	                       hcr.settings = list(stepspn1.rp=input$stepspn1.rp.hcr1,
	                                           stepspn2.rp=input$stepspn2.rp.hcr1,
	                                           stepspn1.target=input$stepspn1.target.hcr1,
	                                           stepspn2.target=input$stepspn2.target.hcr1))
	  }


	  if(input$hcr1.type == "FixedCt"){
	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr1.type ,
	                       hcr.settings = list(fixed.ct= input$fixed.ct.hcr1))
	  }


	  if(input$hcr1.type == "Step"){
	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr1.type ,
	                       hcr.settings = list(step1.rp=input$step1.rp.hcr1,
	                                           step2.rp=input$step2.rp.hcr1,
	                                           step3.rp=input$step3.rp.hcr1,
	                                           step1.rate=input$step1.rate.hcr1,
	                                           step2.rate=input$step2.rate.hcr1,
	                                           step3.rate=input$step3.rate.hcr1
	                       ))
	  }


	  if(input$hcr1.type == "SlopedER"){

	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr1.type ,
	                       hcr.settings = list(slopeder.rp1=input$slopeder.rp1.hcr1,
	                                           slopeder.rp2=input$slopeder.rp2.hcr1,
	                                           slopeder.rp3=input$slopeder.rp3.hcr1,
	                                           slopeder.rp4 = input$slopeder.rp4.hcr1,
	                                           slopeder.rate1=input$slopeder.rate1.hcr1,
	                                           slopeder.rate2=input$slopeder.rate2.hcr1,
	                                           slopeder.rate3=input$slopeder.rate3.hcr1,
	                                           slopeder.rate4 =input$slopeder.rate4.hcr1
	                       ))

	  }


	  if(input$hcr1.type == "IceHockeyStick"){

	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr1.type ,
	                       hcr.settings = list(ice.rp1=input$ice.rp1.hcr1,
	                                           ice.rp2=input$ice.rp2.hcr1,
	                                           ice.rate=input$ice.rate.hcr1))


	  }

	  if(input$hcr1.type == "FieldHockeyStick"){

	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr1.type ,
	                       hcr.settings = list(field.rp1=input$field.rp1.hcr1,
	                                           field.rate=input$field.rate.hcr1))
	  }

	  if(input$hcr1.type == "FieldHockeyStickwFloor"){
	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr1.type ,
	                       hcr.settings = list(fieldwfloor.rp1=input$fieldwfloor.rp1.hcr1,
	                                           fieldwfloor.rate=input$fieldwfloor.rate.hcr1,
	                                           fieldwfloor.floor = input$fieldwfloor.floor.hcr1 ,
	                                           floor.type = input$floor.type.hcr1))
 }



	  hcr.out <- hcr.out %>% select(Run,ER,Spn,Ct)

	  return(hcr.out)

	  }) # end HCR 1 calcs



	comp.hcr2.calc <- reactive({

	  run.vec <- c(input$run.med.comp,
	               max(c(0,input$run.med.comp * (1 - input$run.lower.comp/100))) ,
	               input$run.med.comp * (1 + input$run.upper.comp/100),
	               seq(0,input$plot.lim.comp,length.out = 197)
	  )



	  if(input$hcr2.type == "FixedRate"){
	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr2.type ,
	                       hcr.settings = list(fixed.rate = input$fixed.rate.hcr2))
	  }
	  if(input$hcr2.type == "FixedSpn"){
	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr2.type ,
	                       hcr.settings = list(fixed.spn = input$fixed.spn.hcr2))
	  }

	  if(input$hcr2.type == "StepSpn"){

	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr2.type ,
	                       hcr.settings = list(stepspn1.rp=input$stepspn1.rp.hcr2,
	                                           stepspn2.rp=input$stepspn2.rp.hcr2,
	                                           stepspn1.target=input$stepspn1.target.hcr2,
	                                           stepspn2.target=input$stepspn2.target.hcr2))
	  }


	  if(input$hcr2.type == "FixedCt"){
	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr2.type ,
	                       hcr.settings = list(fixed.ct= input$fixed.ct.hcr2))
	  }


	  if(input$hcr2.type == "Step"){
	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr2.type ,
	                       hcr.settings = list(step1.rp=input$step1.rp.hcr2,
	                                           step2.rp=input$step2.rp.hcr2,
	                                           step3.rp=input$step3.rp.hcr2,
	                                           step1.rate=input$step1.rate.hcr2,
	                                           step2.rate=input$step2.rate.hcr2,
	                                           step3.rate=input$step3.rate.hcr2
	                       ))
	  }


	  if(input$hcr2.type == "SlopedER"){

	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr2.type ,
	                       hcr.settings = list(slopeder.rp1=input$slopeder.rp1.hcr2,
	                                           slopeder.rp2=input$slopeder.rp2.hcr2,
	                                           slopeder.rp3=input$slopeder.rp3.hcr2,
	                                           slopeder.rp4 = input$slopeder.rp4.hcr2,
	                                           slopeder.rate1=input$slopeder.rate1.hcr2,
	                                           slopeder.rate2=input$slopeder.rate2.hcr2,
	                                           slopeder.rate3=input$slopeder.rate3.hcr2,
	                                           slopeder.rate4 =input$slopeder.rate4.hcr2
	                       ))

	  }


	  if(input$hcr2.type == "IceHockeyStick"){

	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr2.type ,
	                       hcr.settings = list(ice.rp1=input$ice.rp1.hcr2,
	                                           ice.rp2=input$ice.rp2.hcr2,
	                                           ice.rate=input$ice.rate.hcr2))


	  }

	  if(input$hcr2.type == "FieldHockeyStick"){

	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr2.type ,
	                       hcr.settings = list(field.rp1=input$field.rp1.hcr2,
	                                           field.rate=input$field.rate.hcr2))
	  }

	  if(input$hcr2.type == "FieldHockeyStickwFloor"){
	    hcr.out <- calcHCR(run.vec = run.vec, hcr.type = input$hcr2.type ,
	                       hcr.settings = list(fieldwfloor.rp1=input$fieldwfloor.rp1.hcr2,
	                                           fieldwfloor.rate=input$fieldwfloor.rate.hcr2,
	                                           fieldwfloor.floor = input$fieldwfloor.floor.hcr2,
	                                           floor.type = input$floor.type.hcr2 ))
	  }



	  hcr.out <- hcr.out %>% select(Run,ER,Spn,Ct)

	  #print(hcr.out)

	  return(hcr.out)


	}) # end HCR 2 calcs



	output$plotComp <- renderPlot({


	  hcr1.in <- comp.hcr1.calc()
	  plot.sub.1 <-  hcr1.in[1:3,] %>% as.data.frame()
	  hcr1.in <- hcr1.in %>% arrange(Run)

	  hcr2.in <- comp.hcr2.calc()
	  plot.sub.2 <-  hcr2.in[1:3,] %>% as.data.frame()
	  hcr2.in <- hcr2.in %>% arrange(Run)



	  if(input$plot.type.comp != "Table"){




	  #print("lty")
	  #print(input$hcr1.line.type)


    if(input$plot.type.comp == "Rate"){
      y.label <- paste(input$rate.type.comp,"(%)")
      plot(hcr1.in$Run,hcr1.in$ER,type="l",bty="n",axes=FALSE,lwd = 5,col=input$hcr1.line.col,
           lty=as.numeric(input$hcr1.line.type),
           xlab = paste(input$run.label.comp,input$units.use.comp), ylab =y.label,ylim = c(0,100),
           cex.lab= 1.5,col.lab="darkblue")
      lines(hcr2.in$Run,hcr2.in$ER,type="l",lwd = 5,col=input$hcr2.line.col,
           lty=as.numeric(input$hcr2.line.type) )

    }


    if(input$plot.type.comp  == "Spawners"){
      y.label <- paste("Spawner Target",input$units.use.comp)
      plot(hcr1.in$Run,hcr1.in$Spn,type="l",bty="n",axes=FALSE,lwd = 5,col=input$hcr1.line.col,
           lty=as.numeric(input$hcr1.line.type),
           xlab = paste(input$run.label.comp,input$units.use.comp), ylab =y.label, ylim = c(0,input$plot.lim.comp),
           cex.lab= 1.5,col.lab="darkblue")
      lines(hcr2.in$Run,hcr2.in$Spn,type="l",bty="n",axes=FALSE,lwd = 5,col=input$hcr2.line.col,
             lty=as.numeric(input$hcr2.line.type) )

    }

    if(input$plot.type.comp  == "Catch"){
      y.label <- paste("Catch Target",input$units.use.comp)
      plot(hcr1.in$Run,hcr1.in$Ct,type="l",bty="n",axes=FALSE,lwd = 5,col=input$hcr1.line.col,
           lty=as.numeric(input$hcr1.line.type),
           xlab = paste(input$run.label.comp,input$units.use.comp), ylab =y.label,
           ylim = c(0,input$plot.lim.comp),
           cex.lab= 1.5,col.lab="darkblue")
      lines(hcr2.in$Run,hcr2.in$Ct,type="l",bty="n",axes=FALSE,lwd = 5,col=input$hcr2.line.col,
           lty=as.numeric(input$hcr2.line.type))
    }


    axis(1, cex.axis=1.5,col.axis = "darkblue",col= "darkblue")
    axis(2, cex.axis=1.5,col.axis = "darkblue",col= "darkblue",las=2)

    # 3 lines version (lines are same for both HCR, only plot 1)
    abline(v = plot.sub.1$Run[1],col = "red", lwd=3)
    abline(v = plot.sub.1$Run[2] ,col = "red", lwd=3,lty=2)
    abline(v = plot.sub.1$Run[3] ,col = "red", lwd=3,lty=2)


    if(input$plot.type.comp == "Rate"){var.pt <- "ER"}
    if(input$plot.type.comp == "Spawners"){var.pt <- "Spn"}
    if(input$plot.type.comp == "Catch"){var.pt <- "Ct"}

    # ref lines
   segments(par("usr")[1],plot.sub.1[1,var.pt],plot.sub.1$Run[1],plot.sub.1[1,var.pt],col="orange",lwd=2)
  segments(par("usr")[1],plot.sub.1[2,var.pt],plot.sub.1$Run[2],plot.sub.1[2,var.pt],col="orange",lwd=2,lty=2)
   segments(par("usr")[1],plot.sub.1[3,var.pt],plot.sub.1$Run[3],plot.sub.1[3,var.pt],col="orange",lwd=2, lty=2)

    segments(par("usr")[1],plot.sub.2[1,var.pt],plot.sub.2$Run[1],plot.sub.2[1,var.pt],col="red",lwd=2)
    segments(par("usr")[1],plot.sub.2[2,var.pt],plot.sub.2$Run[2],plot.sub.2[2,var.pt],col="red",lwd=2,lty=2)
    segments(par("usr")[1],plot.sub.2[3,var.pt],plot.sub.2$Run[3],plot.sub.2[3,var.pt],col="red",lwd=2, lty=2)


    if(input$plot.type.comp == "Rate"){
      lines(hcr1.in$Run,hcr1.in$ER,type="l",bty="n",lwd = 5,col=input$hcr1.line.col,
                                        lty=as.numeric(input$hcr1.line.type))
       lines(hcr2.in$Run,hcr2.in$ER,type="l",bty="n",lwd = 5,col=input$hcr2.line.col,
                 lty=as.numeric(input$hcr2.line.type))

                                        }
    if(input$plot.type.comp == "Spawners"){
      lines(hcr1.in$Run,hcr1.in$Spn,type="l",bty="n",lwd = 5,col=input$hcr1.line.col,
                                            lty=as.numeric(input$hcr1.line.type))
      lines(hcr2.in$Run,hcr2.in$Spn,type="l",bty="n",lwd = 5,col=input$hcr2.line.col,
            lty=as.numeric(input$hcr2.line.type))
            }
    if(input$plot.type.comp == "Catch"){
      lines(hcr1.in$Run,hcr1.in$Ct,type="l",bty="n",lwd = 5,col=input$hcr1.line.col,
                                         lty=as.numeric(input$hcr1.line.type))
           lines(hcr2.in$Run,hcr2.in$Ct,type="l",bty="n",lwd = 5,col=input$hcr2.line.col,
                lty=as.numeric(input$hcr2.line.type))
            }


    # ref points
    points(plot.sub.1$Run[1],plot.sub.1[1,var.pt],pch=21,col="orange",cex=2,lwd=3,bg="bisque")
    points(plot.sub.1$Run[2],plot.sub.1[2,var.pt],pch=21,col="orange",cex=2,lwd=3,bg="white")
    points(plot.sub.1$Run[3],plot.sub.1[3,var.pt],pch=21,col="orange",cex=2,lwd=3,bg="white")

   points(plot.sub.2$Run[1],plot.sub.2[1,var.pt],pch=21,col="red",cex=2,lwd=3,bg="firebrick1")
  points(plot.sub.2$Run[2],plot.sub.2[2,var.pt],pch=21,col="red",cex=2,lwd=3,bg="white")
  points(plot.sub.2$Run[3],plot.sub.2[3,var.pt],pch=21,col="red",cex=2,lwd=3,bg="white")

    title(main = input$plot.type.comp,col.main="darkblue",font.main=2,cex.main=2)



    legend(0,par("usr")[4]*1.12,legend = c(input$hcr1.label,input$hcr2.label),
           col=c(input$hcr1.line.col,input$hcr2.line.col),text.col="darkblue",
           lty = as.numeric(c(input$hcr1.line.type,input$hcr2.line.type)),
           lwd=3,bg="white",box.col="white",
           bty="o",cex = 1.5,xpd=NA)

	  } # end if plot


	  if(input$plot.type.comp == "Table"){


	    table1.in <- plot.sub.1 %>% arrange(Run) %>%
	      round() %>% t() %>% as.data.frame() %>%
	      rownames_to_column("Variable") %>%
	      mutate(HCR = input$hcr1.label) %>% select(HCR, everything())
	    table1.in[1,1]<-""
	    #print(table1.in)

	    table2.in <- plot.sub.2 %>% arrange(Run) %>%
	      round() %>% t() %>% as.data.frame() %>%
	      rownames_to_column("Variable") %>%
	      mutate(HCR = input$hcr2.label) %>% select(HCR, everything())
	    #print(table2.in)

	   # print(table1.in[-1,3:5])
	   # print(table2.in[-1,3:5])

	    diff.src <- as.data.frame(table2.in[-1,3:5]) - as.data.frame(table1.in[-1,3:5])

	    diff.table <- diff.src %>%
	      as.data.frame() %>% mutate(HCR = "Diff",Variable =  table1.in[-1,"Variable"])
      #print(diff.table)

	    table.in <- bind_rows(table1.in,table2.in[-1,],diff.table)
	    names(table.in) <- c("HCR","Variable","Lower","Mid","Upper")
	    #print(table.in)

	    if(input$hcr1.line.col == "darkblue"){hcr1.col <- "lightblue"}
	    if(input$hcr1.line.col != "darkblue"){hcr1.col <- input$hcr1.line.col}
	    if(input$hcr2.line.col == "darkblue"){hcr2.col <- "lightblue"}
	    if(input$hcr2.line.col != "darkblue"){hcr2.col <- input$hcr2.line.col}


	    tt <-  ttheme_default(core=list(
	      fg_params=list(fontface=c("bold.italic",rep("plain",9))),
	      bg_params = list(fill=c("gray90", rep(hcr1.col,3),
	                              rep(hcr2.col,3),rep("orange",3),
	                       alpha = 0.9))  ),base_size=32)




	    grid.table( table.in,theme = tt,rows=NULL)






  }# end if Table

	})


}

