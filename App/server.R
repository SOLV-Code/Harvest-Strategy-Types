
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




	volumes.use <- getVolumes()
	shinyDirChoose(input,"repdir",roots=getVolumes())
	reports.path.use <- reactive(input$repdir)
	output$reports.path.use <- renderPrint({   parseDirPath(roots=volumes.use, selection =reports.path.use())  })











######################################################################
# MODEL PRE_CHECK PIECES ("Explore" Tab)
######################################################################

# SHOULD EXTRACT THE data.file -> prepData as a reactive judge


# model list for Explore tab
output$model.menu.precheck <- renderUI({
	selectizeInput("model.use.precheck", "Model Type", choices = model.list(),
								 multiple=FALSE, selected=model.list()[1])
		})

# Predictor Variable List for Explore Tab
output$pred.var.precheck.menu <- renderUI({
	selectInput("pred.var.precheck", label = "Rate: Predictor", choices = predictors.list(),
							multiple=FALSE,selected = predictors.list()[1] )
				})


# Ages List for Explore Tab
output$ages.menu.precheck <- renderUI({
	selectizeInput("precheck.ageclass", "Age Class", choices = ages.list(),
								 multiple=FALSE, selected=ages.list()[1])
})


# Ages List for Model Selection Detail on Explore>Diagnostics
output$ages.menu.model.selection <- renderUI({
	selectizeInput("model.selection.ageclass", "Age Class", choices = ages.list.short(),
								 multiple=FALSE, selected=ages.list.short()[1])
})




# TS: box cox option for Explore Tab
output$boxcox.precheck.menu <- renderUI({
	checkboxInput("precheck.boxcox", label="Time Series: Box-Cox Transform", value = FALSE )
})


# Naive: num yrs avg option for Explore Tab
output$avgyrs.precheck.menu <- renderUI({
	numericInput("precheck.avgyrs", label=h5("Naive: Avg Years"), value = 3 , min = 1, max = 10, step = 1,   width = "50%")
})


# Sibreg Kalman: AVG N OPTION for Explore Tab
output$intavg.precheck.menu <- renderUI({
	numericInput("precheck.intavg", label=h5("SibReg Kalman: Avg n Est"), value = 5, min = 1, max = 10, step = 1,   width = "50%")
})


# Sibreg Pooled Simple: max age classes to pool
output$max.pool.precheck.menu <- renderUI({
	numericInput("precheck.max.pool", label=h5("SibReg Pooled (Simple or Log Power): Max cohorts to pool"), value = 3, min = 2, max = 5, step = 1,   width = "50%")
})




# Sibreg Complex: Model selection tolerance settings for Explore Tab
output$complex.precheck.menu1 <- renderUI({
	numericInput("precheck.tol.AIC", label=h5("SibReg Complex: Tolerance AIC [1-0]"), value = 0.75, min = 0, max = 1, step = 0.1,   width = "50%")
	})
output$complex.precheck.menu2 <- renderUI({
	numericInput("precheck.tol.r.sq", label=h5("SibReg Complex: Tolerance R2 [0-1]"), value = 0.02, min = 0, max = 1, step = 0.1,   width = "50%")
})






# Predictor Variable List for Compare Tab - Model 10
output$m10.pred.var.menu <- renderUI({
	selectInput("m10.pred.var", label = "Rate:Predictor", choices = predictors.list(),
							multiple=FALSE,selected = predictors.list()[1] )
})



# Predictor Variable List for Compare Tab - Model 11
output$m11.pred.var.menu <- renderUI({
	selectInput("m11.pred.var", label = "Predictor", choices = predictors.list(),
							multiple=FALSE,selected = predictors.list()[1] )
})


# Default label of forecasted value from settings tab
output$axis.label.sel <- renderUI({

	# changes back when switching tabs?
	textInput("axis.label", label=h5("Forecasted Variable"), value = axis.label.default() , width = "40%")

	# dsmr problem
	#selectInput("axis.label", label = h5("Forecasted Variable"), choices = axis.label.default(),
	#						multiple=FALSE,selected = axis.label.default()[1] )

})





	precheck.settings  <- reactive({

		print(input$model.use.precheck )

					# extract settings (should streamline)
				if(input$model.use.precheck  %in% c("TimeSeriesArima","TimeSeriesExpSmooth")){settings.use <- list(BoxCox= input$precheck.boxcox)}
				if(input$model.use.precheck  %in% c("ReturnRate")){
								settings.use <- list(avg =  input$rate.avg.precheck,
																		 pred.label= input$pred.var.precheck,
																		  last.n= input$last.n.precheck
																		 )}
				if(input$model.use.precheck  %in% c("Naive")){settings.use <- list(avg.yrs= input$precheck.avgyrs)}
				if(input$model.use.precheck  %in% c("SibRegKalman")){settings.use <- list(int.avg= input$precheck.intavg)}
	    	if(input$model.use.precheck  %in% c("SibRegComplex")){settings.use <- list(tol.AIC= input$precheck.tol.AIC,
	    																																						 tol.r.sq = input$precheck.tol.r.sq,
	    																																						 incl.base.eq = FALSE)}
				if(input$model.use.precheck  %in% c( "SibRegSimple","SibRegLogPower")){settings.use <- NULL}
				if(input$model.use.precheck  %in% c( "SibRegPooledSimple","SibRegPooledLogPower")){
																											settings.use <- list(max.pool = input$precheck.max.pool)}
				return(settings.use)

					})

	precheck.modelfit  <- reactive({
				data.file.tmp <- data.file()
				sample.dat <-  prepData(data.file.tmp,out.labels="v2")
				settings.use <- precheck.settings()

				print("entering fitModel")

				fit.obj <- fitModel(model= input$model.use.precheck, data = sample.dat$data,
														data.sibreg = sample.dat$sibreg.in,
														settings = settings.use ,tracing=TRUE)


				#print(input$model.use.precheck)
				#print(sample.dat$data)
				#print(settings.use)
				#print("-------------------------------------------")
				#print(fit.obj$"Age 3")
				#print("-------------------------------------------")
				#print(fit.obj$"Age 4")
				#print(names(fit.obj))

				return(fit.obj)
			})


    precheck.fc <- reactive({
				data.file.tmp <- data.file()
				settings.basic.use <- settings.basic()
				fit.obj <- precheck.modelfit()
				settings.use <- precheck.settings()

				sample.dat <-  prepData(data.file.tmp,out.labels="v2")

				print("entering calcFC")

				#if(is.null(predictors.list())){
				#		fc.obj <- calcFC(fit.obj= fit.obj,data = sample.dat$data,
				#										 data.sibreg = sample.dat$sibreg.in,
				#										 fc.yr= settings.basic.use$FCYear,
				#										 settings = settings.use, tracing=FALSE)
				#}

				#if(!is.null(predictors.list())){

				# if no pred or cov, those inputs will just be NULL
					fc.obj <- calcFC(fit.obj= fit.obj,data = sample.dat$data,
													 data.sibreg = sample.dat$sibreg.in,
													 fc.yr= settings.basic.use$FCYear,
													 settings = settings.use,
													 predictors = sample.dat$predictors,
													 covariates = sample.dat$covariates,
													 tracing=FALSE)

				#}

				#print(fc.obj)

				return(fc.obj)

				})


	int.samples.calc <- reactive({

			# for bootstrap and retrospective, calculate the samples, then the quantiles based on the samples
			# for prediction intervals its the other way around....

   			data.file.tmp <- data.file()
				sample.dat <-  prepData(data.file.tmp,out.labels="v2")
				boot.type.in <- input$boot.type.precheck
				boot.n.in <- input$boot.n.precheck
				settings.use <- precheck.settings()
				settings.basic.use <- settings.basic()
				int.type <- input$interval.type.precheck
				fc.in <-  precheck.fc()


				if(int.type == "Bootstrap"){

					boot.int <- doBoot(data= sample.dat,
														 data.sibreg = sample.dat$sibreg.in,
														 args.fitmodel= list(model= input$model.use.precheck, settings = settings.use),
						args.calcfc = list(fc.yr= settings.basic.use$FCYear,  settings = settings.use),
						args.boot = list(boot.type=boot.type.in, boot.n=boot.n.in, plot.diagnostics=FALSE),
						full.out = TRUE, plot.out=FALSE)

					#print(head(boot.int))




					int.samples.out <- boot.int

					} # end if bootstrap


				if(int.type == "Retrospective"){

					min.retro.yrs <- input$min.retroyrs.explore

					retro.int <- doRetro(model= input$model.use.precheck, data = sample.dat$data,
															 data.sibreg = sample.dat$sibreg.in,
            predictors =  sample.dat$predictors, covariates = sample.dat$covariates,
						retro.settings = list(min.yrs=min.retro.yrs),
						fit.settings = settings.use,
						fc.settings = settings.use,
						tracing=FALSE,out.type="short",
						interval.n = boot.n.in,
						interval.quants = FALSE,
						pt.fc = fc.in)

					int.samples.out <- 	retro.int$retro.interval



					} # end if retrospective



					if(int.type == "Prediction"){


						int.samples.out <- doSampleFromInt(fc.obj=fc.in, interval.n=boot.n.in,interval.quants=FALSE)

					} # end if prediction


				#print("-----int.samples.out---------")
				#print(int.samples.out)

					return(int.samples.out)

					})



	  explore.fc.table <- reactive({

				pt.fc <- precheck.fc()
				int.samples <- int.samples.calc()

				fc.mat <- apply(int.samples,MARGIN=2,FUN=quantile,probs=c(0.1,0.25,0.5,0.75,0.90))

				ages.labels <- dimnames(int.samples)[[2]]
				fc.mat <- as.data.frame(rbind(PointFc = unlist(pt.fc$pt.fc), fc.mat))
				fc.mat <- round(fc.mat)
				fc.mat <- sapply(fc.mat , FUN=function(x) prettyNum(x, big.mark=","))

				dimnames(fc.mat)[[2]] <- ages.labels
				dimnames(fc.mat)[[1]] <- c("PointFC", "p10", "p25", "Median", "p75", "p90")

				return(fc.mat)

			})


	 output$table.explore.fc <- DT::renderDataTable(
	 				DT::datatable(explore.fc.table(), extensions = 'Buttons',
	 											options = list(paging = FALSE ,
	 																		 dom = 'Bfrtip',	buttons =  list(
	 																		 	list(extend = 'csv', filename = "ForecastSummary"))))
					)


   # output$download.table.explore.fc <- downloadHandler(
	#				filename = function(){"TableForecastByAge.csv"},
	#				content = function(fname){
	#					write.csv(explore.fc.table(), fname,row.names=TRUE)
	#				}
	#				)



    explore.model.selection <- reactive({

    	if(input$model.use.precheck == 'SibRegComplex'){
    			   	fit.in <- precheck.modelfit()
							age.in <- input$model.selection.ageclass

							print("-----")
							print(names(fit.in[[age.in]]))

							model.selection.df <- fit.in[[age.in]][["model.selection"]] %>%
											select(equ,numCoeff,rankAIC, rankRsq,selected) %>%
								      mutate(selected = as.character(selected)) %>%
											mutate(selected = recode(selected,"FALSE" = "","TRUE" = "X"))
    	}

			if(input$model.use.precheck != 'SibRegComplex'){
				model.selection.df <- data.frame(equ = NA, adj.r.sq = NA,     AIC = NA, diffAIC  = NA,   probAIC = NA )
			}

    	print("model.selection.df")
    	print(model.selection.df)

    	return(model.selection.df)

    })



    output$table.explore.model.selection <- DT::renderDataTable(
    	DT::datatable(explore.model.selection(),
    								extensions = 'Buttons',
    			options = list(paging = FALSE ,
    			dom = 'Bfrtip',	buttons = list(
    				list(extend = 'csv', filename = "ModelSelection")),
    		rowCallback = DT::JS(
    									'function(row, data) {
          if (parseFloat(data[5]) == "X")
          $("td", row).css("background", "orange")
    									}')
      )
    )
    )











	output$precheck.plot.fitandfc <- renderPlot({

				fit.in <- precheck.modelfit()
				fc.in <-  precheck.fc()

				age.in <- input$precheck.ageclass
				plotModelFit(fit.in, options= list(plot.which = "fitted_ts",age.which=age.in,plot.add=FALSE),
										 axis.label = input$axis.label, fc.add= fc.in ) #,subtitle = fit.in[[age.in]]$formula



					})



   	output$precheck.plot.resid_ts <- renderPlot({

				fit.in <- precheck.modelfit()
				fc.in <-  precheck.fc()
				age.in <- input$precheck.ageclass
				plotModelFit(fit.in, options= list(plot.which = "resid_ts",age.which=age.in,plot.add=FALSE), fc.add= fc.in )
				})

   	output$precheck.plot.fitvsobs <- renderPlot({

				fit.in <- precheck.modelfit()
				fc.in <-  precheck.fc()
				age.in <- input$precheck.ageclass
				plotModelFit(fit.in, options= list(plot.which = "fitvsobs",age.which=age.in,plot.add=FALSE), fc.add= fc.in )
				})


   	output$precheck.plot.residvsfitted <- renderPlot({

				fit.in <- precheck.modelfit()
				fc.in <-  precheck.fc()
				#print("starting residsvsfitted plot----------------------------")
				#print(fit.in)
				#print(fc.in)

				age.in <- input$precheck.ageclass
				plotModelFit(fit.in, options= list(plot.which = "residvsfitted",age.which=age.in,plot.add=FALSE),
										 fc.add= fc.in )
				})







   	output$precheck.plot.resid_hist<- renderPlot({

				fit.in <- precheck.modelfit()
				fc.in <-  precheck.fc()
				age.in <- input$precheck.ageclass
				plotModelFit(fit.in, options= list(plot.which = "resid_hist",age.which=age.in,plot.add=FALSE), fc.add= fc.in )
				})




   	output$precheck.plot.resid_qq <- renderPlot({

				fit.in <- precheck.modelfit()
				fc.in <-  precheck.fc()
				age.in <- input$precheck.ageclass
				plotModelFit(fit.in, options= list(plot.which = "resid_qq",age.which=age.in,plot.add=FALSE), fc.add= fc.in )
				})


   	output$precheck.modeldiagnostic <- renderPlot({

				fit.in <- precheck.modelfit()
				fc.in <-  precheck.fc()
				age.in <- input$precheck.ageclass
				plotModelFit(fit.in, options= list(plot.which = "modeldiagnostic",age.which=age.in,plot.add=FALSE), fc.add= fc.in )
				})


    	output$precheck.plot.boots.sample <- renderPlot({
				data.file.tmp <- data.file()
				sample.dat <-  prepData(data.file.tmp,out.labels="v2")
				age.in <- input$precheck.ageclass
				boot.type.in <- input$boot.type.precheck
				plotBootSeries(sample.dat$data, boot.type = boot.type.in, age.which=age.in)

				})






		output$precheck.plot.intervals <- renderPlot({

						int.type <- input$interval.type.precheck
						int.samples <- 	int.samples.calc()

						#print("starting plot")
						#print(head(int.samples))
						#print(colMeans(int.samples))
						#print(int.samples$'Age 5')



						box.plot(int.samples, y.lab = "Forecast Abundance",
							fill.vec = "lightblue" ,border.vec= "darkblue",
							labels=TRUE,violin=TRUE,y.lim=c(0,max(unlist(int.samples))),
							labels.angle = 0,labels.adj=c(0.5,2))

						title(main=paste0("Forecast Interval - ", int.type))



							})


  output$"downloadPreCheckRep" <- downloadHandler(
    filename = function() {
      paste(gsub(".csv","",input$file.name.2 ),"_" ,input$model.use.precheck,"_",  gsub(" ","",input$precheck.ageclass),"_",  Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {

	pdf(file,onefile=TRUE,width=11,height=8.5)

		fit.in <- precheck.modelfit()
		fc.in <-  precheck.fc()
		age.in <- input$precheck.ageclass

		data.file.tmp <- data.file()
		sample.dat <-  prepData(data.file.tmp,out.labels="v2")
		boot.type.in <- input$boot.type.precheck
		boot.n.in <- input$boot.n.precheck
		settings.use <- precheck.settings()
		settings.basic.use <- settings.basic()

		doBoot(data= sample.dat,
					 data.sibreg = sample.dat$sibreg.in,
					 args.fitmodel= list(model= input$model.use.precheck, settings = settings.use),
					args.calcfc = list(fc.yr= settings.basic.use$FCYear,  settings = settings.use),
					args.boot = list(boot.type=boot.type.in, boot.n=boot.n.in, plot.diagnostics=FALSE),
					full.out = FALSE, plot.out=TRUE)



		plotModelFit(fit.in, options= list(plot.which = "precheck.report",age.which=age.in,plot.add=FALSE), fc.add= fc.in )

		data.file.tmp <- data.file()
		sample.dat <-  prepData(data.file.tmp,out.labels="v2")
		boot.type.in <- input$boot.type.precheck
		plotBootSeries(sample.dat$data, boot.type = boot.type.in, age.which=age.in)



		dev.off()
    }
  )



######################################################################
# MODEL COMPARISON PIECES ("Compare" Tab)
######################################################################

	  output$compare.ageclass <- renderText({  input$compare.ageclass })

     multifc.list.gui <- reactive({

				multifc.list <- NULL

				# SHOULD CHANGE THE INPUTS FOR extractSettings() to a named list!!!!!!!!!!!!!!!!!!

				# Naive Models
				if(input$m1.use){multifc.list[[input$m1.name]] <-list(model.type= input$m1.modeltype, settings=extractSettings(input$m1.modeltype,input$m1.avgyrs,input$m1.boxcox,input$m1.kfyear))	}
				if(input$m2.use){multifc.list[[input$m2.name]] <-list(model.type= input$m2.modeltype, settings=extractSettings(input$m2.modeltype,input$m2.avgyrs,input$m2.boxcox,input$m2.kfyear))	}

				# SibReg Versions without Cov
				if(input$m3.use){multifc.list[[input$m3.name]] <-list(model.type= input$m3.modeltype,
						settings=extractSettings(input$m3.modeltype,input$m3.avgyrs,input$m3.boxcox,input$m3.kfyear,input$m3.max.pool))
																				}
				if(input$m4.use){multifc.list[[input$m4.name]] <-list(model.type= input$m4.modeltype,
																															settings=extractSettings(input$m4.modeltype,input$m4.avgyrs,input$m4.boxcox,input$m4.kfyear,input$m4.max.pool))	}
				if(input$m5.use){multifc.list[[input$m5.name]] <-list(model.type= input$m5.modeltype,
																															settings=extractSettings(input$m5.modeltype,input$m5.avgyrs,input$m5.boxcox,input$m5.kfyear,input$m5.max.pool))	}

				# Sibreg Complex
				if(input$m12.use){multifc.list[[input$m12.name]] <-list(model.type= input$m12.modeltype,
																																settings=extractSettings(input$m12.modeltype,input$m12.avgyrs,input$m12.boxcox,input$m12.kfyear,input$m12.max.pool,
																																										input$m12.pred.var,input$m12.rate.avg,input$m12.last.n,
																																									input$m12.tol.AIC,input$m12.tol.r.sq))
				#print(multifc.list[[input$m12.name]])
				#stop()

				}




				# Time Series Models
				if(input$m6.use){multifc.list[[input$m6.name]] <-list(model.type= input$m6.modeltype, settings=extractSettings(input$m6.modeltype,input$m6.avgyrs,input$m6.boxcox,input$m6.kfyear))	}
				if(input$m7.use){multifc.list[[input$m7.name]] <-list(model.type= input$m7.modeltype, settings=extractSettings(input$m7.modeltype,input$m7.avgyrs,input$m7.boxcox,input$m7.kfyear))	}
				if(input$m8.use){multifc.list[[input$m8.name]] <-list(model.type= input$m8.modeltype, settings=extractSettings(input$m8.modeltype,input$m8.avgyrs,input$m8.boxcox,input$m8.kfyear))	}
				if(input$m9.use){multifc.list[[input$m9.name]] <-list(model.type= input$m9.modeltype, settings=extractSettings(input$m9.modeltype,input$m9.avgyrs,input$m9.boxcox,input$m9.kfyear))	}

				# Return Rate Model
				if(input$m11.use){

							multifc.list[[input$m11.name]] <-list(model.type= input$m11.modeltype,
																										settings=extractSettings(input$m11.modeltype,
																																						 input$m11.avgyrs,
																																						 input$m11.boxcox,
																																						 input$m11.kfyear,
																																						 input$m11.pred.var,input$m11.rate.avg,input$m11.last.n,
																																						 input$m11.max.pool,
																																						 input$m11.tol.AIC,input$m11.tol.r.sq))}





				# Any Model
				if(input$m10.use){multifc.list[[input$m10.name]] <-list(model.type= input$m10.modeltype, settings=extractSettings(input$m10.modeltype,input$m10.avgyrs,input$m10.boxcox,input$m10.kfyear,
																																																													input$m10.max.pool,
																																																													input$m10.pred.var,input$m10.rate.avg,input$m10.last.n,
																																																													input$m10.tol.AIC,input$m10.tol.r.sq))}






				print("multifc.list-------------------------")
				print(multifc.list)
        #stop()
				return(multifc.list)

				})


		compare.multifit  <- reactive({
				data.file.tmp <- data.file()
				min.retro.yrs <- input$min.retroyrs.compare
				int.type.in <- input$interval.type.compare
				int.n.in <- input$boot.n.compare
				boot.type.in <- input$boot.type.compare
				settings.use <- multifc.list.gui()

				print("starting multiFC")
				multiresults.retro <- multiFC(data.file=data.file.tmp,
																			settings.list=settings.use,
								do.retro=TRUE,retro.min.yrs=min.retro.yrs,
								out.type="short",
								int.type = int.type.in, int.n = int.n.in ,
								boot.type = boot.type.in,
								tracing=FALSE)


				return(multiresults.retro)


				})





		compare.ptfc.table <- reactive({
					multifit.out <- compare.multifit()
					multi.ptfc <- round(multifit.out[["table.ptfc"]])
					model.names <- dimnames(multi.ptfc )[[1]]


					multi.ptfc <- sapply(multi.ptfc , FUN=function(x) prettyNum(x, big.mark=","))


					dimnames(multi.ptfc )[[1]] <- model.names
					return(multi.ptfc)
					})





			compare.int.array <- reactive({

					#this is the full array of model * plevel * age, without number formatting

					multifit.out <- compare.multifit()
					multi.int.array <- round(multifit.out[["int.array"]])
					return(multi.int.array )


					})

			compare.int.table <- reactive({

					# this is the formatted table of model * plevel for the user-selected age class
					# WARNING: These are now text values with formatting ("13,174" vs 13174)

					multi.int.array <- compare.int.array()

					multi.int.table <- as.data.frame(multi.int.array[,,input$compare.ageclass])
					model.names <- dimnames(multi.int.table)[[1]]
					multi.int.table  <- sapply(multi.int.table , FUN=function(x) prettyNum(x, big.mark=","))

					dimnames(multi.int.table )[[1]] <- model.names
					return(multi.int.table )


					})



			compare.int.report.table <- reactive({

				# this is the formatted table of model * plevel for the user-selected age class
				# WARNING: These are now text values with formatting ("13,174" vs 13174)

				multi.int.array <- compare.int.array()

				age.labels <- dimnames(multi.int.array)[[3]]

				report.table.out <- data.frame(Age = age.labels[1],multi.int.array[,,1]) %>%
					mutate_all(function(x){prettyNum(x, big.mark=",")}) %>%
					rownames_to_column(var="Model")

			if(length(age.labels)>1){

			for(i in 2:length(age.labels)){

					report.table.out <- bind_rows(report.table.out,
																				data.frame(Age = age.labels[i],multi.int.array[,,i]) %>%
																					mutate_all(function(x){prettyNum(x, big.mark=",")}) %>%
																					rownames_to_column(var="Model"))
				}}


				report.table.out <- report.table.out %>% select(Age, everything())

				return(report.table.out)


			})










#		output$table.multi.ptfc  <- renderTable(
#				compare.ptfc.table(), rownames= TRUE, digits=0 , stripes = TRUE, hover = TRUE
#				)

# obsolete, because using version with rank added in below
#     output$table.multi.ptfc <- DT::renderDataTable(
#	 				DT::datatable(compare.ptfc.table(), options = list(paging = FALSE))
#					)


			compare.plot.values <- reactive({



				ranks.full <- compare.ranking.obj()
				multi.int.array <- compare.int.array()
				age.labels <- dimnames(multi.int.array)[[3]]
				model.labels <- dimnames(multi.int.array)[[1]]

				# start with youngest age (or total of no ages)

				table.out <- bind_cols(Age = age.labels[1], Model = model.labels,
															 as.data.frame(multi.int.array[,,age.labels[1]])) %>%
															left_join(ranks.full[[1]] %>% rownames_to_column(var="Model") %>%
																					select(Model,rank.avg) %>% rename(RankByAge = rank.avg)%>%
																					mutate(RankByAge = round(RankByAge,2)),
																				by="Model") %>%
					left_join(ranks.full[["Total"]] %>% rownames_to_column(var="Model") %>%
					select(Model,rank.avg) %>% rename(RankTotal = rank.avg)%>%
						mutate(RankTotal = round(RankTotal,2)),
				by="Model")

				# add rermaining ages
				if(length(age.labels)>1){


				for(i in 2:length(age.labels)){

				table.out <- bind_rows(table.out,
															 bind_cols(Age = age.labels[i], Model = model.labels,
																 as.data.frame(multi.int.array[,,age.labels[i]]))  %>%
															 	left_join(ranks.full[[i]] %>% rownames_to_column(var="Model") %>%
															 							select(Model,rank.avg) %>% rename(RankByAge = rank.avg) %>%
															 						   mutate(RankByAge = round(RankByAge,2)),
															 						by="Model") %>%
															 	left_join(ranks.full[["Total"]] %>% rownames_to_column(var="Model") %>%
															 							select(Model,rank.avg) %>% rename(RankTotal = rank.avg)%>%
															 							mutate(RankTotal = round(RankTotal,2)),
															 						by="Model")
																)


				}}

			# merge in point forecast

				multifit.out <- compare.multifit()
				ptfc.add <- multifit.out$table.ptfc %>% rownames_to_column(var="Model") %>%
					pivot_longer(cols = all_of(age.labels), names_to = "Age")  %>%
					rename(PtFC = value)

			table.out <- left_join(table.out, ptfc.add, by = c("Age","Model"))



				return(table.out)

			})


     	output$compare.ptfc <- renderPlot({

					print("doing plot")

     			table.allages <-			compare.plot.values()
     			table.use <-  table.allages %>% dplyr::filter(Age == input$compare.ageclass ) %>% select(-Age)


     			if(input$compare.plotsort == "AvgRank") {table.use <-  table.use %>% arrange(RankByAge)}
     			if(input$compare.plotsort == "Forecast") {table.use <-  table.use %>% arrange(PtFC)}



     			print(table.use)

     			n.models <- dim(table.use)[1]
					vec.plot <- table.use$PtFC
					fc.p50 <- table.use$p50
					fc.p10 <- table.use$p10
					fc.p90 <- table.use$p90

					par(mai=c(1,2.5,1.5,1))

					plot(vec.plot , n.models:1, axes=FALSE,xlim = c(0,max(fc.p90)), xlab="Forecast",ylab="", ylim=c(0,n.models+1),
							pch=19,type="p",cex=1.5,col="red",cex.lab=1.4,main=input$compare.ageclass,col.main="darkblue")
					abline(h=1:n.models,col="lightgrey")
					segments(fc.p10,n.models:1,fc.p90, n.models:1,col="red",lwd=2)

					# Pt FC
					points(vec.plot , n.models:1, pch=19,type="p",cex=1.5,col="red")
					text(vec.plot , (n.models:1)+0.2, labels=prettyNum(round(vec.plot),big.mark=","),cex=1,col="red")



					# bounds labels
					points(fc.p50 , n.models:1, pch=4,type="p",cex=1.5,col="red")
					text(fc.p10 , (n.models:1)+0.2, labels=prettyNum(round(fc.p10),big.mark=","),cex=0.7,col="red")
					text(fc.p90 , (n.models:1)+0.2, labels=prettyNum(round(fc.p90),big.mark=","),cex=0.7,col="red")


					axis(2,at=n.models:1,labels=paste0(table.use$Model," (",table.use$RankByAge,")"),las=2,cex.axis=1.1)
					axis(1)


			})






		compare.rankingpm <- reactive({input$compare.pm})


		compare.retropm.array<- reactive({
					multifit.out <- compare.multifit()

					multi.retropm <- multifit.out[["retro.pm"]][[input$retrotype.compare]]


					return(multi.retropm)
					})


		compare.retropm.table<- reactive({
					multi.retro.obj <- compare.retropm.array()
					retropm.table.out <- multi.retro.obj[,,input$compare.ageclass]
					return(retropm.table.out)
					})

		compare.retropm.report.table <- reactive({

			#print("STARTING REPORT TABLE")

			multi.retro.obj <- compare.retropm.array()

			age.labels <- dimnames(multi.retro.obj)[[3]]

			report.table.out <- data.frame(Age = age.labels[1],multi.retro.obj[,,1]) %>%
													rownames_to_column(var="Model")

			if(length(age.labels)>1){

			for(i in 2:length(age.labels)){

			report.table.out <- bind_rows(report.table.out,
																		data.frame(Age = age.labels[i],multi.retro.obj[,,i]) %>%
																			rownames_to_column(var="Model"))
			}}


			report.table.out <- report.table.out %>% select(Age, everything())

			return(report.table.out)

		})

		compare.ranking.obj <- reactive({
					ranking.pm.use <- compare.rankingpm()
					multi.retro.obj <- compare.retropm.array()



					ranking.out <- rankModels(dat = multi.retro.obj,columnToRank=ranking.pm.use, relative.bol=input$rel.bol)

					#print(ranking.out)
					# OLD VERSION
					#ranking.out <- getRanks(as.data.frame(multi.retropm), columnToRank = ranking.pm.use)
					#ranking.out <- ranking.out[ , grep("rank",dimnames(ranking.out)[[2]])]   # extract only the rank columns
					# can't sort here, b/c will screw up the merge below!!!
					#ranking.out <- ranking.out[order(ranking.out$average.rank),]

					return(ranking.out)
					})


		compare.ranking.table <- reactive({
					ranking.obj <- compare.ranking.obj()
					ranking.table.out  <- ranking.obj[[input$compare.ageclass]]
					ranking.table.out <- ranking.table.out[ , grep("rank",dimnames(ranking.table.out)[[2]])] # extract only the rank columns
					ranking.table.out <- round(ranking.table.out,2)
					return(ranking.table.out)
					})



		compare.ranking.report.table <- reactive({

			print("STARTING REPORT TABLE")

			ranking.obj <- compare.ranking.obj()

			age.labels <- names(ranking.obj)

			report.table.out <- data.frame(Age = age.labels[1],ranking.obj[[1]]) %>%
				rownames_to_column(var="Model")

			#print(report.table.out)

			if(length(age.labels)>1){

			for(i in 2:length(age.labels)){

				report.table.out <- bind_rows(report.table.out,
																			data.frame(Age = age.labels[i],ranking.obj[[i]]) %>%
																				rownames_to_column(var="Model"))
			}}


			report.table.out <- report.table.out %>% select(Age, everything())

			report.table.out <- report.table.out %>% dplyr::filter(!is.na(rank.avg )) %>% mutate(rank.avg = round(rank.avg,2))

			return(report.table.out)

		})






		compare.cumul.ranking.table <- reactive({
					ranking.obj <- compare.ranking.obj()
					ranking.table.out  <- ranking.obj$cumulativerankSorted
					ranking.table.out <- ranking.table.out[ , grep("rank",dimnames(ranking.table.out)[[2]])] # extract only the rank columns
					ranking.table.out <- round(ranking.table.out,2)
					return(ranking.table.out)
					})


		compare.bestmodel.table <- reactive({

					multifit.out <- compare.multifit()
					pt.fc.in  <- round(multifit.out[["table.ptfc"]])
					ranking.obj <- compare.ranking.obj()
					ranking.bestmodel.out  <- ranking.obj$bestmodel
					multi.int.array <- compare.int.array()

					summary.table <- tableForecastRanking(pt.fc.in = pt.fc.in  , int.in = multi.int.array,
										bestmodel.in = ranking.bestmodel.out )

					return(summary.table )
					})


		compare.ptfc.table.merged <- reactive({
					fc.table <-  compare.ptfc.table() %>% as.data.frame() %>% rownames_to_column("Label")
					rank.table <- compare.cumul.ranking.table() %>% as.data.frame() %>% rownames_to_column("Label")
					
					print(fc.table)
					print(rank.table)
					
					
					merged.table <- left_join(rank.table %>% select(Label,rank.sum), fc.table, by = "Label") %>%
					                   column_to_rownames("Label")
					#merged.table <- cbind(SumRanks = round(rank.table[,"rank.sum"],2),fc.table)

            print("flag1")

					return(merged.table)
					})


     output$table.multi.ptfc <- DT::renderDataTable(
	 				DT::datatable(compare.ptfc.table.merged(), options = list(paging = FALSE))
					)

     output$compare.int.table <- DT::renderDataTable(
	 				DT::datatable(compare.int.table(), options = list(paging = FALSE),caption=input$compare.ageclass)
					)




     output$table.retropm <- DT::renderDataTable(
	 				DT::datatable(compare.retropm.table(), caption = input$compare.ageclass , options = list(paging = FALSE))
					)



      output$table.ranking <- DT::renderDataTable(
	 				DT::datatable(compare.ranking.table(), caption = input$compare.ageclass , options = list(paging = FALSE))
					)

      output$table.cumul.ranking <- DT::renderDataTable(
	 				DT::datatable(compare.cumul.ranking.table(), caption = "Cumulative Ranks" , options = list(paging = FALSE))
					)



       output$table.bestmodels <- DT::renderDataTable(
	 				DT::datatable(compare.bestmodel.table(), rownames=FALSE, options = list(paging = FALSE))
					)



 		output$download.ptfc.table.merged <- downloadHandler(
					filename = function(){"TablePointForecasts.csv"},
					content = function(fname){
						write.csv(compare.ptfc.table.merged(), fname,row.names=TRUE)
					}
					)


		output$download.bestmodels.table <- downloadHandler(
					filename = function(){"Table_BestModels.csv"},
					content = function(fname){
						write.csv(compare.bestmodel.table(), fname,row.names=FALSE)
					}
					)




	output$downloadComparisonRepWordShort <- downloadHandler(
	# using template from https://shiny.rstudio.com/articles/generating-reports.html
			filename =   paste0("WordReport_Short",Sys.Date(),".doc"),
			content = function(file) {
	        # Copy the report file to a temporary directory before processing it, in
			# case we don't have write permissions to the current working dir (which
			# can happen when deployed).
        tempReport <- file.path(tempdir(), "Report_ShortComp.Rmd")
        file.copy("Markdown/Report_ShortComp.Rmd", tempReport, overwrite = TRUE)

		settings.basic.use <- settings.basic()

		# don't use all of these, but simpler to just feed the same as long report and keep the same YAML
		params <- c(settings.basic.use ,
								list(Table_Multi_Pt_FC = compare.ptfc.table.merged()), # formatted table 1
								list(Table_BestModels = compare.bestmodel.table()), # formatted table 2
								list(Table_CumulRanks = compare.cumul.ranking.table()), # formatted table 3
								list(Table_Multi_Int_FC = compare.int.report.table()),
								list(Int_Type = input$interval.type.compare),
								list(Table_RetroPM = compare.retropm.report.table()),
								list(Table_RetroRanks = compare.ranking.report.table()),
								list(plot.sort = input$shortreport.plotsort),
								list(Plot_Values = compare.plot.values()),
								list(multifit.out = compare.multifit() )   # raw outputs in a list object
		)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
			) # end render()
      } # end content
    ) # end downloadHandler()


 	output$downloadComparisonRepWordLong <- downloadHandler(
	# using template from https://shiny.rstudio.com/articles/generating-reports.html
			filename =   paste0("WordReport_Long",Sys.Date(),".doc"),
			content = function(file) {
	        # Copy the report file to a temporary directory before processing it, in
			# case we don't have write permissions to the current working dir (which
			# can happen when deployed).
        tempReport <- file.path(tempdir(), "Report_LongComp.Rmd")
        file.copy("Markdown/Report_LongComp.Rmd", tempReport, overwrite = TRUE)

    settings.basic.use <- settings.basic()
		print("STARTING LONG REPORT")
    print(settings.basic.use)
    print("1-----------")
    print(compare.ptfc.table.merged())
    print("2-----------")
    print(compare.bestmodel.table())
    print("3-----------")
    print(compare.cumul.ranking.table())
    print("4-----------")
    print(compare.int.report.table())
    print("5-----------")
    print(input$interval.type.compare)
    print("6-----------")
    print(compare.retropm.report.table())
    print("7-----------")
    print(compare.ranking.report.table())
    print("8-----------")
    print(input$longreport.plotsort)
    print("9-----------")
    print(compare.plot.values())
    print("10-----------")
    print(compare.multifit() )

    params <- c(settings.basic.use ,
					list(Table_Multi_Pt_FC = compare.ptfc.table.merged()), # formatted table 1
					list(Table_BestModels = compare.bestmodel.table()), # formatted table 2
					list(Table_CumulRanks = compare.cumul.ranking.table()), # formatted table 3
					list(Table_Multi_Int_FC = compare.int.report.table()),
					list(Int_Type = input$interval.type.compare),
					list(Table_RetroPM = compare.retropm.report.table()),
					list(Table_RetroRanks = compare.ranking.report.table()),
					list(plot.sort = input$longreport.plotsort),
					list(Plot_Values = compare.plot.values()),
					list(multifit.out = compare.multifit() )   # raw outputs in a list object
					)

        print(params)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
			) # end render()
      } # end content
    ) # end downloadHandler()





 	#output$downloadComparisonRepWordCustom <- downloadHandler(
 		# using template from https://shiny.rstudio.com/articles/generating-reports.html
 #		filename =   paste0("WordReport_Custom",Sys.Date(),".doc"),
 	#	content = function(file) {
 			# Copy the report file to a temporary directory before processing it, in
 			# case we don't have write permissions to the current working dir (which
 			# can happen when deployed).
 	#		tempReport <- file.path(tempdir(), "Report_Custom.Rmd")
 	#		file.copy("Markdown/CustomReportParts/1_CustomReport_MainFile.Rmd", tempReport, overwrite = TRUE)

 	#		settings.basic.use <- settings.basic()
 	#		params <- c(settings.basic.use,
 	#								list(Table_Multi_Pt_FC = compare.ptfc.table.merged()), # formatted table 1
 	#								list(multifit.out = compare.multifit(),
 	#								     incl.exec.summary = input$customrep.exec.summary
 	#								)   # report settings
 	#		)

 	#		print("CUSTOM REPORT PARAMS --------------------------------------------")
 	#		print(params)

 			# Knit the document, passing in the `params` list, and eval it in a
 			# child of the global environment (this isolates the code in the document
 			# from the code in this app).
 	#		rmarkdown::render(tempReport, output_file = file,
 	#											params = params,
 	#											envir = new.env(parent = globalenv())
 	#		) # end render()
 #		} # end content
# 	) # end downloadHandler()











	# not working for some reason
   	#	compare.fittedpm.table <- reactive({
	#				multifit.out <- compare.multifit()
	#				multi.fittedpm <- multifit.out[["retro.pm"]][["fitted.pm.last"]][,,input$compare.ageclass]
	#				return(multi.fittedpm)
	#				})

	#	output$table.fittedpm <- renderTable(
	#			compare.fittedpm.table(), rownames= TRUE, stripes = TRUE, hover = TRUE
	#			)



######################################################################
# OTHER STUFF
######################################################################

	output$inputheader.table <- renderTable({ data.file() })  # masking issue with package DT? shiny::renderTable doesn't fix it
					#, options = list(scrollX = TRUE, scrollY = "200px")











}

