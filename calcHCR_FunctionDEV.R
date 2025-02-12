library(tidyverse)


calcHCR_dev <- function(run.vec,hcr.type,hcr.settings){
# hcr.vec = vector with run sizes
# hcr.type = one of "FixedRate", "FixedSPn"
# hcr.settings = list object with required settings (details depend on type)

hcr.type.list <- c("FixedRate","FixedSpn","StepSpn","FixedCt", "Step","SlopedER",
                   "IceHockeyStick","FieldHockeyStick","FieldHockeyStickwFloor")

if(!(hcr.type %in% hcr.type.list)){
  warning("Check hcr.type. Selected option currently not available.");stop()}

  if(hcr.type == "FixedRate"){
    hcr.out <- data.frame(Run = run.vec ,ER = rep(hcr.settings$fixed.rate,length(run.vec)))
    hcr.out$Spn <- hcr.out$Run * (1-hcr.out$ER/100)
    hcr.out$Ct <- hcr.out$Run * hcr.out$ER/100
  }

if(hcr.type == "FixedSpn"){

  hcr.out <- data.frame(Run = run.vec ,Spn = rep(hcr.settings$fixed.spn,length(run.vec)))
  below.idx <- (hcr.out$Run - hcr.out$Spn) <0
  hcr.out$Spn[below.idx] <- hcr.out$Run[below.idx]
  hcr.out$Ct <- hcr.out$Run - hcr.out$Spn
  hcr.out$Ct[hcr.out$Ct<0] <- 0
  hcr.out$ER <- round(hcr.out$Ct /  hcr.out$Run *100)
  hcr.out$ER[is.na(hcr.out$ER)] <- 0
}




if(hcr.type == "StepSpn"){


  hcr.out <- data.frame(Run = run.vec ,Spn = run.vec)

  below1.idx <- hcr.out$Run <= hcr.settings$stepspn1.rp
  step1.idx <- hcr.out$Run > hcr.settings$stepspn1.rp & hcr.out$Run <= hcr.settings$stepspn2.rp
  step2.idx <- hcr.out$Run > hcr.settings$stepspn2.rp

  hcr.out$Spn[below1.idx] <- hcr.out$Run[below1.idx] # below lower RP: all Run goes to Spn
  hcr.out$Spn[step1.idx] <- hcr.settings$stepspn1.target # above lower RP but below upper RP: harvest all above target 1
  hcr.out$Spn[step2.idx] <- hcr.settings$stepspn2.target # above upper Ref pt: harvest all above target 2

  hcr.out$Ct <- hcr.out$Run - hcr.out$Spn
  hcr.out$Ct[hcr.out$Ct<0] <- 0
  hcr.out$ER <- round(hcr.out$Ct /  hcr.out$Run *100)
  hcr.out$ER[is.na(hcr.out$ER)] <- 0


}


if(hcr.type == "FixedCt"){

  hcr.out <- data.frame(Run = run.vec ,Ct = rep(hcr.settings$fixed.ct,length(run.vec)))
  below.idx <- (hcr.out$Run - hcr.out$Ct) <0
  hcr.out$Ct[below.idx] <- hcr.out$Run[below.idx]
  hcr.out$Spn <- hcr.out$Run - hcr.out$Ct
  hcr.out$Spn[hcr.out$Spn<0] <- 0
  hcr.out$ER <- round(hcr.out$Ct /  hcr.out$Run *100)

  #print(hcr.out)

}

if(hcr.type == "Step"){

  step0.idx <- run.vec < hcr.settings$step1.rp
  step1.idx <- run.vec >= hcr.settings$step1.rp & run.vec < hcr.settings$step2.rp
  step2.idx <- run.vec >= hcr.settings$step2.rp & run.vec < hcr.settings$step3.rp
  step3.idx <- run.vec >= hcr.settings$step3.rp

  hcr.out <- data.frame(Run = run.vec ,ER = NA)
  hcr.out$ER[step0.idx] <- 0
  hcr.out$ER[step1.idx] <- hcr.settings$step1.rate
  hcr.out$ER[step2.idx] <- hcr.settings$step2.rate
  hcr.out$ER[step3.idx] <- hcr.settings$step3.rate

  hcr.out$Spn <- hcr.out$Run * (1-hcr.out$ER/100)
  hcr.out$Ct <- hcr.out$Run * hcr.out$ER/100


}

return(hcr.out)

} # end function


# Test all the options
calcHCR_dev(run.vec = seq(0,200,by=10),
            hcr.type = "FixedRate",
            hcr.settings = list(fixed.rate = 50))

calcHCR_dev(run.vec = seq(0,200,by=10),
            hcr.type = "FixedSpn",
            hcr.settings = list(fixed.spn = 75))

calcHCR_dev(run.vec = seq(0,200,by=10),
            hcr.type = "StepSpn",
            hcr.settings = list(stepspn1.rp=30,
                                stepspn2.rp=70,
                                stepspn1.target=15,
                                stepspn2.target=45))

calcHCR_dev(run.vec = seq(0,200,by=10),
            hcr.type = "FixedCt",
            hcr.settings = list(fixed.ct= 25))


calcHCR_dev(run.vec = seq(0,200,by=10),
            hcr.type = "Step",
            hcr.settings = list(step1.rp=50,
                                step2.rp=80,
                                step3.rp=120,
                                step1.rate=15,
                                step2.rate=30,
                                step3.rate=45
                                ))










if(input$display.tab == "SlopedER"){

  # 4 ref points, each have run size and corresponding ER
  # this results in 5 zones:
  # Zone 0: Run is 0 to RP1 -> ER = 0
  # Zone 1: RP1 < Run <= RP2  -> slope and intercept based on points 1 and 2
  # Zone 2: RP2 < Run <= RP3  -> slope and intercept based on points 2 and 3
  # Zone 3: RP3 < Run <= RP4  -> slope and intercept based on points 3 and 4
  # Zone 4: Run larger than RP4 -> ER = ER4

  zone0.idx <- run.vec < input$slopeder.rp1
  zone1.idx <- run.vec >= input$slopeder.rp1 & run.vec < input$slopeder.rp2
  zone2.idx <- run.vec >= input$slopeder.rp2 & run.vec < input$slopeder.rp3
  zone3.idx <- run.vec >= input$slopeder.rp3 & run.vec < input$slopeder.rp4
  zone4.idx <- run.vec >= input$slopeder.rp4

  # line fit for Zone 1
  line1.pts <- data.frame(Run = c(input$slopeder.rp1,input$slopeder.rp2),
                          ER = c(input$slopeder.rate1,input$slopeder.rate2))
  line1.fit <- lm(ER ~ Run, data = line1.pts)
  # lines 2,3
  line2.pts <- data.frame(Run = c(input$slopeder.rp2,input$slopeder.rp3),
                          ER = c(input$slopeder.rate2,input$slopeder.rate3))
  line2.fit <- lm(ER ~ Run, data = line2.pts)
  line3.pts <- data.frame(Run = c(input$slopeder.rp3,input$slopeder.rp4),
                          ER = c(input$slopeder.rate3,input$slopeder.rate4))
  line3.fit <- lm(ER ~ Run, data = line3.pts)



  hcr.out <- data.frame(Run = run.vec ,ER = NA)
  hcr.out$ER[zone0.idx] <- 0
  hcr.out$ER[zone1.idx] <- predict(line1.fit,newdata=hcr.out[zone1.idx,])
  hcr.out$ER[zone2.idx] <- predict(line2.fit,newdata=hcr.out[zone2.idx,])
  hcr.out$ER[zone3.idx] <- predict(line3.fit,newdata=hcr.out[zone3.idx,])
  hcr.out$ER[zone4.idx] <- input$slopeder.rate4

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

  #print(hcr.out)

}

if(input$display.tab == "FieldHockeyStick"){

  hcr.out <- data.frame(Run = run.vec ,ER = NA)

  # calc abd above spn target
  abd.above.spn <- 	 hcr.out$Run  - input$field.rp1
  abd.above.spn[abd.above.spn<0] <- 0

  # calc potential rate (bounded by cap)
  hcr.out$ER <- round(abd.above.spn/hcr.out$Run,5)*100
  hcr.out$ER <- pmin(hcr.out$ER,input$field.rate)


  hcr.out$Spn  <- hcr.out$Run  * (1-hcr.out$ER/100)
  hcr.out$Ct <- hcr.out$Run  * (hcr.out$ER/100)


  #print(hcr.out)

}

if(input$display.tab == "FieldHockeyStickwFloor"){

  hcr.out <- data.frame(Run = run.vec ,ER = NA)

  # calc abd above spn target
  abd.above.spn <- 	 hcr.out$Run  - input$fieldwfloor.rp1
  abd.above.spn[abd.above.spn<0] <- 0

  # calc potential rate (bounded by cap)
  hcr.out$ER <- round(abd.above.spn/hcr.out$Run,5)*100
  hcr.out$ER <- pmin(hcr.out$ER,input$fieldwfloor.rate)
  hcr.out$ER <- pmax(hcr.out$ER,input$fieldwfloor.floor)

  hcr.out$Spn  <- hcr.out$Run  * (1-hcr.out$ER/100)
  hcr.out$Ct <- hcr.out$Run  * (hcr.out$ER/100)


  #print(hcr.out)

}



hcr.out <- hcr.out %>% select(Run,ER,Spn,Ct)

return(hcr.out)


