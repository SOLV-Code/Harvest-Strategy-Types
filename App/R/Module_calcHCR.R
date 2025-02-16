calcHCR <- function(run.vec,hcr.type,hcr.settings){
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


if(hcr.type == "SlopedER"){

  # 4 ref points, each have run size and corresponding ER
  # this results in 5 zones:
  # Zone 0: Run is 0 to RP1 -> ER = 0
  # Zone 1: RP1 < Run <= RP2  -> slope and intercept based on points 1 and 2
  # Zone 2: RP2 < Run <= RP3  -> slope and intercept based on points 2 and 3
  # Zone 3: RP3 < Run <= RP4  -> slope and intercept based on points 3 and 4
  # Zone 4: Run larger than RP4 -> ER = ER4

  zone0.idx <- run.vec < hcr.settings$slopeder.rp1
  zone1.idx <- run.vec >= hcr.settings$slopeder.rp1 & run.vec < hcr.settings$slopeder.rp2
  zone2.idx <- run.vec >= hcr.settings$slopeder.rp2 & run.vec < hcr.settings$slopeder.rp3
  zone3.idx <- run.vec >= hcr.settings$slopeder.rp3 & run.vec < hcr.settings$slopeder.rp4
  zone4.idx <- run.vec >= hcr.settings$slopeder.rp4

  # line fit for Zone 1
  line1.pts <- data.frame(Run = c(hcr.settings$slopeder.rp1,hcr.settings$slopeder.rp2),
                          ER = c(hcr.settings$slopeder.rate1,hcr.settings$slopeder.rate2))
  line1.fit <- lm(ER ~ Run, data = line1.pts)
  # lines 2,3
  line2.pts <- data.frame(Run = c(hcr.settings$slopeder.rp2,hcr.settings$slopeder.rp3),
                          ER = c(hcr.settings$slopeder.rate2,hcr.settings$slopeder.rate3))
  line2.fit <- lm(ER ~ Run, data = line2.pts)
  line3.pts <- data.frame(Run = c(hcr.settings$slopeder.rp3,hcr.settings$slopeder.rp4),
                          ER = c(hcr.settings$slopeder.rate3,hcr.settings$slopeder.rate4))
  line3.fit <- lm(ER ~ Run, data = line3.pts)



  hcr.out <- data.frame(Run = run.vec ,ER = NA)
  hcr.out$ER[zone0.idx] <- 0
  hcr.out$ER[zone1.idx] <- predict(line1.fit,newdata=hcr.out[zone1.idx,])
  hcr.out$ER[zone2.idx] <- predict(line2.fit,newdata=hcr.out[zone2.idx,])
  hcr.out$ER[zone3.idx] <- predict(line3.fit,newdata=hcr.out[zone3.idx,])
  hcr.out$ER[zone4.idx] <- hcr.settings$slopeder.rate4

  hcr.out$Spn <- hcr.out$Run * (1-hcr.out$ER/100)
  hcr.out$Ct <- hcr.out$Run * hcr.out$ER/100

}

if(hcr.type == "IceHockeyStick"){


  low.idx <- run.vec <= hcr.settings$ice.rp1
  high.idx <- run.vec >= hcr.settings$ice.rp2
  mid.idx <- !low.idx & ! high.idx

  ice.slope <-  hcr.settings$ice.rate / (hcr.settings$ice.rp2 - hcr.settings$ice.rp1)

  hcr.out <- data.frame(Run = run.vec ,ER = NA)

  hcr.out$ER[low.idx] <- 0
  hcr.out$ER[high.idx] <- hcr.settings$ice.rate
  hcr.out$ER[mid.idx] <- (run.vec[mid.idx] -  hcr.settings$ice.rp1) * ice.slope

  hcr.out$Spn <- hcr.out$Run * (1-hcr.out$ER/100)
  hcr.out$Ct <- hcr.out$Run * hcr.out$ER/100

  #print(hcr.out)

}

if(hcr.type == "FieldHockeyStick"){

  hcr.out <- data.frame(Run = run.vec ,ER = NA)

  # calc abd above spn target
  abd.above.spn <- 	 hcr.out$Run  - hcr.settings$field.rp1
  abd.above.spn[abd.above.spn<=0] <- 0

  # calc potential rate (bounded by cap)
  hcr.out$ER <- round(abd.above.spn/hcr.out$Run,5)*100
  hcr.out$ER <- pmin(hcr.out$ER,hcr.settings$field.rate)
  hcr.out$ER[abd.above.spn<=0] <- 0

  hcr.out$Spn  <- hcr.out$Run  * (1-hcr.out$ER/100)
  hcr.out$Ct <- hcr.out$Run  * (hcr.out$ER/100)
}



if(hcr.type == "FieldHockeyStickwFloor"){

  hcr.out <- data.frame(Run = run.vec ,ER = NA)

  # calc abd above spn target
  abd.above.spn <- 	 hcr.out$Run  - hcr.settings$fieldwfloor.rp1

  abd.above.spn[abd.above.spn<=0] <- 0


  # calc potential rate (then bound by cap, floor)
  hcr.out$ER <- round(abd.above.spn/hcr.out$Run,5)*100
  hcr.out$ER[abd.above.spn<=0] <- 0
  hcr.out$ER <- pmin(hcr.out$ER,hcr.settings$fieldwfloor.rate)
  
  if(hcr.settings$floor.type == "Fixed"){
		hcr.out$ER <- pmax(hcr.out$ER,hcr.settings$fieldwfloor.floor)
	}

  if(hcr.settings$floor.type == "Declining"){

	switch.pt <- hcr.settings$fieldwfloor.rp1*(1/(1-hcr.settings$fieldwfloor.floor/100))
	floor.zone.idx <- run.vec <= switch.pt  
	print(switch.pt)

  # line fit for floor zone
  floor.pts <- data.frame(Run = c(0,switch.pt),
                          ER = c(0,hcr.settings$fieldwfloor.floor))
  floor.fit <- lm(ER ~ Run, data = floor.pts)
  hcr.out$ER[floor.zone.idx] <- predict(floor.fit,newdata=hcr.out[floor.zone.idx,])
	}


  

  hcr.out$Spn  <- hcr.out$Run  * (1-hcr.out$ER/100)
  hcr.out$Ct <- hcr.out$Run  * (hcr.out$ER/100)


  #print(hcr.out)

}


return(hcr.out)

} # end function