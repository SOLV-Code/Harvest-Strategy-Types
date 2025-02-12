library(tidyverse)


source("App/R/Module_calcHCR.R")


# Test all the options
calcHCR(run.vec = seq(0,200,by=10),
            hcr.type = "FixedRate",
            hcr.settings = list(fixed.rate = 50))

calcHCR(run.vec = seq(0,200,by=10),
            hcr.type = "FixedSpn",
            hcr.settings = list(fixed.spn = 75))

calcHCR(run.vec = seq(0,200,by=10),
            hcr.type = "StepSpn",
            hcr.settings = list(stepspn1.rp=30,
                                stepspn2.rp=70,
                                stepspn1.target=15,
                                stepspn2.target=45))

calcHCR(run.vec = seq(0,200,by=10),
            hcr.type = "FixedCt",
            hcr.settings = list(fixed.ct= 25))


calcHCR(run.vec = seq(0,200,by=10),
            hcr.type = "Step",
            hcr.settings = list(step1.rp=50,
                                step2.rp=80,
                                step3.rp=120,
                                step1.rate=15,
                                step2.rate=30,
                                step3.rate=45
                                ))

calcHCR(run.vec = seq(0,200,by=10),
            hcr.type = "SlopedER",
            hcr.settings = list(slopeder.rp1=50,
                                slopeder.rp2=80,
                                slopeder.rp3=120,
                                slopeder.rp4 = 150,
                                slopeder.rate1=15,
                                slopeder.rate2=30,
                                slopeder.rate3=45,
                                slopeder.rate4 =55
            ))



calcHCR(run.vec = seq(0,200,by=10),
            hcr.type = "IceHockeyStick",
            hcr.settings = list(ice.rp1=50,
                                ice.rp2=80,
                                ice.rate=50
            ))

calcHCR(run.vec = seq(0,200,by=10),
            hcr.type = "FieldHockeyStickwFloor",
            hcr.settings = list(fieldwfloor.rp1=40,
                                fieldwfloor.rate=60,
                                fieldwfloor.floor = 10
            ))







