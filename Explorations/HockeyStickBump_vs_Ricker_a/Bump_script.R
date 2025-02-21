# GENERATE PLOTS FOR ICE HOCKEY STICK HCR
# SET AT Sgen, 80% Smsy, AND Umsy
# AT DIFFERENT PRODUCTIVITY LEVELS


# Tried {animation} package but resolution was poor and noe all the plot components showed up,
# the gif wasn't animated - > must have hit an error
# this example worked:
# https://www.programmingr.com/content/animations-r/#:~:text=Fortunately%2C%20creating%20animated%20graphs%20in%20R%20is%20fairly,graph%20with%20a%20couple%20of%20bells%20and%20whistles


# Instead: Just make a series of high-res png files, then feed them into
# a an on-line conversion tool.
# https://ezgif.com/maker worked well on the first try.


# Settings

install.packages <- FALSE
smax.use <- 2000
a.vec <- c(1.7,1.85,2,2.2,2.5,3,3,4,5,10,20)

# install packages
if(install.packages){
  install.packages("devtools") # Install the devtools package
  library(devtools) # Load the devtools package.
  install_github("SOLV-Code/RapidRicker", dependencies = TRUE,
                 build_vignettes = FALSE)
  install.packages("tidyverse")

  install.packages('animation', repos = 'https://xran.yihui.org')

}

# load required packages
library(RapidRicker)
library(tidyverse)


# Generate data set of alternative Ricker a (productivity)
# and calculate standard benchmarks for each


alt.pars.df <- data.frame(
  alpha = a.vec,
  ln.alpha = log(a.vec),
  beta= 1/smax.use
)

alt.pars.df


# calculate standard biological benchmarks

bm.calc.int <- calcRickerSmsy(alt.pars.df ,
                              method ="Scheuerell2016",sr.scale =1, out.type = "Full")


bm.calc.out <- calcRickerSgen(X = bm.calc.int,
                              method = "Connorsetal2022",
                              sr.scale = 1, out.type = "Full")
bm.calc.out



# Calculate HCR across range of run sizes

run.vec <- round(seq(0,smax.use*1.2,length = 300))
run.vec

for(i in 1:dim(bm.calc.out)[1]){


bm.sub <- bm.calc.out[i,]
bm.sub

hcr.sub <- calcHCR(run.vec = run.vec,
                   hcr.type = "IceHockeyStick" ,
                   hcr.settings = list(ice.rp1=bm.sub$Sgen,
                                       ice.rp2=0.8 * bm.sub$Smsy,
                                       ice.rate=bm.sub$Umsy*100)) %>%
            mutate(index = i,alpha = bm.sub$alpha,
                   ln.alpha = bm.sub$ln.alpha,
                   beta = bm.sub$beta,
                   Smsy = bm.sub$Smsy,
                   Smsy80p = bm.sub$Smsy*0.8,
                   Sgen = bm.sub$Sgen,
                   Umsy = bm.sub$Umsy)
#head(hcr.sub)

if(i == 1){hcr.out <- hcr.sub}
if(i > 1){hcr.out <- bind_rows(hcr.out,hcr.sub)}

} # end looping through alphas

head(hcr.out)

write_csv(hcr.out,"Explorations/HockeyStickBump_vs_Ricker_a/Bump_Full_Output.csv")


# Smsy and Sgen Plot

plot(bm.calc.out$ln.alpha,ylim = c(0,smax.use),
    bm.calc.out$Smsy,type = "o",las=1,
    pch=21,col="darkblue",bg="lightblue",bty="n",
    xlab="Productivity Parameter: ln(Ricker a)",
    ylab = "Spawner Abundance",
    main = "A) Change in Abundance Benchmarks")
lines(bm.calc.out$ln.alpha,
      bm.calc.out$Sgen,type = "o",las=1,
      pch=22,col="darkblue",bg="firebrick1")
legend("topleft",legend = c("Smsy","Sgen"),col="darkblue",lty=1,
       pch=c(21,22),pt.bg = c("darkblue","red"),bty="n")

# Umsy Plot

plot(bm.calc.out$ln.alpha,ylim = c(0,100),
     bm.calc.out$Umsy*100,type = "o",las=1,
     pch=21,col="darkblue",bg="darkblue",bty="n",
     xlab="Productivity Parameter: ln(Ricker a)",
     ylab = "ER (%)",
     main = "A) Change in Removal Benchmark")
legend("topleft",legend = "Umsy",col="darkblue",lty=1,
       pch=21,pt.bg = "darkblue",bty="n")



# HCR plot: Target ER version

plot(1:5,1:5,type="n",las=1,ylim=c(0,100),
     xlim=c(0, max(run.vec)),bty="n",
     xlab = "Run Size",ylab = "Target ER (%)"
)


for(i in sort(unique(hcr.out$index))){
hcr.plot <- hcr.out %>% dplyr::filter(index==i)
lines(hcr.plot$Run,hcr.plot$ER)
}

# HCR plot: Target Spn version -> These are not really clear

plot(1:5,1:5,type="n",las=1,ylim=c(0,max(hcr.out$Spn)),
     xlim=c(0, max(run.vec)),
     bty="n",
     xlab = "Run Size",ylab = "Target Spn"
)

for(i in sort(unique(hcr.out$index))){
  hcr.plot <- hcr.out %>% dplyr::filter(index==i)

  lines(hcr.plot$Run,hcr.plot$Spn,lwd=2,col="darkgrey")
  if(i==1){lines(hcr.plot$Run,hcr.plot$Spn,lwd=3,col="red")}
  if(i==max(hcr.out$index)){lines(hcr.plot$Run,hcr.plot$Spn,lwd=3,col="darkblue")}

}


# ---------------------------------------------------------------------
# Create images for multi-panel animation


for(i in 1:max(hcr.out$index)){


png(filename = paste0("Explorations/HockeyStickBump_vs_Ricker_a/Plots/IceHockeyBump_",i,".png"),
    width = 480*4.5, height = 480*4,
    units = "px", pointsize = 14*3.5, bg = "white",  res = NA)




layout(matrix(1:4,ncol=2,byrow=TRUE),heights = c(1,1))
#par(mai=c(0.7,4,2.2,0.3))


# Smsy and Sgen Plot

plot(bm.calc.out$ln.alpha,ylim = c(0,smax.use),
     bm.calc.out$Smsy,type = "o",las=1,
     pch=21,col="darkblue",bg="lightblue",bty="n",
     xlab="Productivity Parameter: ln(Ricker a)",
     ylab = "Spawner Abundance",
     main = "A) Change in Abundance Benchmarks",
     col.main = "darkblue")


rect(bm.calc.out$ln.alpha[i]-0.04,0,
     bm.calc.out$ln.alpha[i]+0.04,smax.use,
     col="red",border="red")

lines(bm.calc.out$ln.alpha,ylim = c(0,smax.use),
       bm.calc.out$Smsy,type = "o",las=1,
       pch=21,col="darkblue",bg="lightblue")

lines(bm.calc.out$ln.alpha,
      bm.calc.out$Sgen,type = "o",las=1,
      pch=22,col="darkblue",bg="lightgrey")


legend("topleft",legend = c("Smsy","Sgen"),col="darkblue",lty=1,
       pch=c(21,22),pt.bg = c("lightblue","lightgrey"),bty="o",border = "white",
       box.col="white")




# Umsy Plot

plot(bm.calc.out$ln.alpha,bm.calc.out$Umsy*100,
     ylim = c(0,100),type = "o",las=1,
     pch=21,col="darkblue",bg="darkblue",bty="n",
     xlab="Productivity Parameter: ln(Ricker a)",
     ylab = "ER (%)",
     main = "B) Change in Removal Benchmark",
     col.main = "darkblue")

rect(bm.calc.out$ln.alpha[i]-0.04,0,
     bm.calc.out$ln.alpha[i]+0.04,smax.use,
     col="red",border="red")

lines(bm.calc.out$ln.alpha, bm.calc.out$Umsy*100,type = "o",
  pch=21,col="darkblue",bg="darkblue")

legend("topleft",legend = "Umsy",col="darkblue",lty=1,
       pch=21,pt.bg = "darkblue",bty="o",border = "white",
       box.col="white")


# HCR plot: Target ER version

plot(1:5,1:5,type="n",las=1,ylim=c(0,100),
     xlim=c(0, max(run.vec)),bty="n",
     xlab = "Run Size",ylab = "Target ER (%)",
     main = "C) Harvest Rule - ER Target",
     col.main = "darkblue"
)


for(j in sort(unique(hcr.out$index))){
  hcr.plot <- hcr.out %>% dplyr::filter(index==j)
  lines(hcr.plot$Run,hcr.plot$ER,lwd=4,col="darkgrey")
}

hcr.plot <- hcr.out %>% dplyr::filter(index==i)
lines(hcr.plot$Run,hcr.plot$ER,lwd=9,col="red")


# SPN VERSION

plot(1:5,1:5,type="n",las=1,ylim=c(0,max(hcr.out$Spn)),
     xlim=c(0, max(run.vec)),
     bty="n",
     xlab = "Run Size",ylab = "Target Spn",
     main = "D) Harvest Rule - Spawner Target",
     col.main = "darkblue"
)

for(j in sort(unique(hcr.out$index))){
  hcr.plot <- hcr.out %>% dplyr::filter(index==j)
  lines(hcr.plot$Run,hcr.plot$Spn,lwd=4,col="darkgrey")
}

hcr.plot <- hcr.out %>% dplyr::filter(index==i)
lines(hcr.plot$Run,hcr.plot$Spn,lwd=9,col="red")


title(main = "Effect of Productivity on BM-based Hockey Stick Rule",outer=TRUE,
      col.main="darkblue",line=-1,xpd=NA)


dev.off()

} # end looping through Ricker a



