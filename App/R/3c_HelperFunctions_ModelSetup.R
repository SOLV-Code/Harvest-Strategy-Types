# This script contains functions for setting-up and checking the model components

# Inventory
# - function to verify 32bit vs. 64bit installation match of R and Java  - function created and applied in this script
# - function to load/install R packages -> moved to R/utils.R


# Source all the functions in the new modules

source.modules <- function(path.use){

	file.list <- list.files(path=path.use,pattern=".R") # get all .R files 
	module.list <- file.list[grepl("Module_",file.list)]
	for(module.source in module.list){
		print(paste("Sourcing: ",module.source))
		source(paste(path.use,module.source,sep="/"))}
}




checkJava <- function(){
# -----------------------
# need to run this first, before trying to install rJava package

#  32bit vs. 64bit
# https://stackoverflow.com/questions/2464721/r-looking-for-the-wrong-java-version
# https://stackoverflow.com/questions/28133360/rjava-is-not-picking-up-the-correct-java-version/32962637
#https://stackoverflow.com/questions/18091614/how-can-i-know-if-r-is-running-on-64-bits-versus-32/18093936
#https://stackoverflow.com/questions/2062020/how-can-i-tell-if-im-running-in-64-bit-jvm-or-32-bit-jvm-from-within-a-program/2062263#2062263
# ?system did the trick in the end!

# Java version
# this seems to be finicky across OS etc. need to do some testing
java.systemcheck <- system("java  -version",intern=TRUE)[3]
java.64 <- grepl("64-Bit",java.systemcheck)
if(java.64){java.version <- "64-Bit"}
if(!java.64){java.version <- "32-Bit"}


# R version
pointer <- .Machine$sizeof.pointer
if(pointer == 4){r.version <- "32-Bit"}
if(pointer == 8){r.version <- "64-Bit"}

# compare R and Java
if(r.version==java.version){
						print("Your R version matches your Java version (32-Bit vs. 64-Bit)")
						check.out <- TRUE
						}


if(r.version!=java.version){
						warning("Your R version DOES NOT MATCH your Java version (32-Bit vs. 64-Bit).")
						warning("Either start a different version of R or install a different version of Java")
						check.out <- FALSE
					}
return(check.out)
}









