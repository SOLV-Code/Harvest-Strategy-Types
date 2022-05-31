is_installed <- function(mypkg){ is.element(mypkg, installed.packages()[,1])}



#' @title Package load or install and load
#'
#' @description For installing and/or loading R packages
#'
#' @param package_names Character string of length one or more. The package name.
#'
#' @return Nothing returned.
#' @export
#'
#' @examples
load_or_install <- function(package_names){
	# function to load/install required packages
	for(package_name in package_names){
		if(!is_installed(package_name)){install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")}
		library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
	}
}#END load_or_install



include.dependencies <- function(){
#' @title Internal handling of dependencies
#'
#' @description Seems like this is needed in addition to the import list in the DESCRIPTION file, so keeping them all together in one place.
#' @import dplyr
#' @import markdown
#' @import meboot
#' @import moments
#' @import rmarkdown
#' @import rstanarm
#' @import stringi
#' @return Nothing returned.
#' @export


print("including dependencies")


} # end function include.dependencies
