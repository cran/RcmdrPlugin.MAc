\name{RcmdrPlugin.MAc-package}
\alias{RcmdrPlugin.MAc-package}
\docType{package}
\title{Rcmdr Plug-in for Meta-Analysis with Correlations (MAc)
}
\description{ This package provides an Rcmdr plug-in for the \bold{MAc} package, which is useful for conducting meta-analyses with correlational data.  This Rcmdr plug-in package will run all of the meta-analytic functions from the \bold{MAc} package though a familiar Graphical User Interface (GUI) environment (e.g., SPSS). For more details regarding the \bold{MAc} package, please see the \bold{MAc} link on the CRAN website \url{http://CRAN.R-project.org/package=MAc}
\tabular{ll}{
Package: \tab RcmdrPlugin.MAc-package\cr
Type: \tab Package\cr
Version: \tab 1.0.4\cr
Date: \tab 2010-02-21\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}

}
\author{AC Del Re & William T. Hoyt

Maintainer: AC Del Re <acdelre@gmail.com>
}
\references{ AC Del Re & William T. Hoyt (2010). \emph{MAc: Meta-Analysis with Correlations}. R package version 1.0.4.
  http://CRAN.R-project.org/package=MAc
 
}
\examples{
# Implementing the RcmdrPlugin.MAc package:

# 1. Begin the meta-analysis project using Excel (or a similar program) and be sure to have a column for id (study id),
#    r (correlation coefficients, and n (sample size). All other variables (i.e., moderators) can be have any 
#    name that is desired. Note that this package requires the study id, correlation coefficient, and sample size of the 
#    studies in the Excel workbook (or R data.frame) to be named exactly as stated above or the functions will not output 
#     data. 
# 2.  Import the worksheet or data into R:
# 2a. Save main data file (excel or spss) to a .csv file [e.g., see save options in excel]
# 2b. Import the .csv data file using setwd() with all relevant meta data into R by setting the R working directory 
#     to the location of your .csv data file. e.g.: setwd("C:/Users/Desktop/R"). Or, if preferred, use the R menu:
#     File --> Change Dir --> (location of .csv file) 
# 2c. mydata <- read.csv("MetaData.csv",header=TRUE,na.strings="") 
#     Name the object (mydata) as desired but be sure to match the name of the .csv in the import function exactly as named 
#     in the .csv file. If the file successfully imports there will be no feedback about the import. Checking to see if the
#     file imported successfully, at the command prompt type ls() and the name of the file should appear on the screen.
# 3a. Download the RcmdrPlugin.MAc package (and other relevant packages if they are not automatically downloaded). To do this,
#     open R locally and look to the top where the pull-down menu resides. Click Packages --> (select a CRAN mirror--find any
#     mirror that is in relatively close proximity, if possible) --> (scroll down to RcmdrPlugin.MAc and click). 
# 3b. After the package(s) are downloaded, type library(RcmdrPlugin.MAc) at the command prompt. This will load the Rcmdr 
#     Graphical User Interface (GUI) which is similar to the SPSS-style pull down menu environment. The MAc package (Meta-
#     Analysis with Correlations) will also be loaded with its own pull-down menu on the top right of the program.
#     From here, the researcher can use the pull-down menu to run all analyses. The first step in analyzing data is to 
#     click to the right of 'Data set' (top left) that says <No active dataset> and click on the data file that was just
#     imported. If the data file that was just imported does not show up then it was not imported. Otherwise, everything 
#     should be ready to run! 
}  
\keyword{ package }
\seealso{
\bold{MAc} package:  \url{http://CRAN.R-project.org/package=MAc}
}