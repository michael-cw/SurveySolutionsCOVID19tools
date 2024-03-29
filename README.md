# Survey Solutions COVID 19 survey tools

This is a repository for the Survey Solutions toolbox to support survey implementation in response to the COVID 19 data collection initiative.

All its modules come with a user interface and are either based on R's shiny or flexdashboard. 

To run it you first need to 

* install R: https://cran.r-project.org/mirrors.html

* Second you require RStudio: https://rstudio.com/products/rstudio/download/

* And third you need to open Rstudio and run the following command:

```
devtools::install_github("michael-cw/SurveySolutionsCOVID19tools")

```
<br>

* or in case you also want the vignettes include, please use:
```
devtools::install_github("michael-cw/SurveySolutionsCOVID19tools", build_vignettes = T)

```
<br>
This will install the package from the github repository. Packages which are not available will be installed during the installation.

After finishing the installation process load the library. 

Now you can run the individual functions.
<br>
```
library(SurveySolutionsCOVID19tools)

```

### Sampling

To start the sampling module, run:
<br>
```
suso_covid19_samplingApp()
```
<br>


This will open the sampling dashboard in your default browser.

<br><br>

### REGENESEES

Since this package also uses the REGENESEES package, which is not on CRAN, you need to download this package from the following link prior to the installation of the tool box.
https://www.istat.it/en/methods-and-tools/methods-and-it-tools/process/processing-tools/regenesees

***

To find information on the World Bank's Survey Solutions CASS have a look on these pages:

* Survey Solutions Support Page: https://support.mysurvey.solutions/
* Survey Solutions Server request: https://mysurvey.solutions/

To find information on the API syntax, visit your own servers API documentation or got to:

* https://demo.mysurvey.solutions//apidocs/index#!

*****
#### For users with absolutely zero knowledge of R and Rstudio

As since recently also users with zero knowledge about R and Rstudio try to install this package, i added here a step-by-step guide on how to install from github: https://www.displayr.com/installing-r-packages-from-github/

However in general i recommend to have a least some basic familiarity with R and the R environment when using packages from github, as this also facilitates trouble shooting. In addtion it allows you also to raise useful issues and feedback. 

A good starting point is here: https://rstudio.cloud/learn/primers

Another starting point is here: https://education.rstudio.com/learn/beginner/

And there are plenty of other resources out there, which allow you to learn a very interesting and useful (statistical) programming language.
