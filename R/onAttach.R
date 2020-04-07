# start up message

.onAttach = function(libname, pkgname) {
  packageStartupMessage("**********\nThis is the Survey Solutions COVID19 toolbox.
  It requires the latest version of R, R-Studio, Shiny, and Flexdashboard
                        to be installed.\n\nFor MC supported operations it will use\n\n",
                        future::availableCores(), " cores.\n\n**********")
}
