
.onAttach <- function(libname, pkgname) {
  madrat::madratAttach(c(pkgname, "GDPuc", "mrremind", "mrcommons", "mrdrivers"))
  
}

.onDetach <- function(libpath) {
  madrat::madratDetach(c(libpath, "GDPuc", "mrremind", "mrcommons", "mrdrivers"))
}

# redirect standard messaging functions to vcat
cat     <- function(...) vcat(1, ...)
message <- function(...) vcat(1, ...)
warning <- function(...) vcat(0, ...)
stop    <- function(...) vcat(-1, ...)
