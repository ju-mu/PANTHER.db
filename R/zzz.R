

.onLoad <- function(libname, pkgname){

  dbfile <- suppressMessages(AnnotationHub()[["AH75194", verbose=FALSE]])

  db <- new(
    getClassDef(Class = pkgname, where = getNamespace(name = pkgname)),
    conn = dbConnect(
      SQLite(), dbfile, cache_size = 64000, synchronous = "off"
    )
  )

  ns <- asNamespace(pkgname)
  assign(pkgname, db, envir=ns)
  namespaceExport(ns, pkgname)

  db$.initializePANTHERdb()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("PANTHER.db version ", packageVersion(pkgname))
}

.onUnload <- function(libpath) {
  PANTHER.db$finalize()
}

