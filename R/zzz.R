

datacache <- new.env(hash=TRUE, parent=emptyenv())

PANTHER <- function() showQCData("PANTHER.db", datacache)
PANTHER_dbconn <- function() dbconn(datacache)
PANTHER_dbfile <- function() dbfile(datacache)
PANTHER_dbschema <- function() cat(readLines(system.file("extdata/PANTHER_DB.sql",package="PANTHER.db")), sep = "\n")
PANTHER_dbInfo <- function() dbInfo(datacache)

loadDb2<-function (file, dbType, dbPackage, ...) 
{
  #require(dbPackage, character.only = TRUE)
  db <- getClassDef(dbType, where = getNamespace(dbPackage))
  new(db, conn = dbConnect(SQLite(), file), ...)
}


.onLoad <- function(libname, pkgname)
{

    dbfile<-system.file("extdata", paste(pkgname,".sqlite",sep=""), package=pkgname, lib.loc=libname,mustWork=T)
    assign("dbfile", dbfile, envir=datacache)
    dbconn <- dbFileConnect(dbfile)
    assign("dbconn", dbconn, envir=datacache)
      
    db <- loadDb2(file=dbfile,dbType=pkgname,dbPackage=pkgname)

    dbNewname<-AnnotationDbi:::dbObjectName(pkgname,pkgname)
    ns <- asNamespace(pkgname)
    assign(dbNewname, db, envir=ns)
    namespaceExport(ns, dbNewname)
    db$.setAvailableSpecies()
	
	packageStartupMessage("PANTHER.db version ", packageDescription(pkgname, fields="Version"))
	
}

.onUnload <- function(libpath)
{

}



