

datacache <- new.env(hash=TRUE, parent=emptyenv())

PANTHER <- function() showQCData("PANTHER", datacache)
PANTHER_dbconn <- function() dbconn(datacache)
PANTHER_dbfile <- function() dbfile(datacache)
PANTHER_dbschema <- function(file="", show.indices=FALSE) dbschema(datacache, file=file, show.indices=show.indices)
PANTHER_dbInfo <- function() dbInfo(datacache)

loadDb2<-function (file, dbType, dbPackage, ...) 
{
  #require(dbPackage, character.only = TRUE)
  db <- getClassDef(dbType, where = getNamespace(dbPackage))
  new(db, conn = dbConnect(SQLite(), file), ...)
}


.onLoad <- function(libname, pkgname)
{
    
    sPkgname <- sub(".db$","",pkgname)
    dbfile<-system.file("extdata", paste(pkgname,".sqlite",sep=""), package=pkgname, lib.loc=libname,mustWork=T)
    assign("dbfile", dbfile, envir=datacache)
    dbconn <- dbFileConnect(dbfile)
    assign("dbconn", dbconn, envir=datacache)
    
    
    db <- loadDb2(file=dbfile,dbType=sPkgname,dbPackage=pkgname)

    dbNewname<-AnnotationDbi:::dbObjectName(pkgname,sPkgname)
    ns <- asNamespace(pkgname)
    assign(dbNewname, db, envir=ns)
    namespaceExport(ns, dbNewname)
    db$.setAvailableSpecies()
}

.onUnload <- function(libpath)
{

}
getMethod("loadDb",c("character","character","character"))
