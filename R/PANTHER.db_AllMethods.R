
.species <- function(x){
  x$.species
} 
setMethod("species", "PANTHER.db", .species)

#emulate S4 hehavior
setReplaceMethod("species", "PANTHER.db",
  function(x, value) {
    x$.setSpecies(value)
    x
  }
) 


setMethod("show", 
  signature(object="PANTHER.db"), 
  definition=function (object) 
  {
    cat(class(object), "object:\n")
    metadata <- metadata(object)
    metadata$value[grep("SPECIES",metadata$name)]<-object$.species
    for (i in seq_len(nrow(metadata))) {
      cat("| ", metadata[i, "name"], ": ", metadata[i, "value"], 
          "\n", sep = "")
    }
  }
)

.resetSpecies <- function(x){
  x$.resetSpecies()
  cat(species(x))
} 
setMethod("resetSpecies", "PANTHER.db", .resetSpecies)


.cols <- function(x){
  c("FAMILY_ID","GOSLIM_ID","GOSLIM_TERM","UNIPROT","SPECIES","FAMILY_TERM","SUBFAMILY_TERM","CLASS_ID","CLASS_TERM","PATHWAY_ID","PATHWAY_TERM","COMPONENT_ID","COMPONENT_TERM","EVIDENCE","EVIDENCE_TYPE","CONFIDENCE_CODE","ENTREZ")
} 
setMethod("columns", "PANTHER.db", .cols)

.traverseClassTree<-function(x,query,scope){
  scopes<-c("CHILD","PARENT","ANCESTOR","OFFSPRING")
  if(length(scope)>1)stop("Only one scope argument allowed")
  if(scope %in% scopes){
    scope<-tolower(scope)
  } else {
    stop(paste0("scope must be one of the following:\n",paste(scopes,collapse="|")))
  }
  query<-paste0("'",paste(query,collapse="','"),"'")

  dbGetQuery(x$conn,sprintf( "SELECT class_id from protein_class_tree WHERE class_tree_id IN (SELECT pf.%s_class_id FROM protein_class_%s as pf NATURAL JOIN protein_class_tree as pc WHERE pc.class_id IN (%s)) ",scope,scope,query))$class_id
    
}
setMethod("traverseClassTree", signature(x="PANTHER.db",query="character",scope="character"),.traverseClassTree)

type2table <- function(x){
  cls<-c(x$.ref_table,"go_slim","go_slim","uniprot","uniprot",x$.ref_table,x$.ref_table,"protein_class","protein_class","panther_go","panther_go",rep("panther_go_component",5),"entrez")
  names(cls)<-columns(x)
  cls
} 
setMethod(".type2table", "PANTHER.db", type2table)

.availablePantherSpecies <- function(x)
{
  res<-dbGetQuery(x$conn, "SELECT * from species")
  colnames(res)<-c("PANTHER Species","UNIPROT Species","UNIPROT Taxon ID")
  res
}
setMethod("availablePantherSpecies", "PANTHER.db", .availablePantherSpecies)


type2col <- function(x){
  cls<-c("family_id","goslim_id","ontology","uniprot_id","species","family_term","subfamily_term","class_id","class_term","go_id","go_term","component_go_id","component_go_term","evidence","evidence_type","confidence_code","entrez_id")
  names(cls)<-columns(x)
  cls
} 
setMethod(".type2col", "PANTHER.db", type2col)

.keytypes <- function(x){
  c("FAMILY_ID","GOSLIM_ID","CLASS_ID","PATHWAY_ID","COMPONENT_ID","UNIPROT","SPECIES","ENTREZ")
} 
setMethod("keytypes", "PANTHER.db", .keytypes)


.getTableNames <- function(x){
  alltabs <- dbListTables(x$conn)
  toupper(alltabs[!alltabs %in% c("map_counts","map_metadata","metadata")])
} 
setMethod("getTableNames", "PANTHER.db", .getTableNames)

.keys <- function(x, keytype)
{
  t2t<-.type2table(x)
  t2c<-.type2col(x)
  if(!keytype %in% keytypes(x))stop(paste0("keytype must be one of the following:\n",paste(keytypes(x),collapse="|")))
  joinc<-if(x$.ref_table!=t2t[keytype]) sprintf("NATURAL JOIN %s",x$.ref_table) else ""
  dbGetQuery(x$conn, sprintf("SELECT DISTINCT(%s) FROM %s %s ORDER BY %s",t2c[keytype],t2t[keytype],joinc,t2c[keytype]))[,1]#join families to keep species specificity!!
}
setMethod("keys", "PANTHER.db",function(x, keytype){ if (missing(keytype)) keytype <- "FAMILY_ID";.keys(x, keytype)})


.select <- function(x, keys, columns, keytype){
  t2t<-.type2table(x)
  t2c<-.type2col(x)
  if(length(keytype)>1)stop("keytype can not be more than one")
  if(any(!columns %in% columns(x)))stop(paste0("column must be one of the following:\n",paste(columns(x),collapse="|")))
  if(!keytype %in% keytypes(x))stop(paste0("keytype must be one of the following:\n",paste(keytypes(x),collapse="|")))
  columns<-unique(columns)
  mcols<-if(keytype %in% names(t2c[columns]))t2c[columns] else c(t2c[keytype],t2c[columns])
  mtabs<-if(keytype %in% names(t2c[columns]))unique(t2t[columns][!t2t[columns]==x$.ref_table]) else unique(c(t2t[keytype],t2t[columns][!t2t[columns]==x$.ref_table]))
  mtabs<-mtabs[!mtabs == "panther_families"]
  
  join_clause<-if(!length(mtabs)) "" else paste("NATURAL JOIN",paste(mtabs,collapse=" NATURAL JOIN "))
  mkeys<-paste0("'",paste(keys,collapse="','"),"'")
  
  res<-dbGetQuery(x$conn, sprintf("SELECT %s FROM %s %s WHERE %s IN (%s) ORDER BY %s",paste(mcols,collapse=","),x$.ref_table,join_clause,t2c[keytype],mkeys,t2c[keytype]))
  res<-res[!duplicated(res),]
  colnames(res)<-names(mcols)
  res
}
setMethod("select", "PANTHER.db",function(x, keys, columns, keytype){ if (missing(keytype)) keytype <- "FAMILY_ID";.select(x, keys, columns, keytype)})




