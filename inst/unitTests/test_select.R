

#### Prepare reference queries from PANTHER webservice 18.09.2013 ####
#results will be uesd for Unit Tests
if(F){
  library(RCurl)
  
  url <- 'http://www.pantherdb.org/webservices/garuda/search.jsp?keyword='
  ids <- keys(PANTHER.db,"UNIPROT")[c(50,70,65490)];ids
  
  fullUrl <- paste0(url,paste(ids, collapse="|"),'&listType=gene&type=getList')
  gs<-read.delim(fullUrl,stringsAsFactors=F)$Gene.Symbol
  
  fullUrl <- paste0(url,paste(ids, collapse="+or+"),'&listType=pathway&type=getList')
  path_acc<-read.delim(fullUrl,stringsAsFactors=F)$Pathway.Accession
  
  fullUrl <- paste0(url,paste(ids, collapse="+or+"),'&listType=family&type=getList')
  fam_acc<-read.delim(fullUrl,stringsAsFactors=F)$Family.Accession
  
  fullUrl <- paste0(url,paste(ids, collapse="+or+"),'&listType=category&type=getList')
  cat_acc<-read.delim(fullUrl,stringsAsFactors=F)$Category.Accession
}
### ###


test_structure <- function(){
  checkEquals(keytypes(PANTHER.db),c("FAMILY_ID","GOSLIM_ID","CLASS_ID","PATHWAY_ID","COMPONENT_ID","UNIPROT","SPECIES","ENTREZ"))
	checkEquals(columns(PANTHER.db),c('FAMILY_ID','GOSLIM_ID','GOSLIM_TERM','UNIPROT','SPECIES','FAMILY_TERM','SUBFAMILY_TERM','CLASS_ID','CLASS_TERM','PATHWAY_ID','PATHWAY_TERM','COMPONENT_ID','COMPONENT_TERM','EVIDENCE','EVIDENCE_TYPE','CONFIDENCE_CODE','ENTREZ'))
}


test_select_vs_webquery <- function(){
	#webquery: "http://www.pantherdb.org/webservices/garuda/search.jsp?keyword=PTHR10015:SF129&listType=category&type=getList"
  ids<-"PTHR10015:SF129"
  webquery_res<-sort(c('GO:0002376','GO:0003676','GO:0003677','GO:0003700','GO:0005488','GO:0006139','GO:0006350','GO:0006357','GO:0006366','GO:0006950','GO:0008152','GO:0030528','GO:0044238','GO:0050896','PC00171','PC00218'))
  select_res<-sort(c(select(PANTHER.db,keys=ids,columns="GOSLIM_ID")$GOSLIM_ID,select(PANTHER.db,keys=ids,columns="CLASS_ID")$CLASS_ID))
  checkEquals(webquery_res,select_res)
  
  #webquery: "http://www.pantherdb.org/webservices/garuda/search.jsp?keyword=PC00015&listType=family&type=getList"
  ids<-"PC00015"
  webquery_res<-sort(c('PTHR11352','PTHR11352:SF0','PTHR13321:SF0','PTHR15268:SF7','PTHR15268:SF14'))
  select_res<-sort(select(PANTHER.db,keys=ids,columns="FAMILY_ID",keytype="CLASS_ID")$FAMILY_ID)
  checkEquals(webquery_res,select_res)
  
  #webquery: "http://www.pantherdb.org/webservices/garuda/search.jsp?keyword=A0JM86+or+A0JMJ1+or+F1PB63&listType=pathway&type=getList"
  ids<-c("A0JM86","A0JMJ1","F1PB63")
  webquery_res<-sort(c("P00020","P00029","P06664"))
  select_res<-sort(select(PANTHER.db,keys=ids,columns="PATHWAY_ID",keytype="UNIPROT")$PATHWAY_ID)
  checkEquals(webquery_res,select_res)

}



