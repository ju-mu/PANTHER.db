

library(Homo.sapiens)



test_select <- function(){
	checkEquals(keytypes(PANTHER.db),c("FAMILY_ID","GOSLIM_ID","CLASS_ID","PATHWAY_ID","COMPONENT_ID","UNIPROT","SPECIES","ENTREZ"))
	checkEquals(columns(PANTHER.db),c('FAMILY_ID','GOSLIM_ID','GOSLIM_TERM','UNIPROT','SPECIES','FAMILY_TERM','SUBFAMILY_TERM','CLASS_ID','CLASS_TERM','PATHWAY_ID','PATHWAY_TERM','COMPONENT_ID','COMPONENT_TERM','EVIDENCE','EVIDENCE_TYPE','CONFIDENCE_CODE','ENTREZ'))

	keytypes(Homo.sapiens)
	
	
	select(PANTHER.db,keys="O00267",columns="ENTREZ",keytype="UNIPROT")
	select(Homo.sapiens,keys="O00267",columns="ENTREZID",keytype="UNIPROT")
}


