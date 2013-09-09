#### Functions to build the sqlite DB for Package PANTHER.db ####
# Generated for PANTHER version 8.2
# All source files from the ftp at ftp://ftp.pantherdb.org//

library(RCurl)
library("R.utils")
library(RSQLite)

#### SET THE TARGET FOLDER FOR ALL FILES FROM PANTHER USED FOR db BUILDING ####
home_folder<-"~/Downloads/"
source("scheme.txt")
### ###

#### RESTRICT db TO SPECIES SUPPORTED BY AnnotationDb ####
#species present in AnnotationDb
bioc_sql<-list.files(system.file("DBschemas/schemas_2.1",package="AnnotationDbi"))
bioc_sql<-bioc_sql[grep("_DB.sql$",bioc_sql)]
bioc_sql<-bioc_sql[grep("CHIP_DB.sql$|^KEGG_|^GO_|^INPARANOID_|^PFAM_",bioc_sql,invert=T)]
bioc_sql.orgs<-sub("_DB.sql","",bioc_sql)

#species IDs: list bioconductor species id -> ( panther species ID , hmm sequence filename suffix, internal taxonomy file)
panther_species<-list("ANOPHELES"=c("ANOGA","mosquito","anopheles_gambiae"),"ARABIDOPSIS"=c("ARATH","arabidopsis","arabidopsis_thaliana"),"BOVINE"=c("BOVIN","cattle","bos_taurus"),"CANINE"=c("CANFA","dog","canis_familiaris"),"CHICKEN"=c("CHICK","chicken","gallus_gallus"),"CHIMP"=c("PANTR","chimp","pan_troglodytes"),"COELICOLOR"=c("STRCO","streptomyces","streptomyces_coelicolor"),"ECOLI"=c("ECOLI","ecoli","escherichia_coli"),"FLY"=c("DROME","fly","drosophila_melanogaster"),"HUMAN"=c("HUMAN","human","homo_sapiens"),"MALARIA"=c("PLAFA","plasmodium","plasmodium_falciparum"),"MOUSE"=c("MOUSE","mouse","mus_musculus"),"PIG"=NA,"RAT"=c("RAT","rat","rattus_norvegicus"),"RHESUS"=NA,"WORM"=c("CAEEL","worm","caenorhabditis_elegans"),"XENOPUS"=c("XENTR","frog","xenopus_tropicalis"),"YEAST"=c("YEAST","yeast","saccharomyces_cerevisiae"),"ZEBRAFISH"=c("DANRE","zfin","danio_rerio"))
org_bioc2panther<-sapply(panther_species,"[",1)
org_bioc2panther_fn<-sapply(panther_species,"[",2)

stopifnot(all(bioc_sql.orgs == names(panther_species)))
### ###

#### WEB INTERFACE ####

# uri <- 'http://www.pantherdb.org/webservices/garuda/search.jsp?keyword='
# ids <- panther_hmm$ProteinClass[1]
# fullUri <- paste0(uri,paste(ids, collapse="|"),'&listType=gene&type=getList')
# read.delim(fullUri )
# fullUri <- paste0(uri,paste(ids, collapse="+or+"),'&listType=pathway&type=getList')
# read.delim(fullUri )
# fullUri <- paste0(uri,paste(ids, collapse="+or+"),'&listType=family&type=getList')
# read.delim(fullUri )
# fullUri <- paste0(uri,paste(ids, collapse="+or+"),'&listType=category&type=getList')
# read.delim(fullUri )
### ###


######## ftp download #################

# Source Folders 
src.hmm<-"ftp.pantherdb.org/hmm_classifications/8.1/"
src.pathway<-"ftp.pantherdb.org/pathway/3.2.1/"
src.seq<-"ftp.pantherdb.org/sequence_classifications/8.1/PANTHER_Sequence_Classification_files/"
#src.ortho<-"ftp.pantherdb.org/ortholog/8.1/"#not needed!
src.class<-"http://data.pantherdb.org/PANTHER8.1//ontology/Protein_Class_7.0"
src.class_rel<-"http://data.pantherdb.org/PANTHER8.1//ontology/Protein_class_relationship"

sub("Protein_Class_7.0","",src.class,fixed=T)
# Target Folders 
dir.hmm<-file.path(home_folder,src.hmm)
dir.pathway<-file.path(home_folder,src.pathway)
dir.seq<-file.path(home_folder,src.seq)
dir.class<-file.path(home_folder,src.class)
dir.class_rel<-file.path(home_folder,src.class_rel)
#dir.ortho<-file.path(home_folder,src.ortho)

#download files
if(T){
  dir.create(paste(dir.hmm, sep=.Platform$file.sep),recursive=TRUE, mode="0755")
  dir.create(paste(dir.pathway, sep=.Platform$file.sep),recursive=TRUE, mode="0755")
  dir.create(paste(dir.seq, sep=.Platform$file.sep),recursive=TRUE, mode="0755")
  dir.create(file.path(paste(sub("Protein_Class_7.0","",dir.class,fixed=T), sep=.Platform$file.sep)),recursive=TRUE, mode="0755")
  filenames <- unlist(strsplit(getURL(src.hmm, dirlistonly = TRUE), "\n"))
  sapply(filenames,function(fn){download.file(file.path("ftp:/",src.hmm,fn),file.path(dir.hmm,fn), cacheOK = TRUE, quiet = FALSE,method="wget",extra=c("-nv","-t 3","-nc","-T 5"))})
  filenames <- unlist(strsplit(getURL(src.pathway, dirlistonly = TRUE), "\n"))
  sapply(filenames[grep("LICENSE|README|SequenceAssociationPathway",filenames)],function(fn){download.file(file.path("ftp:/",src.pathway,fn),file.path(dir.pathway,fn), cacheOK = TRUE, quiet = FALSE,method="wget",extra=c("-nv","-t 3","-nc","-T 5"))})
  filenames <- unlist(strsplit(getURL(src.seq, dirlistonly = TRUE), "\n"))
  sapply(filenames[grep(paste(na.omit(sapply(panther_species,"[",2)),collapse="|"),filenames)],function(fn){download.file(file.path("ftp:/",src.seq,fn),file.path(dir.seq,fn), cacheOK = TRUE, quiet = FALSE,method="wget",extra=c("-nv","-t 3","-nc","-T 5"))})

  download.file(url=src.class,destfile=dir.class)
  download.file(url=src.class_rel,destfile=dir.class_rel)

}
######################################################  


#### PREPARE HMM classifications for GO and Class <> PANTHER ID mappings ####
panther_hmm2df<-function(mdir,version){
  p_hmm<-read.delim(file.path(mdir,sprintf("PANTHER%s_HMM_classifications",version)),stringsAsFactors=F,header=F)
  stopifnot(ncol(p_hmm)==7)
  colnames(p_hmm)<-c("PANTHER_Subfamily_ID","Annotation_curated","MF_GOslim","BP_GOslim","CC_GOslim","ProteinClass","PANTHER_Pathway")
  p_fnames<-strsplit(p_hmm$PANTHER_Subfamily_ID,":",fixed=T)
  p_hmm$PantherID<-sapply(p_fnames,"[",1)
  p_hmm$PantherSF<-sapply(p_fnames,"[",2)
  
  cid<-function(mp,idlen=10){if(nchar(mp)){ms<-unlist(strsplit(mp,"#"));paste(sapply(ms[2:length(ms)],substr,start=1,stop=idlen),collapse="|")}else{NA}}
  
  p_hmm$PANTHER_Pathway<-sapply(p_hmm$PANTHER_Pathway,cid,6)
  p_hmm$MF_GOslim<-sapply(p_hmm$MF_GOslim,cid)
  p_hmm$BP_GOslim<-sapply(p_hmm$BP_GOslim,cid)
  p_hmm$CC_GOslim<-sapply(p_hmm$CC_GOslim,cid)
  p_hmm$ProteinClass<-sapply(p_hmm$ProteinClass,cid,7)
  p_hmm
}

panther_hmm<-panther_hmm2df(dir.hmm,version="8.1")
dim(panther_hmm);length(unique(panther_hmm$PANTHER_Subfamily_ID))
#readLines(file.path(dir.hmm,version,"README"))
#readLines(file.path(dir.hmm,version"LICENSE"))
### ###


#### PREPARE Pathways for PANTHER pathway <> pantherID and Uniprot mappings ####
panther_pathways2df<-function(mdir,version,species_remap){
  p_pway<-read.delim(file.path(mdir,sprintf("/SequenceAssociationPathway%s.txt",version)),stringsAsFactors=F,header=F)
  stopifnot(ncol(p_pway)==12)
  p_pway<-p_pway[,c(1:11)]
  colnames(p_pway)<-c("Pathway_Accession","Pathway_Name","Pathway_Component_Accession","Pathway_Component_Name","UniprotID","Protein_Definition","Confidence_Code","Evidence","Evidence_Type","PANTHER_Subfamily_ID","PANTHER_Subfamily_Name")
  p_upid<-strsplit(p_pway$UniprotID,"|",fixed=T)
  
  p_pway$Species<-sapply(p_upid,"[",1)
  invisible(sapply(unique(p_pway$Species),function(mspec){mhit<-which(species_remap==mspec);if(length(mhit)){nspec<-names(species_remap)[mhit];p_pway$Species[p_pway$Species==mspec]<<-nspec}}))
  p_pway$UniprotID<-substr(sapply(p_upid[1],"[",3),start=11,16)
  p_pway<-p_pway[which(p_pway$Species %in% names(species_remap)),]
  
  p_fnames<-strsplit(p_pway$PANTHER_Subfamily_ID,":",fixed=T)
  p_pway$PantherID<-sapply(p_fnames,"[",1)
  p_pway$PantherSF<-sapply(p_fnames,"[",2)
  
  p_pway
}

panther_pathways<-panther_pathways2df(dir.pathway,version="3.2",species_remap=org_bioc2panther)
dim(panther_pathways)
### ###

#### PREPARE Sequence classifications for Uniprot <> PANTHER ID mappings ####
panther_seq2df<-function(mdir,version,species){
  mlist<-vector("list",length(species))
  names(mlist)<-names(species)
  for(spec in names(species)){
    if(is.na(species[[spec]])){
      message(sprintf("Species %s not found in PANTHER folder",spec))
      next
    }
    mlist[[spec]]<-read.delim(file.path(mdir,sprintf("PTHR%s_%s",version,species[[spec]])),stringsAsFactors=F,header=F)
    stopifnot(ncol(mlist[[spec]])==10)
    mlist[[spec]]<-mlist[[spec]][,c(1,3:10)]
    mlist[[spec]]$Species<-spec
  }
  p_seq<-do.call(rbind,mlist)
  colnames(p_seq)<-c("Gene_Identifier","PantherSF","PANTHER_Family_Name","PANTHER_Subfamily_Name","PANTHER_MF","PANTHER_BP","PANTHER_CC","ProteinClass","PANTHER_Pathway","Species")
  
  p_seq$UniprotID<-substr(p_seq$Gene_Identifier,start=nchar(p_seq$Gene_Identifier)-5,stop=nchar(p_seq$Gene_Identifier))
  
  #p_upid<-strsplit(p_seq$Gene_Identifier,"|",fixed=T)
  #p_seq$Species<-sapply(p_upid,"[",1)
  #p_seq$UniprotID<-substr(sapply(strsplit(p_seq$Gene_Identifier,"|",fixed=T),"[",3),start=11,16)
  
  p_seq$PantherIDSF<-p_seq$PantherSF
  sfam<-strsplit(p_seq$PantherSF,":",fixed=T)
  p_seq$PantherID<-sapply(sfam,"[",1)
  p_seq$PantherSF<-sapply(sfam,"[",2)
  
  cid<-function(mp,idlen=10){if(nchar(mp)){ms<-unlist(strsplit(mp,"#"));paste(sapply(ms[2:length(ms)],substr,start=1,stop=idlen),collapse="|")}else{NA}}
  
  p_seq$PANTHER_Pathway<-sapply(p_seq$PANTHER_Pathway,cid,6)
  p_seq$PANTHER_MF<-sapply(p_seq$PANTHER_MF,cid)
  p_seq$PANTHER_BP<-sapply(p_seq$PANTHER_BP,cid)
  p_seq$PANTHER_CC<-sapply(p_seq$PANTHER_CC,cid)
  p_seq$ProteinClass<-sapply(p_seq$ProteinClass,cid,7)
  
  p_seq

}

panther_seq<-panther_seq2df(dir.seq,version="8.1",species=org_bioc2panther_fn)
dim(panther_seq)
#readLines(file.path(dir.seq,"8.1","README"))
#readLines(file.path(dir.seq,"8.1","LICENSE"))
### ###


#### PREPARE Orthologs, not needed at the moment ####
panther_ortholog2df<-function(mdir,version,species_remap){
  p_ortho<-read.delim(file.path(mdir,"/RefGeneomeOrthologs.txt"),stringsAsFactors=F,header=F)
  stopifnot(ncol(p_ortho)==5)

  colnames(p_ortho)<-c("Species","SpeciesTarget","OrthologeType","Group","PantherID")
  p_spec<-strsplit(p_ortho$Species,"|",fixed=T)
  p_ortho$Species<-sapply(p_spec,"[",1)
  p_ortho$Uniprot<-substr(sapply(p_spec,"[",3),start=11,16)
  p_ortho<-p_ortho[which(p_ortho$Species %in% species_remap),]
  
  p_spec_targ<-strsplit(p_ortho$SpeciesTarget,"|",fixed=T)
  p_ortho$SpeciesTarget<-sapply(p_spec_targ,"[",1)
  p_ortho$UniprotTarget<-substr(sapply(p_spec_targ,"[",3),start=11,16)
  p_ortho<-p_ortho[which(p_ortho$SpeciesTarget %in% species_remap),]  
  #map species to abbreviation used in bioC
  invisible(sapply(unique(p_ortho$Species),function(mspec){mhit<-which(species_remap==mspec);if(length(mhit)){nspec<-names(species_remap)[mhit];p_ortho$Species[p_ortho$Species==mspec]<<-nspec}}))
  invisible(sapply(unique(p_ortho$SpeciesTarget),function(mspec){mhit<-which(species_remap==mspec);if(length(mhit)){nspec<-names(species_remap)[mhit];p_ortho$SpeciesTarget[p_ortho$SpeciesTarget==mspec]<<-nspec}}))
  
  p_ortho
}
#mdir<-dir.ortho;version<-"8.1";species_remap=org_bioc2panther
#panther_ortholog<-panther_ortholog2df(dir.ortho,version="8.1",species_remap=org_bioc2panther)
### ###


#### READ CLASS FILES ####
data_class<-read.delim(dir.class,stringsAsFactors=F,header=F)[,c(1,3,4)]
colnames(data_class)<-c("class_id","class_term","definition")
data_class_rel<-read.delim(dir.class_rel,stringsAsFactors=F,header=F)[,c(1,3)]
#remove empty columns
colnames(data_class_rel)<-c("class_id_offspring","class_id_parent")
data_class_rel<-data_class_rel[nchar(data_class_rel$class_id_offspring)==7,]
### ###


#### FILTER PANTHER HMM for bioC unsupported species ####
#sequence files are only from supported species and contain the PID<>UNIPROT mappings
nrow(panther_hmm);panther_hmm<-panther_hmm[which(panther_hmm$PantherID %in% panther_seq$PantherID),];nrow(panther_hmm)
### ###

#### MAKE UNIQUE PANTHER SEQ UNIPROT IDs and filter duplicates due to multiple UNIPROT<>GENE mappings ####
nrow(panther_seq);panther_seq<-panther_seq[!duplicated(panther_seq$UniprotID),];nrow(panther_seq)
### ###

#### Order source dfs by PANTHER FAMILY ID for simple comparisons ####
panther_pathways<-panther_pathways[order(panther_pathways$PANTHER_Subfamily_ID),]
panther_hmm<-panther_hmm[order(panther_hmm$PANTHER_Subfamily_ID),]
#add pk _id used by all other tables as fk
panther_hmm$`_id`<-1:nrow(panther_hmm)
rownames(panther_hmm)<-panther_hmm$PANTHER_Subfamily_ID
panther_seq<-panther_seq[order(panther_seq$PantherIDSF),]
#panther_ortholog<-panther_ortholog[order(panther_ortholog$PantherID),]
### ###

#### Create the database file ####

drv <- dbDriver("SQLite")

file.remove(file.path(home_folder,"PANTHER.db.sqlite"))
db <- dbConnect(drv, dbname=file.path(home_folder,"PANTHER.db.sqlite"))

## Create tables
create.sql <- strsplit(schema.text, "\n")[[1]]
create.sql <- paste(collapse="\n", create.sql)
create.sql <- strsplit(create.sql, ";")[[1]]
create.sql <- create.sql[-length(create.sql)] # nothing to run here

tmp <- sapply(create.sql, function(x) sqliteQuickSQL(db, x))
dbListTables(db)
#dbListFields(db,"uniprot")
### ###

#### Convenience function for data frame insertion ####
insert_df<-function(mdb,tname,mcols,mdata){
  dbGetQuery(mdb, sprintf('delete from %s;',tname))
  dbBeginTransaction(mdb)
  dbGetPreparedQuery(mdb,sprintf('INSERT INTO "%s" VALUES(%s);',tname,mcols),bind.data=mdata)
  dbCommit(mdb)
  dbListFields(mdb,tname)
  print(dbGetQuery(db,sprintf('select * from "%s" limit 3;',tname)))
  xc<-as.numeric(dbGetQuery(mdb,sprintf('select count(*) from "%s"',tname)));
  stopifnot(xc==nrow(mdata))
  cat(sprintf("INSERTION_SUCCESS n=%d",xc))
}
### ###

#### Prepare PANTHER FAMILIES ####
#table(panther_pathways$PANTHER_Subfamily_ID %in% panther_hmm$PANTHER_Subfamily_ID[grep("^PTHR[0-9]+:SF[0-9]+$",panther_hmm$PANTHER_Subfamily_ID)])
data_panther_families<-panther_hmm[,c("_id","PANTHER_Subfamily_ID")]
data_panther_families<-merge(data_panther_families,panther_seq[,c("PantherIDSF","PANTHER_Family_Name","PANTHER_Subfamily_Name")],by.x="PANTHER_Subfamily_ID",by.y="PantherIDSF",all=T)
data_panther_families<-data_panther_families[!duplicated(data_panther_families),];nrow(data_panther_families)
colnames(data_panther_families)<-c("family_id","_id","family_term","subfamily_term")
rownames(data_panther_families)<-data_panther_families$family_id
### ###

#### Insert PANTHER FAMILIES ####
insert_df(db,"panther_families",":_id,:family_id, :family_term, :subfamily_term",data_panther_families)
### ###

#### Prepare GO SLIM ####
#table(panther_pathways$PANTHER_Subfamily_ID %in% panther_hmm$PANTHER_Subfamily_ID[grep("^PTHR[0-9]+:SF[0-9]+$",panther_hmm$PANTHER_Subfamily_ID)])
data_go_slim<-panther_hmm[,c("_id","MF_GOslim","BP_GOslim","CC_GOslim")]

gos_MF<-strsplit(data_go_slim$MF_GOslim,"|",fixed=T)
names(gos_MF)<-data_go_slim$`_id`
gos_MF<-stack(gos_MF)
nrow(gos_MF);gos_MF<-gos_MF[!is.na(gos_MF$values),];nrow(gos_MF)
gos_MF$ind<-as.character(gos_MF$ind)
gos_MF$ontology<-"MF"

gos_BP<-strsplit(data_go_slim$BP_GOslim,"|",fixed=T)
names(gos_BP)<-data_go_slim$`_id`
gos_BP<-stack(gos_BP)
nrow(gos_BP);gos_BP<-gos_BP[!is.na(gos_BP$values),];nrow(gos_BP)
gos_BP$ind<-as.character(gos_BP$ind)
gos_BP$ontology<-"BP"

gos_CC<-strsplit(data_go_slim$CC_GOslim,"|",fixed=T)
names(gos_CC)<-data_go_slim$`_id`
gos_CC<-stack(gos_CC)
nrow(gos_CC);gos_CC<-gos_CC[!is.na(gos_CC$values),];nrow(gos_CC)
gos_CC$ind<-as.character(gos_CC$ind)
gos_CC$ontology<-"CC"

data_go_slim<-rbind(gos_BP,gos_MF,gos_CC);nrow(data_go_slim)
#nrow(data_go_slim[!duplicated(data_go_slim),])
colnames(data_go_slim)<-c("goslim_id","_id","ontology")
### ###

#### Insert GO SLIM ####
insert_df(db,"go_slim",":_id,:goslim_id, :ontology",data_go_slim)
### ###


#### Prepare uniprot; sequnce files need to be expanded due to missing family only IDs!! ####
data_uniprot<-data.frame(uniprot_id=rep(panther_seq$UniprotID,2),species=rep(panther_seq$Species,2),family_id=c(panther_seq$PantherIDSF,panther_seq$PantherID),stringsAsFactors=F)
data_uniprot$`_id`<-panther_hmm[data_uniprot$family_id,"_id"]
### ###

#### Insert uniprot ####
insert_df(db,"uniprot",":_id,:uniprot_id,:species",data_uniprot)
### ###

#### Prepare protein_class ####
#all(panther_hmm$PANTHER_Subfamily_ID==data_panther_families$panther_subfamily_id)
hmm_cl<-strsplit(panther_hmm$ProteinClass,"|",fixed=T)
names(hmm_cl)<-panther_hmm$`_id`
data_protein_class<-stack(hmm_cl);nrow(data_protein_class)
data_protein_class<-data_protein_class[!is.na(data_protein_class$values),];nrow(data_protein_class)
data_protein_class$ind<-as.character(data_protein_class$ind)
colnames(data_protein_class)<-c("class_id","_id")
data_protein_class$class_term<-NA
invisible(sapply(1:length(data_class$class_id),function(ci){md<-grep(data_class$class_id[ci],data_protein_class$class_id,fixed=T);if(length(md))data_protein_class$class_term[md]<<-data_class$class_term[ci]}))
if(any(is.na(data_protein_class$class_term)))stop("error")
### ###

#### Insert protein_class ####
insert_df(db,"protein_class",":_id,:class_id,:class_term",data_protein_class)
### ###

#### Prepare protein_class_tree ####
data_class$class_tree_id<-1:nrow(data_class)
### ###
#### Insert protein_class_tree ####
insert_df(db,"protein_class_tree",":class_tree_id,:class_id,:class_term,:definition",data_class)
### ###


#### Prepare parent protein_class_tree ####
data_class_rel$parent_class_id<-NA
data_class_rel$class_tree_id<-NA
#convert class ids to tree_ids
invisible(sapply(1:length(data_class$class_id),function(ci){md<-grep(data_class$class_id[ci],data_class_rel$class_id_offspring,fixed=T);if(length(md))data_class_rel$class_tree_id[md]<<-data_class$class_tree_id[ci]}))
invisible(sapply(1:length(data_class$class_id),function(ci){md<-grep(data_class$class_id[ci],data_class_rel$class_id_parent,fixed=T);if(length(md))data_class_rel$parent_class_id[md]<<-data_class$class_tree_id[ci]}))

### ###
#### Insert parent protein_class_tree ####
insert_df(db,"protein_class_parents",":class_tree_id,:parent_class_id",data_class_rel)
### ###


#### Prepare offspring protein_class_tree ####
find_offspring<-function(node,parents,offspring){
  child_vec<-offspring[grep(node,data_class_rel$class_id_parent,fixed=T)]
  temp_vec<-c();
  for(child in child_vec){
    parent_vec<-parents[grep(child,parents,fixed=T)]
    if(!length(parent_vec)){
      next
    }else{
      temp_vec<-c(temp_vec,find_offspring(child,parents,offspring))
    }
  }
  c(child_vec,temp_vec)
}
olist<-lapply(unique(data_class_rel$class_id_parent),function(cid){unique(find_offspring(cid,data_class_rel$class_id_parent,data_class_rel$class_id_offspring))})

names(olist)<-unique(data_class_rel$class_id_parent)
data_class_rel_off<-stack(olist);nrow(data_class_rel_off)
data_class_rel_off$ind<-as.character(data_class_rel_off$ind)
colnames(data_class_rel_off)<-c("class_id_offspring","class_id_parent")
data_class_rel_off$offspring_class_id<-NA
data_class_rel_off$class_tree_id<-NA
invisible(sapply(1:length(data_class$class_id),function(ci){md<-grep(data_class$class_id[ci],data_class_rel_off$class_id_offspring,fixed=T);if(length(md))data_class_rel_off$offspring_class_id[md]<<-data_class$class_tree_id[ci]}))
invisible(sapply(1:length(data_class$class_id),function(ci){md<-grep(data_class$class_id[ci],data_class_rel_off$class_id_parent,fixed=T);if(length(md))data_class_rel_off$class_tree_id[md]<<-data_class$class_tree_id[ci]}))
### ###
#### Insert offspring protein_class_tree ####
insert_df(db,"protein_class_offspring",":class_tree_id,:offspring_class_id",data_class_rel_off)
### ###


#### Prepare panther_go ####
data_pathway<-data.frame(go_id=rep(panther_pathways$Pathway_Accession,2),go_term=rep(panther_pathways$Pathway_Name,2),family_id=c(panther_pathways$PANTHER_Subfamily_ID,panther_pathways$PantherID),stringsAsFactors=F)
data_pathway$`_id`<-panther_hmm[data_pathway$family_id,"_id"]
nrow(data_pathway);data_pathway<-data_pathway[!duplicated(data_pathway),];nrow(data_pathway)
### ###

#### Insert panther_go ####
insert_df(db,"panther_go",":_id,:go_id,:go_term",data_pathway)
### ###

#### Prepare panther_go ####
data_pathway_component<-data.frame(component_go_id=rep(panther_pathways$Pathway_Component_Accession,2),component_term=rep(panther_pathways$Pathway_Component_Name,2),evidence=rep(panther_pathways$Evidence,2),evidence_type=rep(panther_pathways$Evidence_Type,2),confidence_code=rep(panther_pathways$Confidence_Code,2),family_id=c(panther_pathways$PANTHER_Subfamily_ID,panther_pathways$PantherID),stringsAsFactors=F)
data_pathway_component$`_id`<-panther_hmm[data_pathway_component$family_id,"_id"]
nrow(data_pathway_component);data_pathway_component<-data_pathway_component[!duplicated(data_pathway_component),];nrow(data_pathway_component)
### ###

#### Insert panther_go ####
insert_df(db,"panther_go_component",":_id,:component_go_id,:component_term,:evidence,:evidence_type,:confidence_code",data_pathway_component)
### ###

#### metadata ####
metadata <- rbind(
  c("SPECIES", paste(sort(names(panther_species)[!is.na(panther_species)]),collapse="|")),
  c("PANTHERVERSION", "8.1"),
  c("PANTHERSOURCEURL","ftp.pantherdb.org"),
  c("PANTHERSOURCEDATE",format(Sys.time(), "%Y-%b%d")),
  c("package","AnnotationDbi"),
  c("Db type","PANTHER"),
  c("DBSCHEMAVERSION", "2.1")
)
q <- paste(sep="", "INSERT INTO 'metadata' VALUES('", metadata[,1],"','", metadata[,2], "');")
tmp <- sapply(q, function(x) sqliteQuickSQL(db, x))
## map_counts
map.counts <- rbind(
  c("GOCOMPONENT", nrow(data_pathway_component)),
  c("FAMILIES", nrow(data_panther_families)),
  c("GOSLIM", nrow(data_go_slim)),
  c("CLASS", nrow(data_protein_class)),
  c("UNIPROT", nrow(data_uniprot)),
  c("GO", nrow(data_pathway))
)

q <- paste(sep="", "INSERT INTO 'map_counts' VALUES('", map.counts[,1],"',", map.counts[,2], ");")

tmp <- sapply(q, function(x) sqliteQuickSQL(db, x))

### ###

#### DISCONNECT ####
dbDisconnect(db)
### ###

