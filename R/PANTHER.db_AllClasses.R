.PANTHER<-setRefClass(
  "PANTHER", 
  contains="AnnotationDb",
  fields=list(.ref_table="character", .user_filter="logical",.species="character",.allSpecies="character"),
  methods=list(
    setSpecies=function(nspec){
      if(!nspec %in% unlist(strsplit(.allSpecies,"|",fixed=T)))stop(paste0("Species must be one of the following:\n",.allSpecies))
      if(.allSpecies==nspec)return()
      .species<<-nspec
      .user_filter<<-T
      
      tfname<-"sfilt"
      tfile<-paste0(tfname,".sqlite")
      if(file.exists(tfile))file.remove(tfile)
      if( tfname %in% dbGetQuery(.self$conn, "PRAGMA database_list")$name )dbGetQuery(.self$conn, paste("DETACH DATABASE",tfname))
      
      dbt <- dbConnect("SQLite", dbname=tfile);dbDisconnect(dbt)
      dbGetQuery(.self$conn, sprintf("ATTACH '%s' AS %s",tfile,tfname))
      sql <- sprintf("CREATE TABLE %s.panther_families_filtered AS SELECT _id,family_id,family_term,subfamily_term FROM panther_families NATURAL JOIN uniprot WHERE species='%s'",tfname,.species)
      sqliteQuickSQL(.self$conn, sql)
      
      .ref_table<<-paste0(tfname,".panther_families_filtered")
    },
    .resetSpecies=function(){
      .species<<-.allSpecies
      .user_filter<<-F
      .ref_table<<-"panther_families"
    },
    .setAvailableSpecies=function(){
      sql <- paste("SELECT DISTINCT(species) from uniprot")
      res <- dbGetQuery(AnnotationDbi:::dbConn(.self), sql)
      .allSpecies<<-paste(sort(res[,1]),collapse="|")
      .resetSpecies()
      .ref_table<<-"panther_families"
    }
  )  
)
