

#### Prepare reference queries from PANTHER webservice 18.09.2013 ####
#results will be used for Unit Tests
require(PANTHER.db)

if(F){
  library(RCurl)
  require(RUnit)

  url <- 'http://www.pantherdb.org/webservices/garuda/search.jsp?keyword='
  ids <- PANTHER.db::keys(PANTHER.db,"UNIPROT")[c(50,111,444)];ids

  fullUrl <- paste0(url,paste(ids[[3]], collapse="|"),'&listType=gene&type=getList')#only one allowed
  gs <- read.delim(fullUrl,stringsAsFactors=F)$Gene.Symbol

  fullUrl <- paste0(url,paste(ids, collapse="+or+"),'&listType=pathway&type=getList')
  path_acc <- read.delim(fullUrl,stringsAsFactors=F)$Pathway.Accession

  fullUrl <- paste0(url,paste(ids, collapse="+or+"),'&listType=family&type=getList')
  fam_acc <- read.delim(fullUrl,stringsAsFactors=F)$Family.Accession

  fullUrl <- paste0(url,paste(ids, collapse="+or+"),'&listType=category&type=getList')
  cat_acc <- read.delim(fullUrl,stringsAsFactors=F)$Category.Accession
}
### ###


test_structure <- function(){
  RUnit::checkEquals(sort(PANTHER.db::keytypes(PANTHER.db)),sort(PANTHER.db:::.keytypes()))
  RUnit::checkEquals(sort(PANTHER.db::keys(PANTHER.db)),sort(PANTHER.db:::.keys(PANTHER.db,"FAMILY_ID")))
  RUnit::checkEquals(sort(PANTHER.db::columns(PANTHER.db)),sort(PANTHER.db:::.cols()))
}

online <- FALSE

test_select_vs_webquery <- function(){

  ids <- "PTHR10763:SF23"#webserver returns go cats and class ids for all subfamilies
  webquery <- sprintf("http://www.pantherdb.org/webservices/garuda/search.jsp?keyword=%s&listType=category&type=getList",ids)

  if(online){
    res <- read.delim(webquery) #webquery result on 12.10.2019 with v14.1:
    dput(sort(as.character(res$Category.Accession)))
  }
  response.acc <- c("GO:0000075", "GO:0000076", "GO:0000086", "GO:0000228", "GO:0000278",
                    "GO:0000280", "GO:0000781", "GO:0000784", "GO:0000808", "GO:0003676",
                    "GO:0003677", "GO:0003688", "GO:0003690", "GO:0005488", "GO:0005622",
                    "GO:0005623", "GO:0005634", "GO:0005664", "GO:0005694", "GO:0006139",
                    "GO:0006259", "GO:0006260", "GO:0006261", "GO:0006270", "GO:0006725",
                    "GO:0006807", "GO:0006996", "GO:0007049", "GO:0007093", "GO:0007154",
                    "GO:0007165", "GO:0007346", "GO:0008152", "GO:0009058", "GO:0009059",
                    "GO:0009987", "GO:0010389", "GO:0010564", "GO:0010948", "GO:0016043",
                    "GO:0018130", "GO:0019438", "GO:0022402", "GO:0023052", "GO:0031570",
                    "GO:0031974", "GO:0031981", "GO:0032991", "GO:0034641", "GO:0034645",
                    "GO:0034654", "GO:0043170", "GO:0043226", "GO:0043227", "GO:0043228",
                    "GO:0043229", "GO:0043231", "GO:0043232", "GO:0043233", "GO:0043565",
                    "GO:0044237", "GO:0044238", "GO:0044249", "GO:0044260", "GO:0044271",
                    "GO:0044422", "GO:0044424", "GO:0044427", "GO:0044428", "GO:0044446",
                    "GO:0044454", "GO:0044464", "GO:0044770", "GO:0044772", "GO:0044774",
                    "GO:0044839", "GO:0045786", "GO:0045930", "GO:0046483", "GO:0048285",
                    "GO:0048519", "GO:0048523", "GO:0050789", "GO:0050794", "GO:0050896",
                    "GO:0051716", "GO:0051726", "GO:0065007", "GO:0070013", "GO:0071704",
                    "GO:0071840", "GO:0071897", "GO:0090304", "GO:0097159", "GO:0098687",
                    "GO:0140014", "GO:1901360", "GO:1901362", "GO:1901363", "GO:1901576",
                    "GO:1901987", "GO:1901988", "GO:1901990", "GO:1901991", "GO:1902749",
                    "GO:1903047", "GO:1990837", "PC00009", "PC00171", "PC00199")

  select_res <- sort( c( PANTHER.db::select(PANTHER.db,keys=ids,columns="GOSLIM_ID")$GOSLIM_ID, PANTHER.db::select(PANTHER.db,keys=ids,columns="CLASS_ID")$CLASS_ID ) )
  RUnit::checkEquals(response.acc,select_res)

  ids <- "PC00015"
  if(online){
    webquery <- sprintf("http://www.pantherdb.org/webservices/garuda/search.jsp?keyword=%s&listType=family&type=getList",ids)
    res <- read.delim(webquery) #webquery result on 12.10.2019 with v14.1:
    dput(sort(as.character(res$Family.Accession)))
  }
  webquery_res <- c("PTHR11352", "PTHR11352:SF0", "PTHR11352:SF10", "PTHR11352:SF3",
                    "PTHR11352:SF5", "PTHR11352:SF6", "PTHR11352:SF8", "PTHR11352:SF9")
  select_res <- sort(PANTHER.db::select(PANTHER.db,keys=ids,columns="FAMILY_ID",keytype="CLASS_ID")$FAMILY_ID)
  RUnit::checkEquals(webquery_res,select_res)

  ids <- "A0JM86"
  if(online){
    webquery <- sprintf("http://www.pantherdb.org/webservices/garuda/search.jsp?keyword=%s&listType=pathway&type=getList",paste(ids,collapse="+or+"))
    res <- read.delim(webquery) #webquery result on 12.10.2019 with v14.1:
    dput(sort(as.character(res$Pathway.Accession)))
  }
  webquery_res <- "P06664"
  # this also returns NA, as there is no Pathway association to Family terms, just to subfamilies!
  # BUT:
  # From PANTHER authors on 15.10.2019:
  # The sequence association file provides direct association of data to the pathway, while keyword search uses rather loose criteria to return results. In your example, the family is an indirect (or inferred) association to the pathway, and thus not included in the sequence association file. The inferred data are allowed in web search or browsing, but not included in any statistical tools. Therefore, they are not included in the sequence association files either.
  #
  # Hope this helps.
  #
  # Thanks,
  #
  # PANTHER feedback

  select_res <- AnnotationDbi::mapIds(PANTHER.db,keys=ids,column="PATHWAY_ID",keytype="UNIPROT")[[1]]
  RUnit::checkEquals(webquery_res, select_res)

  ids <- "P06664"
  if(online){
    webquery <- sprintf("http://www.pantherdb.org/webservices/garuda/search.jsp?keyword=%s&listType=family&type=getList",paste(ids,collapse="+or+"))
    res <- read.delim(webquery) #webquery result on 12.10.2019 with v14.1:
    dput(sort(as.character(res$Family.Accession)))
  }

  webquery_res <- c("PTHR10005:SF3", "PTHR10024:SF114", "PTHR10071:SF149", "PTHR10071:SF154",
    "PTHR10082:SF28", "PTHR10155:SF3", "PTHR10159:SF309", "PTHR10201:SF24",
    "PTHR10218:SF227", "PTHR10218:SF230", "PTHR10218:SF302", "PTHR10218:SF316",
    "PTHR10218:SF318", "PTHR10218:SF320", "PTHR10218:SF73", "PTHR10252:SF8",
    "PTHR10336:SF12", "PTHR10343:SF83", "PTHR10343:SF84", "PTHR10373:SF25",
    "PTHR10373:SF33", "PTHR10378:SF7", "PTHR10502:SF26", "PTHR10522:SF0",
    "PTHR10614:SF11", "PTHR10614:SF2", "PTHR10614:SF7", "PTHR10614:SF8",
    "PTHR10684:SF3", "PTHR10701:SF10", "PTHR10728:SF13", "PTHR10728:SF39",
    "PTHR10827:SF51", "PTHR10844:SF18", "PTHR10913:SF6", "PTHR11042:SF91",
    "PTHR11064:SF9", "PTHR11213:SF1", "PTHR11255:SF43", "PTHR11269:SF8",
    "PTHR11417:SF5", "PTHR11454:SF9", "PTHR11462:SF37", "PTHR11462:SF7",
    "PTHR11462:SF8", "PTHR11509", "PTHR11509:SF0", "PTHR11515:SF11",
    "PTHR11515:SF17", "PTHR11544:SF14", "PTHR11566:SF32", "PTHR11584:SF332",
    "PTHR11588:SF133", "PTHR11588:SF251", "PTHR11588:SF64", "PTHR11635:SF126",
    "PTHR11636:SF47", "PTHR11723:SF16", "PTHR11723:SF5", "PTHR11771:SF33",
    "PTHR11801:SF2", "PTHR11848:SF117", "PTHR11848:SF125", "PTHR11848:SF133",
    "PTHR11848:SF135", "PTHR11848:SF137", "PTHR11848:SF141", "PTHR11848:SF143",
    "PTHR11848:SF165", "PTHR11848:SF22", "PTHR11848:SF29", "PTHR11849:SF178",
    "PTHR11850:SF59", "PTHR11850:SF80", "PTHR11850:SF89", "PTHR11866:SF10",
    "PTHR11866:SF3", "PTHR11866:SF4", "PTHR11866:SF6", "PTHR11866:SF7",
    "PTHR11866:SF8", "PTHR11920:SF479", "PTHR11945:SF221", "PTHR12167:SF2",
    "PTHR12533:SF11", "PTHR12533:SF4", "PTHR12533:SF5", "PTHR12533:SF6",
    "PTHR12623:SF6", "PTHR12623:SF9", "PTHR12632:SF6", "PTHR13227",
    "PTHR13227:SF0", "PTHR13589:SF14", "PTHR13703:SF23", "PTHR13703:SF36",
    "PTHR13703:SF41", "PTHR13703:SF42", "PTHR13703:SF45", "PTHR13703:SF53",
    "PTHR13715:SF51", "PTHR13715:SF52", "PTHR13715:SF53", "PTHR13780:SF122",
    "PTHR13780:SF38", "PTHR13808:SF29", "PTHR13808:SF34", "PTHR13809:SF25",
    "PTHR13935:SF66", "PTHR14002:SF7", "PTHR14403", "PTHR14403:SF6",
    "PTHR15009", "PTHR15009:SF4", "PTHR15119", "PTHR15119:SF0", "PTHR15359:SF7",
    "PTHR15427:SF20", "PTHR15729:SF13", "PTHR18945:SF52", "PTHR19304:SF9",
    "PTHR19375:SF223", "PTHR19384:SF63", "PTHR19850:SF27", "PTHR19850:SF28",
    "PTHR19850:SF29", "PTHR19850:SF31", "PTHR19850:SF36", "PTHR20855:SF33",
    "PTHR20855:SF40", "PTHR22888:SF9", "PTHR23036:SF86", "PTHR23113:SF150",
    "PTHR23113:SF168", "PTHR23220:SF22", "PTHR23221:SF7", "PTHR23235:SF16",
    "PTHR23235:SF42", "PTHR23255:SF22", "PTHR23255:SF49", "PTHR23255:SF50",
    "PTHR23255:SF62", "PTHR23255:SF63", "PTHR23255:SF64", "PTHR23255:SF70",
    "PTHR23257:SF716", "PTHR23351:SF23", "PTHR23351:SF3", "PTHR23351:SF4",
    "PTHR23503:SF51", "PTHR24055:SF107", "PTHR24055:SF109", "PTHR24055:SF110",
    "PTHR24055:SF146", "PTHR24055:SF172", "PTHR24055:SF203", "PTHR24055:SF222",
    "PTHR24055:SF393", "PTHR24057:SF8", "PTHR24070:SF385", "PTHR24070:SF393",
    "PTHR24072:SF105", "PTHR24072:SF136", "PTHR24082:SF14", "PTHR24082:SF197",
    "PTHR24084:SF27", "PTHR24084:SF4", "PTHR24084:SF6", "PTHR24084:SF7",
    "PTHR24085:SF1", "PTHR24086:SF24", "PTHR24139:SF34", "PTHR24169:SF1",
    "PTHR24204:SF4", "PTHR24208:SF80", "PTHR24216:SF11", "PTHR24241:SF22",
    "PTHR24248:SF87", "PTHR24343:SF306", "PTHR24347:SF403", "PTHR24356:SF158",
    "PTHR24356:SF159", "PTHR24356:SF170", "PTHR24356:SF171", "PTHR24356:SF181",
    "PTHR24356:SF193", "PTHR24356:SF322", "PTHR24361:SF191", "PTHR24361:SF205",
    "PTHR24361:SF225", "PTHR24361:SF314", "PTHR24361:SF324", "PTHR24361:SF342",
    "PTHR24361:SF370", "PTHR24361:SF376", "PTHR24361:SF387", "PTHR24361:SF392",
    "PTHR24361:SF40", "PTHR24361:SF414", "PTHR24361:SF608", "PTHR24361:SF651",
    "PTHR24361:SF656", "PTHR24361:SF88", "PTHR24361:SF94", "PTHR24391:SF17",
    "PTHR24416:SF106", "PTHR24416:SF535", "PTHR24416:SF91", "PTHR24418:SF53",
    "PTHR24418:SF78", "PTHR24418:SF94", "PTHR44329:SF14", "PTHR44329:SF17",
    "PTHR44329:SF22", "PTHR44329:SF35", "PTHR44329:SF39", "PTHR44329:SF46",
    "PTHR45620:SF12", "PTHR45627:SF17", "PTHR45628:SF10", "PTHR45628:SF11",
    "PTHR45628:SF2", "PTHR45628:SF9", "PTHR45655:SF17", "PTHR45655:SF2",
    "PTHR45673:SF5", "PTHR45750:SF2", "PTHR45793:SF9", "PTHR45879:SF1",
    "PTHR45882:SF1", "PTHR45882:SF4", "PTHR45976:SF4", "PTHR46037:SF4",
    "PTHR46180:SF1", "PTHR46385:SF3", "PTHR46513:SF5", "PTHR46716",
    "PTHR46716:SF1", "PTHR46845:SF1")

  # remove non sub family, text search based results, as per above....
  webquery_res <- sort(webquery_res[grepl(":",webquery_res)])
  select_res <- sort(PANTHER.db::select(PANTHER.db,keys=ids,columns="FAMILY_ID",keytype="PATHWAY_ID")$FAMILY_ID)
  RUnit::checkEquals(webquery_res,select_res)
  #PANTHER.db::select(PANTHER.db,keys=ids,columns=c( "FAMILY_ID","PATHWAY_ID","SPECIES"),keytype="UNIPROT")

}


test_switch_species <- function(){
  #species in PANTHER v14.1
  all_specs <- "AMBTC|ANOCA|ANOPHELES|AQUAE|ARABIDOPSIS|ASHGO|ASPFU|BACCR|BACSU|BACTN|BATDJ|BOVINE|BRADI|BRADU|BRAFL|BRARP|CAEBR|CANAL|CANINE|CHICKEN|CHIMP|CHLAA|CHLRE|CHLTR|CIOIN|CITSI|CLOBH|COELICOLOR|COXBU|CRYNJ|CUCSA|DAPPU|DEIRA|DICDI|DICPU|DICTD|ECOLI|EMENI|ENTHI|ERYGU|FELCA|FLY|FUSNN|GEOSL|GIAIC|GLOVI|GORGO|GOSHI|HAEIN|HALSA|HELAN|HELPY|HELRO|HORSE|HORVV|HUMAN|IXOSC|KORCO|LEIMA|LEPIN|LEPOC|LISMO|MAIZE|MALARIA|MEDTR|METAC|METJA|MONBE|MONDO|MOUSE|MUSAM|MYCGE|MYCTU|NEIMB|NEMVE|NEUCR|NITMS|ORNAN|ORYLA|ORYSJ|OSTTA|PARTE|PHANO|PHODC|PHYPA|PHYRM|PIG|POPTR|PRIPA|PRUPE|PSEAE|PUCGT|PYRAE|RAT|RHESUS|RHOBA|RICCO|SALTY|SCHPO|SCLS1|SETIT|SHEON|SOLLC|SORBI|SOYBN|STAA8|STRPU|STRR6|SULSO|SYNY3|THAPS|THECC|THEKO|THEMA|THEYD|TOBAC|TRIAD|TRICA|TRIVA|TRYB2|USTMA|VIBCH|VITVI|WHEAT|WORM|XANCP|XENOPUS|YARLI|YEAST|YERPE|ZEBRAFISH|ZOSMR"
  RUnit::checkEquals(all_specs,PANTHER.db::pthOrganisms(PANTHER.db))
  PANTHER.db::pthOrganisms(PANTHER.db) <- "HUMAN"
  RUnit::checkEquals("HUMAN",pthOrganisms(PANTHER.db))
  PANTHER.db::resetPthOrganisms(PANTHER.db)
  RUnit::checkEquals(all_specs,pthOrganisms(PANTHER.db))
}

