meta <- data.frame(
  Title = "PANTHER.db database",
  Description = paste0("Restructured data from PANTHER.db v14.1 ",
                       "downloaded from the PANTHER.db website",
                       "with added entrez gene mappings"),
  BiocVersion = "3.10",
  Genome = NA,
  SourceType = "TSV",
  SourceUrl = "http://www.pantherdb.org,ftp.pantherdb.org,http://data.pantherdb.org,http://www.uniprot.org",
  SourceVersion = "14.1",
  Species = NA,
  TaxonomyId = NA,
  Coordinate_1_based = NA,
  DataProvider = "http://www.pantherdb.org",
  Maintainer = "Julius Muller <mail@jmuller.eu>",
  RDataClass = "SQLite",
  DispatchClass = "FilePath",
  RDataPath = "PANTHER.db/1.0.5/PANTHER.db.sqlite",
  ResourceName = "PANTHER.db.sqlite",
  Tags = "Annotation"
)

## Not run:
## Write the data out and put in the inst/extdata directory.
write.csv(meta, file=file.path("P:\\workspace\\PANTHER.db\\inst\\extdata\\metadata.csv"), row.names=FALSE)

makeAnnotationHubMetadata("P:\\workspace\\PANTHER.db")
