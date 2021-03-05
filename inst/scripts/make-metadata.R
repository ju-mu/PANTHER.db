meta <- data.frame(
  Title = "PANTHER.db database",
  Description = paste0("Restructured data from PANTHER.db v16.0 ",
                       "downloaded from the PANTHER.db website",
                       "with added entrez gene mappings"),
  BiocVersion = "3.13",
  Genome = NA,
  SourceType = "TSV",
  SourceUrl = "http://www.pantherdb.org,ftp.pantherdb.org,http://data.pantherdb.org,http://www.uniprot.org",
  SourceVersion = "16.0",
  Species = NA,
  TaxonomyId = NA,
  Coordinate_1_based = TRUE,
  DataProvider = "http://www.pantherdb.org",
  Maintainer = "Julius Muller <mail@jmuller.eu>",
  RDataClass = "SQLite",
  DispatchClass = "FilePath",
  RDataPath = "PANTHER.db/1.0.11/PANTHER.db.sqlite",
  ResourceName = "PANTHER.db.sqlite",
  Tags = "Annotation"
)

## Not run:
## Write the data out and put in the inst/extdata directory.
write.csv(meta, file=file.path("PANTHER.db/inst/extdata/metadata.csv"), row.names=FALSE)

makeAnnotationHubMetadata("PANTHER.db/")

