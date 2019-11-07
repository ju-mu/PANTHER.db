--
-- PANTHER_DB schema
-- ====================
--
CREATE TABLE panther_families (
_id INTEGER PRIMARY KEY,--
family_id VARCHAR(14) NOT NULL ,-- PANTHER ID
family_term VARCHAR(255) NULL , -- description of family
subfamily_term VARCHAR(255) NULL -- description of subfamily
);

CREATE INDEX panther_families_idx ON panther_families(_id);

CREATE TABLE protein_class (
_id INTEGER NOT NULL REFERENCES panther_families (_id),-- REFERENCES panther ID
class_id VARCHAR(7) NOT NULL, -- PANTHER class ID
class_term VARCHAR(50) NOT NULL --
);

CREATE INDEX protein_class_idx ON protein_class(_id);
CREATE INDEX protein_class_idx2 ON protein_class(class_id);

CREATE TABLE protein_class_tree (
class_tree_id INTEGER PRIMARY KEY ,-- REFERENCES class ID
class_id VARCHAR(7) NOT NULL, -- PANTHER class ID
class_term VARCHAR(50) NOT NULL, --
definition TEXT NOT NULL --
);

CREATE INDEX protein_class_tree_idx ON protein_class_tree(class_tree_id);

CREATE TABLE protein_class_parent (
class_tree_id INTEGER NOT NULL REFERENCES protein_class_tree (class_tree_id),-- REFERENCES class tree ID
parent_class_id INTEGER NOT NULL REFERENCES protein_class_tree (class_tree_id) -- PANTHER class ID
);

CREATE INDEX lprotein_class_parent_idx ON protein_class_parent(class_tree_id,parent_class_id);
CREATE INDEX rprotein_class_parent_idx ON protein_class_parent(parent_class_id,class_tree_id);

CREATE TABLE protein_class_offspring (
class_tree_id INTEGER NOT NULL REFERENCES protein_class_tree (class_tree_id),-- REFERENCES tree class ID
offspring_class_id INTEGER NOT NULL REFERENCES protein_class_tree (class_tree_id) -- PANTHER class ID
);

CREATE INDEX lprotein_class_offspring_idx ON protein_class_offspring(class_tree_id,offspring_class_id);
CREATE INDEX rprotein_class_offspring_idx ON protein_class_offspring(offspring_class_id,class_tree_id);

CREATE TABLE protein_class_ancestor (
class_tree_id INTEGER NOT NULL REFERENCES protein_class_tree (class_tree_id),-- REFERENCES class tree ID
ancestor_class_id INTEGER NOT NULL REFERENCES protein_class_tree (class_tree_id) -- PANTHER class ID
);

CREATE INDEX lprotein_class_ancestor_idx ON protein_class_ancestor(class_tree_id,ancestor_class_id);
CREATE INDEX rprotein_class_ancestor_idx ON protein_class_ancestor(ancestor_class_id,class_tree_id);

CREATE TABLE protein_class_child (
class_tree_id INTEGER NOT NULL REFERENCES protein_class_tree (class_tree_id),-- REFERENCES tree class ID
child_class_id INTEGER NOT NULL REFERENCES protein_class_tree (class_tree_id) -- PANTHER class ID
);

CREATE INDEX lprotein_class_child_idx ON protein_class_child(class_tree_id,child_class_id);
CREATE INDEX rprotein_class_child_idx ON protein_class_child(child_class_id,class_tree_id);


CREATE TABLE go_slim (
_id INTEGER NOT NULL REFERENCES panther_families (_id),-- REFERENCES panther ID
goslim_id CHAR(10) NOT NULL ,
ontology VARCHAR(9) NOT NULL
);

CREATE INDEX go_slim_idx ON go_slim(_id);
CREATE INDEX go_slim_idx2 ON go_slim(goslim_id);

CREATE TABLE panther_go (
_id INTEGER NOT NULL REFERENCES panther_families (_id),-- REFERENCES panther ID
go_id VARCHAR(6) NOT NULL ,
go_term VARCHAR(255) NOT NULL
);

CREATE INDEX panther_go_idx ON panther_go(_id);
CREATE INDEX panther_go_idx2 ON panther_go(go_id);

CREATE TABLE uniprot (
_id INTEGER NOT NULL REFERENCES panther_families (_id),-- REFERENCES panther ID
uniprot_id VARCHAR(6) NOT NULL ,
species VARCHAR(11) NOT NULL -- bioconductor species mnemonic
);

CREATE INDEX uniprot_idx ON uniprot(_id);
CREATE INDEX uniprot_idx2 ON uniprot(uniprot_id);
CREATE INDEX uniprot_idx3 ON uniprot(species);

CREATE TABLE entrez (
_id INTEGER NOT NULL REFERENCES panther_families (_id),-- REFERENCES panther ID
entrez_id VARCHAR(6) NOT NULL,
species VARCHAR(11) NOT NULL -- bioconductor species mnemonic
);

CREATE INDEX entrez_idx ON entrez(_id);
CREATE INDEX entrez_idx2 ON entrez(entrez_id);
CREATE INDEX entrez_idx3 ON entrez(species);

CREATE TABLE panther_go_component (
_id INTEGER NOT NULL REFERENCES panther_families (_id),-- REFERENCES panther ID
component_go_id VARCHAR(6) NOT NULL ,
component_term VARCHAR(255) NOT NULL ,
evidence VARCHAR(255) NULL ,
evidence_type VARCHAR(10) NULL ,
confidence_code VARCHAR(3) NOT NULL
);

CREATE INDEX panther_go_component_idx ON panther_go_component(_id);
CREATE INDEX panther_go_component_idx2 ON panther_go_component(component_go_id); 


-- Metadata tables

CREATE TABLE metadata (
  name VARCHAR(80) PRIMARY KEY,
  value VARCHAR(255)
);

CREATE TABLE map_counts (
  map_name VARCHAR(80) PRIMARY KEY,
  count INTEGER NOT NULL
);

CREATE TABLE map_metadata (
  map_name VARCHAR(80) NOT NULL,
  source_name VARCHAR(80) NOT NULL,
  source_url VARCHAR(255) NOT NULL,
  source_date VARCHAR(20) NOT NULL
);

CREATE TABLE species (
  species VARCHAR(11) NOT NULL,-- bioconductor species mnemonic
  mnemonic_panther VARCHAR(5) NOT NULL, -- PANTHER species mnemonic
  genome_src_panther VARCHAR(20) NOT NULL, -- PANTHER genome source
  genome_date_panther VARCHAR(6) NOT NULL, -- PANTHER genome date
  mnemonic_uniprot VARCHAR(5) NOT NULL, -- uniprot species mnemonic
  species_uniprot VARCHAR(150) NOT NULL, -- uniprot full species name
  taxid_uniprot INTEGER NOT NULL -- uniprot tax ID
);


