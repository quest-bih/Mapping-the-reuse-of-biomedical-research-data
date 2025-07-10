for "extract_ids_from_datastet_AC.R":

first extract by cond0: 
  cond0 <- str_extract_all(text, "(?i)(figshare|zenodo|osf|mendeley|harvard|dryad)[^\\s,)]*")
  
then extract by cond1, unless the extraction of cond1 is already contained in the extraction of cond0
(for example if you extracted "zenodo.phs005" in cond0, and cond1 detected "phs005", skip this 
extraction for cond1 because it's the same extraction!).
cond1: detect prefix+any number of digits anywhere in the string. including "." (dots) between the digits, if there are any. stop when there are no more digits or if there's a dot and after the dot there is no digit.
prefixes:
prefixes <- c(
    "sam", "gse", "gsm", "gds", "gpl", "e-mtab-", "egas", "egad", "e-geod", "mk", "mh", "phs", "mn", "mw",
    "pxd", "srr", "prj(eb|na|db|da|ea|sa|ma)", "emd-", "gcst", "pdb_", "nm_", "nct", "err", "gds", "msv", "mz", "nc_", "np_",
    "sr(p|r|x|s|z)", "phs", "pgs", "s-bsst", "mt", "kt", "st", "ol", "op", "or", "oq", "scp",
    "s-biad", "e-tabm", "empiar", "fr-fcm-z", "gca_", "egac", "up", "ng", "gcf_", "ensg", "syn"
  )

then extract by cond2, unless cond0's or cond1's extractions are included in what would be extracted by cond2.
cond2 aims to find DOIs (again, except if they are already extracted differently using cond0 / cond1): 
  cond2 <- str_extract_all(text, "10\\.[^\\s,)]+")[[1]]
  
then extract by cond3, unless cond0's or cond1's or cond2's extractions are included in what would be extracted by cond3.
cond3 aims to extract anything from "//" forward, unless its already been extracted differnely using cond0/1/2:
cond3 <- str_extract_all(text, "//[^\\s,)]+")

for example if you have the string "//zenodo.dspfijds", cond0 will extract "dspfijds" and will make cond3 redundant even though it has "//".

lastly, other conditions: any of these patterns, unless their extractions are included in any of the previous conditions' extractions:
  other_patterns <- c(
    "fcon_ ?1000\\.projects\\.nitrc\\.org",
    "rcsb\\.?\\s*org/structure/[^)]+",
    "(?<!of )[0-9]{6,10}\\b",
    "dip:[0-9]{3}",
    "fr-fcm-[a-z0-9]{4}",
    "collections?(?:[:/])[0-9]{4}",
    "icpsr ?[0-9]{4}",
    "sn ?[0-9]{4}",
    "search\\.kg\\.ebrains\\.eu",
	"[a-z]{1}[:digit:]{4}"
	"[a-z]{2}[:digit:]{6}"
	"[a-z]{3}[:digit:]{5}"
	"[a-z]{4,6}[:digit:]{3,}"
	)

if it's just digits - it's not extracted!

in addition, if there's more than one extraction, it's first separated by ";". for example, if phs505 and up0132 are extracted, then the value will be "phs505;up0132". then it's separated into 2 different rows with the same DOI