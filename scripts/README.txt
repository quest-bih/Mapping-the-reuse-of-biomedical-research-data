pseudo-code for "extract_ids_from_datastet_AC.R":

first extract by cond1: 
  cond0 <- str_extract_all(text, "(?i)(figshare|zenodo|osf|mendeley|harvard|dryad)[^\\s,)]*")
(actually these patterns: figshare.
zenodo.
osf.io/
mendeley.com/datasets/
dryad.)
once one of the patterns is extracted (figshare., zenodo. etc.), allow white spaces in the extraction forwards, because there are also cases like: "https:// doi. org/ 10. 5281/ zenodo. 42237 29." (which should be extracted as "zenodo.4223729").
so the pattern to look for in cond1 should be: "the pattern" (e.g. zenodo."), then "some digits and or letters and or white spaces" and then stop when reaching "." or ")" or "]" or "," or "#" or "'" or ":" or ";" or ">"

then extract by cond2, unless the extraction of cond2 is already contained in the extraction of cond1
(for example if you extracted "zenodo.phs005" in cond1, and cond2 detected "phs005", skip this 
extraction for cond1 because it's the same extraction!).

cond2: in cond2, look for the pattern: "prefix", then "some digits or white spaces", and then stop when reaching anything that's not digits or whitespaces.
prefixes <- c(
    "sam", "gse", "gsm", "gds", "gpl", "e-mtab-", "egas", "egad", "e-geod", "mk", "mh", "phs", "mn", "mw",
    "pxd", "srr", "prj(eb|na|db|da|ea|sa|ma)", "emd-", "gcst", "pdb_", "nm_", "nct", "err", "gds", "msv", "mz", "nc_", "np_",
    "sr(p|r|x|s|z)", "phs", "pgs", "s-bsst", "mt", "kt", "st", "ol", "op", "or", "oq", "scp",
    "s-biad", "e-tabm", "empiar", "fr-fcm-z", "gca_", "egac", "up", "ng", "gcf_", "ensg", "syn"
  )

then extract by cond3, unless cond1's or cond2's extractions are included in what would be extracted by cond3.
cond3 aims to find DOIs (again, except if they are already extracted differently using cond1 / cond2): 
  cond3 <- str_extract_all(text, "10\\.[^\\s,)]+")[[1]]
 
cond3: look for the pattern: "10.", then "some digits", then "/", then "some digits and or letters and or white spaces" and or ".", and then stop when reaching or ")" or "]" or "," or "#" or "'" or ":" or ";" or ">"

then extract by cond4, unless cond1's or cond2's or cond3's extractions are included in what would be extracted by cond4.
cond4 aims to extract anything from "//" forward, unless its already been extracted differnely using cond1/2/3:
cond4 <- str_extract_all(text, "//[^\\s,)]+")

So in cond4, look for "//" and then extract "//" and everything after that, until reaching ")" or "]" or "," or "#" or "'" or ":" or ";" or ">"
for example if you have the string "//zenodo.dspfijds", cond1 will extract "dspfijds" and will make cond4 redundant even though it has "//".

lastly, other conditions: any of these patterns, unless their extractions are included in any of the previous conditions' extractions - but allow whitspaces:
  other_patterns <- c(
    "fcon_\\s*1000\\.\\s*projects\\.\\s*nitrc\\.\\s*org",
    "rcsb\\.\\s*org/structure/[^).,#']+",
    "(?<!of )[0-9\\s]{6,10}\\b",
    "dip:[0-9\\s]{3}",
    "fr-fcm-[a-z0-9\\s]{4}",
    "collections?(?:[:/])[0-9\\s]{4}",
    "icpsr\\s*[0-9\\s]{4}",
    "sn\\s*[0-9\\s]{4}",
    "search\\.\\s*kg\\.\\s*ebrains\\.\\s*eu",
    "[a-z]{1}[:digit:]{4}",
    "[a-z]{2}[:digit:]{6}",
    "[a-z]{3}[:digit:]{5}",
    "[a-z]{4,6}[:digit:]{3,}",
    "e\\s*n\\s*c\\s*s\\s*r\\s*0\\s*0\\s*0\\s*[0-9]{3}\\s*[a-z]{3}"
  )

if it's just digits - it's not extracted!

in addition, if there's more than one extraction, it's first separated by ";". for example, if phs505 and up0132 are extracted, then the value will be "phs505;up0132". then it's being separated into 2 different rows with the same DOI