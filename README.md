# Competence Mismatch

## Data

This repository uses restricted data, and so the data must be manually loaded on a secure machine.

The following files are required:

* `data/els02_r/els02_by_f3_pets.rds` (ELS02 BY-F3 & Pets File)

These files should be created using the following process on a secure machine:

* Extract data from NCES-provided restricted data tool to Stata `.dta` files.
* Use the `readstata13` R library to read these files into R.
* Use the `saveRDS` command in Tidyverse to save a serialized data file to the corresponding file names above.

_Note:_ This repository uses `.gitignore` files to keep restricted data out of version control. Take care to keep all restricted data files off of unsecured, unapproved machines.