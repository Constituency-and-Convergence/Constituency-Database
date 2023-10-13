# Constituency and Convergence Database

Welcome to the Constituency and Convergence database (CCDB).
This typological database contains planar structures and results of constituency tests applied across multiple languages with a common methodology.
The data is provided by language experts who have conducted extensive fieldwork on the respective languages. 

The methodology and creation of the database is explained in:
* Adam J.R. Tallman. 2021: Constituency and Convergence in Chácobo (Pano). *Studies in Language* 45(2). 321--383. https://doi.org/10.1075/sl.19025.tal
* Adam J.R. Tallman. in press: Introduction. In: Adam J.R. Tallman, Sandra Auderset, Hiroto Uchihara (eds.). *Constituency and Convergence in the Americas*. Language Science Press. https://langsci-press.org/catalog/book/291
* Sandra Auderset et al. in press: Discussion. In: Adam J.R. Tallman, Sandra Auderset, Hiroto Uchihara (eds.). *Constituency and Convergence in the Americas*. Language Science Press. https://langsci-press.org/catalog/book/291

The database consists of the following files:
* planar = planar structures for each language and type
     - Language_ID and Language_Name
     - Planar_ID: a unique identifier for each planar structure
     - Planar_Type: base of the planar structure (verbal, nominal)
     - Position_ID: a unique identifier for each position in each planar structure
     - Position: sequential number of position
     - Position_Type: whether the position is a slot or a zone
     - Elements: a list (not exhaustive) of forms or types of forms that can fill a position
* domains = constituency test results
     - Language_ID and Language_Name
     - Domain_ID: a unique identifier for each constituency test result
     - Domain_Type: level a test targets (morphosyntactic, phonological, indeterminate)
     - Abstract_Type: abstract type of test
     - CrossL_Fracture: cross-linguistically applicaple fracture of a test
     - MinMax_Fracture: fracture into a minimal and maximal domain
     - Lspecific_Fracture: language-specific fracture
     - Left_Edge: left boundary of the test span
     - Right_Edge: right boundary of the test span
     - Convergence: number of other tests (in the same language) a span converges with
     - Relative_Convergence: Convergence/Tests_Total
     - Size: number of positions the span covers
     - Relative_Size: Size/Largest
     - Largest: largest span observed in a language
     - Position_Total: total number of positions in the respective planar structure
     - Tests_Total: total number of tests applied in language/planar structure
     - Planar_ID: which planar structur the test results are applied to
     - Test_Labels: short labels for plotting
* input/metadata = 
     - Language_Name, Alternative_Name, Short_Name (for plotting)
     - Language_ID (this is the Glottocode, if there is one, and a four-letter+four-digits code otherwise)
     - Latitude and Longitude
     - Contribution: publication where a description of the data can be found
     - Contributors
* input/overlaps = codes the position of the verb/noun base for each planar structure
     - Language_ID
     - Overlap_Verbal: position of the verb base in the planar structure
     - Overlap_Verbal_Extended: ?
     - Overlap_Nominal: position of the noun base in the planar structure
* sources = bib-file with the sources

The **input** folder contains the raw files in which the data is initially entered and updated. The **scripts** folder contains R scripts that produce the output version of the database.

The CCDB is available under the Creative Commons Attribution Share Alike 4.0 International license. It is a work in progress and continuously updated. For research and citation, please refer to the most recent release archived in Zenodo.


## CCDB 1.0 - October 2023
The data for version 1.0 are described and illustrated in:
* Adam J.R. Tallman, Sandra Auderset, Hiroto Uchihara (eds.). *Constituency and Convergence in the Americas*. Language Science Press. https://langsci-press.org/catalog/book/291



[![CC BY-SA 4.0][cc-by-sa-image]][cc-by-sa]

[cc-by-sa]: http://creativecommons.org/licenses/by-sa/4.0/
[cc-by-sa-image]: https://licensebuttons.net/l/by-sa/4.0/88x31.png
[cc-by-sa-shield]: https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg
