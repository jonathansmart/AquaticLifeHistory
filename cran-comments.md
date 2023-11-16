## Resubmission
This is a resubmission. In this version I have:

* Removed \dontrun from most function examples and reduced their runtime through
fewer bootstraps. 

* The Estimate_Len_Maturity() function has been wrapped with \donttest{} as this 
function cannot be run in < 5 sec with the example data.

* Checked potentially misspelt words and links in the DESCRIPTION file. All are 
correct.

* (possibly) invalid URLs detected by rhub::check_for_cran are valid and can be 
accessed.

  
## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.
