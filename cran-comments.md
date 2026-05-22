Update familiar to version 2.0.1

Version 2.0.1 primarily fixes a few bugs present in version 2.0.0.

Vignettes are pre-compiled to avoid long compilation times on build 
(several minutes).

Longer tests and tests involving parallel processing are not performed on CRAN,
but are performed locally as part of the release process. Running the full test 
suite takes several hours. Locally run unit and integrated tests did not produce
errors or (unexpected) warnings.


# R CMD check results

R CMD check was run on GitHub using 
https://github.com/alexzwanenburg/familiar/actions/workflows/auto-test-package_pull.yml

----------------------------------
windows-latest:
0 errors | 0 warnings | 0 notes

----------------------------------
macos-latest
0 errors | 0 warnings | 0 notes

----------------------------------
ubuntu-latest:
0 errors | 0 warnings | 0 notes



# R CMD check noSuggests

R CMD check (noSuggests) was run on GitHub using 
https://github.com/alexzwanenburg/familiar/actions/workflows/auto-test-no-suggests-pull.yml
(ubuntu-latest) and locally (windows-latest).



# Downstream dependencies

There are currently no downstream dependencies for this package.
