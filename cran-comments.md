## Summary of changes

* New financial market object support and associated interpolation methods
* No backward incompatible changes

## Test environments

* travis-ci based ubuntu 12.04 running R 3.3.3, 3.4.0 and r72638 
* travis-ci based macOS 10.12.1 running R 3.4.0
* appveyor based Windows 3.3.3 patched (r72556)

## R CMD check results

0 errors | 0 warning | 1 note

I get a NOTE about misspelling ISDA and indices. The former is an abbreviation that is expanded in the DESCRIPTION file while the latter is not a misspelt word.

## Reverse dependencies

No reverse dependency issues
