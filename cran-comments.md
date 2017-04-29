## Summary of changes

* New financial market object support and associated interpolation methods
* No backward incompatible changes

## Test environments

* travis-ci based ubuntu 12.04 running R 3.3.3, 3.4.0 and r72638 
* travis-ci based macOS 10.12.1 running R 3.4.0
* appveyor based Windows 3.3.3 patched (r72556)

## R CMD check results

0 errors | 1 warning | 0 notes

I get the following warning when running `devtools::release()`:

> * checking top-level files ... WARNING
> Conversion of ‘README.md’ failed:
> pandoc: Failed to retrieve https://travis-ci.org/imanuelcostigan/fmbasics.svg?branch=master
> TlsException (HandshakeFailed (Error_Misc "user error (unexpected type received. expecting 
> handshake and got: Alert [(AlertLevel_Fatal,InternalError)])"))


I think this error is spurious. I can successfully access the URL in my browser and it renders correctly in the README on Github (https://github.com/imanuelcostigan/fmbasics/blob/master/README.md) and on CRAN (https://cran.rstudio.com/web/packages/fmbasics/README.html).

## Reverse dependencies

No reverse dependency issues
