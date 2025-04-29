# gstest 3.0.0

(*G-Series 3.0 in R*)

* Initial release of G-Series in R (package gstest).

* New functionality (`stock_benchmarking()`) designed for benchmarking stocks using a cubic spline interpolation approach.

* Improvements to the already existing functionalities:
  * **Benchmarking** (`benchmarking()`): 
    * new options allowing proportional benchmarking (`lambda != 0`) in presence of negative values in the input data;
    * a flat indicator series of *all zeros* is now allowed with additive benchmarking (`lambda = 0`).
  * **Raking** (`tsrasking()`):
    * optional alternative handling of negative values in the input data (same handling as `tsbalancing()`);
    * helper function (`tsraking_driver()`) to simplify the reconciliation of several periods in a single function call.
  * **Balancing** (`tsbalancing()`): 
    * customizable solving sequence (multiple attempts at solving the problem as opposed to a single attempt);
    * improved solution validation process with more information (output data frames) available to help troubleshooting invalid solutions;
    * optional input data *validation only* process (without attempting to solve the problem, i.e., resolve the discrepancies, if any).

* New utility functions to help using the core functions, e.g.;
  * for producing benchmarking plots;
  * for converting reconciliation problem metadata (`tsrasking()` to `tsbalancing()`);
  * for manipulating time series data (prepare or convert the input and output data objects).


# Version 2.0.0

(*G-Series 2.0 in SAS<sup>®</sup>*, contact [g-series@statcan.gc.ca](mailto:g-series@statcan.gc.ca))

* New ***GSeriesTSBalancing*** macro.


# Version 1.4.0

(*G-Series 1.04 in SAS<sup>®</sup>*, contact [g-series@statcan.gc.ca](mailto:g-series@statcan.gc.ca))

* Name change: *Forillon* becomes *G-Series*.

* Introduction of alterability coefficients for PROC BENCHMARKING (when `rho <  1`).

* New `WITH` statement for PROC BENCHMARKING.

* New options `WARNNEGRESULT|NOWARNNEGRESULT` and `TOLNEGRESULT=` for PROC BENCHMARKING and PROC TSRAKING.


# Version 1.3.0

(*Forillon 1.03 in SAS<sup>®</sup>*, contact [g-series@statcan.gc.ca](mailto:g-series@statcan.gc.ca))

* New `VAR` and `BY` statements for PROC BENCHMARKING.


# Version 1.2.0

(*Forillon 1.02 in SAS<sup>®</sup>*, contact [g-series@statcan.gc.ca](mailto:g-series@statcan.gc.ca))

* Addition of PROC TSRAKING.


# Version 1.1.0

(*Forillon 1.01 in SAS<sup>®</sup>*, contact [g-series@statcan.gc.ca](mailto:g-series@statcan.gc.ca))

* Initial release with PROC BENCHMARKING.
