# pridit 1.0-2

## Changes for CRAN resubmission

* Fixed additional CRAN issues:
  - Corrected invalid DOI reference in ridit function documentation
  - Changed license specification to computable format (Apache-2.0)
  - Removed LICENSE file as requested by CRAN maintainers

# pridit 1.0-1

## Changes for CRAN resubmission

* Fixed DESCRIPTION file issues identified by CRAN automated checks:
  - Added single quotes around technical terms to address spelling NOTEs
  - Removed obsolete 'RoxygenType' field 
  - Changed license specification to reference LICENSE file
* All NOTEs from previous submission (1.0.0) have been addressed

# pridit 1.0.0

## Initial CRAN release

* Implements ridit scoring as developed by Bross (1958) and modified by Brockett et al. (2002)
* Calculates PRIDIT weights using Principal Component Analysis
* Computes final PRIDIT scores for multivariate analysis of ordinal data
* Comprehensive test suite with 100% function coverage
* Full documentation with working examples for all functions
* Includes sample dataset for testing and demonstration
