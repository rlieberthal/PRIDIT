# pridit

<!-- [![CRAN status](https://www.r-pkg.org/badges/version/pridit)](https://CRAN.R-project.org/package=pridit) -->
[![R-CMD-check](https://github.com/rlieberthal/PRIDIT/workflows/R-CMD-check/badge.svg)](https://github.com/rlieberthal/PRIDIT/actions)

An R package that implements the PRIDIT (Principal Component Analysis applied to RIDITs) analysis system as described in Brockett et al. (2002).

## Installation

### From CRAN (recommended):
```r
install.packages("pridit")
```

### From GitHub (development version):
```r
# Install devtools if you haven't already
install.packages("devtools")

# Install the PRIDIT package
devtools::install_github("rlieberthal/PRIDIT")
```

## Description

This package provides three main functions for calculating and analyzing Ridit scores and PRIDIT scores:

- **`ridit()`** - Calculates Ridit scores for a given dataset using the method developed by Bross (1958) and modified by Brockett et al. (2002)
- **`PRIDITweight()`** - Applies Principal Component Analysis (PCA) to Ridit scores to calculate PRIDIT weights for each variable
- **`PRIDITscore()`** - Calculates final PRIDIT scores using the weights and ridit scores

## Quick Start

```r
library(pridit)

# Load your data (first column should be IDs)
data <- data.frame(
  ID = c("A", "B", "C", "D", "E"),
  var1 = c(0.9, 0.85, 0.89, 1.0, 0.89),
  var2 = c(0.99, 0.92, 0.90, 1.0, 0.93),
  var3 = c(1.0, 0.99, 0.98, 1.0, 0.99)
)

# Step 1: Calculate ridit scores
ridit_scores <- ridit(data)

# Step 2: Calculate PRIDIT weights
weights <- PRIDITweight(ridit_scores)

# Step 3: Calculate final PRIDIT scores
final_scores <- PRIDITscore(ridit_scores, data$ID, weights)

print(final_scores)
```

## Data Format

Your input data should be structured as:
- **First column**: Unique identifiers (IDs)
- **Remaining columns**: Numerical variables to be analyzed
- All variables should be numeric (convert factors/categories to numeric values like 0,1 or 1,2,3,4,5)

## Output

The final PRIDIT scores range from -1 to 1, where:
- The **sign** indicates class identity
- The **magnitude** indicates the intensity of that identity

## References

- Bross, I. D. (1958). How to use ridit analysis. *Biometrics*, 14(1), 18-38. doi:10.2307/2527727
- Brockett, P. L., Derrig, R. A., Golden, L. L., Levine, A., & Alpert, M. (2002). Fraud classification using principal component analysis of RIDITs. *Journal of Risk and Insurance*, 69(3), 341-371. doi:10.1111/1539-6975.00018
- Lieberthal, R. D. (2008). Hospital quality: A PRIDIT approach. *Health services research*, 43(3), 988-1005.

## License
This project is licensed under the Apache License 2.0.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
