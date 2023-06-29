# PRIDIT
PRIDIT packages for R

# Project Name

The project provides three R scripts: `ridit.R`, `PRIDITweight.R`, and `PRIDITscore.R`.

## Table of Contents

- [Description](#description)
- [Usage](#usage)
- [Contributing](#contributing)
- [License](#license)

## Description

The project provides a set of R scripts for calculating and analyzing Ridit scores and PRIDIT (Principal Component Analysis applied to Ridit scores) scores for a given dataset.

- `ridit.R` - This script calculates the Ridit scores for a given dataset. Ridit analysis is a non-parametric method used to compare ordinal categorical variables across groups. The script takes a dataset as input and returns the Ridit scores for each observation.

- `PRIDITweight.R` - This script applies Principal Component Analysis (PCA) to Ridit scores and calculates the weighted PRIDIT scores for a given dataset. The Ridit scores represent the ordinal categorical variables. The script takes a dataset and the weights for each Ridit score as input and returns the weighted PRIDIT scores.

- `PRIDITscore.R` - This script applies Principal Component Analysis (PCA) to Ridit scores and calculates the PRIDIT scores for a given dataset. The Ridit scores represent the ordinal categorical variables. The script takes a dataset as input and returns the PRIDIT scores.

## Usage

To use the scripts, follow these steps:

1. Load the required R packages (if any) using the `library()` function.

2. Source the script(s) you need using the `source()` function. For example:

```R
source("ridit.R")
```

3. Call the relevant function(s) from the script(s) with the appropriate arguments. For example:

```R
ridit_scores <- calculate_ridit(dataset)
```

4. Use the results returned by the function(s) as needed in your analysis or further processing.

## Contributing

Contributions to this project are welcome. If you want to contribute, please follow these steps:

1. Fork the repository.

2. Create a new branch for your feature or bug fix.

3. Make your changes and commit them with descriptive commit messages.

4. Push your changes to your forked repository.

5. Submit a pull request to the main repository, explaining the changes you have made.

## License

This project is licensed under the [MIT License](LICENSE). You are free to use, modify, and distribute the code in accordance with the terms of the license.

---

Feel free to customize this README file according to your project's specific details and requirements.
