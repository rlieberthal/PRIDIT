# Set the working directory
setwd('/Users/rlieberthal/Documents/PRIDIT-main/data')

# Read in the test data
data <- read.csv("test.csv")

# Generate a vector of IDs
ID.data <- data[, 1]

# Generate the ridit weights for the example data
ridit.data <- ridit(data)

# Generate the PRIDIT weights for the example data
PRIDITweights.data <- PRIDITweight(ridit.data)

# Generate the PRIDIT scores for the example data
PRIDITscore.data <- PRIDITscore(ridit.data, ID.data, PRIDITweights.data)
