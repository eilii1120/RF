## Usage

1. Run the R script
2. Select a CSV-formatted data file in the file dialog that appears
3. The program will automatically process the data and fit the linear mixed model
4. Results will be saved in the output directory

## Data Format Requirements

Input data should be in CSV format with the following columns:
- `time`: Numeric time variable
- `subject`: Grouping variable (will be converted to factor)
- `abundance`: Numeric response variable

## Output Files

The program will generate the following files in the output directory:
  
1. `model_summary.txt`: Complete model summary information
2. `time_coefficient.csv`: Detailed statistics for time coefficient (estimate, standard error, t-value, p-value)
3. `confidence_intervals.txt`: 95% confidence intervals for model parameters

## Sample Data and Results

This project includes a sample dataset `data2_for_LMM.csv` and the complete output results from running the analysis on this data in the `LMM_results` directory for reference.
 

### Sample Output Contents
- Model fit summary including fixed and random effect estimates
- Statistical significance test results for the time variable
- Confidence interval estimates for model parameters

## Model Formula
abundance ~ time + (1 | subject)


