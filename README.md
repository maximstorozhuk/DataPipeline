# File System Navigation

- All CSV files downloaded or generated are under the csv folder
- XLSX files, including the data dictionary are under the excel folder
- All python scripts can be found under the python folder

## Dataset

https://www.kaggle.com/datasets/cviaxmiwnptr/nba-betting-data-october-2007-to-june-2024

## Steps Taken

Everything past this point is in chronological order. Follow the steps to reproduce the data.

### Data Cleaning

Steps taken can be found under dictionaries subdirectory in excel folder, in file nba_2008-2025_dictionary.xlsx. Input was nba_2008-2025.csv, result written to file nba_2008-2025_cleaned.csv

### Data Generation

Generate additional data from the cleaned data using Python. Scripts can be found under the python folder. File deriveFields.py was used to generate the extra data fields. Input was nba_2008-2025_cleaned.csv, result written to file nba_2008-2025_extended.csv. Detailed descriptions of how and why this was done can be found under the 'TransformationDictionary' sheet of nba_2008-2025_dictionary.xlsx.

### Save to BigQuery

For the operations and analysis that are being done in this project, this is not necessary. However, if you would like to access the data through BigQuery, you can request a key.