# DS4002
## Section 1: Software and platform used
- **Software:** Jupyter Notebook, Excel, GitHub
- **Add-on packages on Python Notebook:** TextBlob, pandas, numpy, matplotlib
- **Platform:** Mac and Windows

## Section 2: A Map of our Documentation
This project contains three main folders: A Data Folder, A Scripts Folder, and an Output Folder. Each folder contains the work files of our data collection, processing, analysis, and presentation. Below we will outline the purpose and layout of each folder, and if applicable, its hierarchy and sub-folders.

### Data
This folder contains five files:
- Our raw movie data called “raw_data.csv”
- Our cleaned-up data called “Movie_Theater.csv”
- Our sentiment analysis data called “sentiment_analysis_results”
- A combination of the sentiment analysis and cleaned-up data called “combined_rating_and_sentiment_analysis.csv”
- Data Appendix

We gathered our raw data using the Phantom Web Extension, which automatically uploaded the scraped data into the raw_data.csv. We put all of the raw results from the web scraper into the file.

We then cleaned up the raw data by deleting several columns that contained sensitive information or were unnecessary for our analysis. The result was exported into the “Movie_Theater.csv” file.

In order to conduct our sentiment analysis, we used a Python notebook (located in scripts folder), and ran a sentiment analysis program on our cleaned data. The results of this analysis were exported to the “sentiment_analysis.csv”. Lastly, we combined the cleaned data with the sentiment analysis into the “combined_rating_and_sentiment_analysis.csv”.

Additionally, we included the Data Appendix.

### Scripts
The only file in this folder is the Python notebook that was used to perform sentiment analysis on our cleaned-up data. This script is called “sentiment_analysis.ipynb”.

### Output
There are __ files in this folder:
- A pdf of our Excel data analysis called “excel_analysis.pdf”
- Our PowerPoint presentation called “”

### Miscellaneous
The only other files within the overall repository are:
- “LICENSES.md”
- “references.pdf”
- This ReadMe file

These files were not placed in a sub-folder and remain in the main project folder.

## Section 3: Instructions for reproducing results
### Gathering Data
We gathered our data using a chrome extension called “Phantom - GMB Audit Tool” ([link](https://chromewebstore.google.com/detail/phantom-gmb-audit-tool/ppjhoknoadhakhhdbbjjinlopefmehik)). We then used this audit tool by going to the google reviews of our three businesses and clicking the “Review Audit” button located on the Review Section to the right of the webpage. 

From there, the audit tool automatically added the reviews to a csv file. We repeated this process for all the other movie theaters we looked at, and combined all the csv files into one excel file.

We then cleaned up the data for sentiment analysis. We removed unnecessary columns and private information.  

### Sentiment Analysis 
Our combined dataset, “Movie_Theater.csv” is imported into a Jupyter notebook via url and converted into DataFrame “movies_df”.

1. Define a function to get sentiment polarity and subjectivity using TextBlob. 
2. Apply the function to each row of the “review content” column.
3. Convert the completed sentiment analysis into a new DataFrame for export.
4. Export DataFrame to CSV.

We then added the exported sentiment analysis to our Movie_Theater.csv and created the final copy called “combined_rating_and_sentiment_analysis.csv”.
