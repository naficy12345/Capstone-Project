Capstone project
========================================================
author: Shahrzad N.
date: 6/5/2016
Data Science Capstone
by Johns Hopkins University


Introduction
========================================================

More and more people using their mobile devices to type text for email, social networking, etc.  The goal of this capstone project is to make it easier for people to type on their mobile devices by predicting the next word they intend to type based on the previous words they have already typed. 
The prediction algorithm used here is using 3 input data (blogs, news, twitter) and creates the frequency of bi-grams and tri-grams used to predict the next word. 


Steps in developing the Next word prediction Application
========================================================
There are two phases in development of the Next Word Applicaltion.

1- Preprocessing Phase which uses the 3 input files and creates 2 CSV files which have tri and bi grams and their frequencies as follows:

- Input the 3 given text files (blogs, news, twitter) in to R
- Data cleansing
    - removing punctuations, stop words, white space, and numbers. 
    - convert to lower case
- create bi-gram and tri-gram of the cleaned data with their frequencies

- save the bi-gram and tri-gram files to be used for prediction

Steps in developing the Next word prediction Application (Continue)
========================================================
2- Application phase which is developed with shinny app and uses the 2 tri and bi gram files and matches the last two words of the input text with the tri-gram csv file with the highest frequency and finds the next word.  In case there is no match found in the tri-gram file the program would try to match the last word of the text within the bi-gram file.  If there is no match either in the tri-gram or bi-gram file the program would return a meassage that could not find the next word.

All the application related files are under https://github.com/naficy12345/Capstone-Project

Restrictions and Assumptions
========================================================
- 70% of the data from the 3 resources were randomly sampled and added to gether as one file prior to data cleansing.

- used the textcnt function in R to produce bi-grams and tri-grams
    - due to computer memory restriction (4GB) the creation of bi-grams and     tri-grams were done in chunks.
    
- Used the aggregate function in R to sum the frequencies of bi-grams and tri- grams and saved them as two csv files sorted by frequencies.  

- in order to increase the application performance, the tri-gram, and bi-gram csv files only have the entries with the frequencies greater than ten.