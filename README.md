# Replication Materials of "Can Digital Aid Deliver During Humanitarian Crises?"

_By Michael Callen, Miguel Fajardo-Steinhäuser, Michael Findley & Tarek Ghani_



This replication archive contains two types of files. In order to replicate the article’s results, first create a main folder. Inside the main folder, create the following three folders, and place the replication files inside each of these folders in the following way:

1.	Folder “Data”: Contains the data used for the analysis. In this folder, place the following .dta files:
   
      a.	surveysDataset: survey data from all survey waves.
  
      b.	cleanedBaselineData: baseline data.
  
      c.	cleanedTransactionData: transaction-level data.
  
      d.	ExpertsSurvey: data from the experts’ predictions.
  
      e.	cleanedFollowUpData: follow-up data

3.	Folder “Code”: Contains all Stata do files used to generate each of the manuscript’s exhibits, including those in the Online Appendix. The name of each do file indicates the table/figure that a given do file generates (with exhibits from the Online Appendix having the prefix “A” – for example “Table_A2”). These were created using Stata 17. The master do file, where relative paths are set and the programs needed to run the different do files are installed, is called “0_master”. Each do file is named after the exhibit it reproduces.

Programs to install: texsave, regsave, rwolf2, unique, reghdfe, ftools, coefplot, ivreg2, RANKTEST, leebounds, randtreat. 

**Important**: Before running any of the do files, make sure to adjust the relative paths in the “0_master” do file and run the globals setting the paths. 

**Important**: User needs to have Stata version 16 or later to execute the do files.

3.	Folder “Output”: Any output produced by the do files will be stored here, named after the corresponding exhibit in the manuscript. 

There is also a spreadsheet called “CostEffCalculations” that contains the calculations used in the cost effectiveness and cost efficiency calculations. 

The replication folder should look as follows: 

![image](https://github.com/user-attachments/assets/fea18a6a-63b5-431a-aaf4-31a43234d7c3)

