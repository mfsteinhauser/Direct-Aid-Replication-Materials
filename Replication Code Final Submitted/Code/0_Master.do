/**************************************************************************

	DESCRIPTION: 	This .do file sets the path directories and runs all the
					do files.

**************************************************************************/

**************************************************************************
* 0. PREAMBLE
**************************************************************************

clear all
clear matrix
clear mata
set more off
estimates clear

* Setting relevant directories

**** IMPORTANT: SET YOUR OWN PATH TO THE FOLDER IN WHICH THE REPLICATION FILES ARE LOCATED
/*
INSTRUCTIONS

1. Uncomment the code after the last path defined:

else if "`c(username)'" == "INSERT USERNAME"	{
	global folder "INSERT PATH TO THE WORKING DIRECTORY"
}

2. Replace "INSERT USERNAME" with the actual username of your PC.
3. Replace "INSERT PATH TO THE WORKING DIRECTORY" with the actual path to the working directory in your PC.
*/
		
global	data		"${folder}/Data"
global	output		"${folder}/Output"
global	code		"${folder}/Code"

**** Downloading packages needed to run do files 
foreach package in unique texsave regsave reghdfe ftools rwolf2 coefplot ivreg2 RANKTEST leebounds randtreat{
     capture which `package'
     if _rc==111 ssc install `package'
}

********************************************************************************
* 1. RUNNING ALL DO FILES AT ONCE
********************************************************************************
* The do file's name indicates which corresponding table/figure in the paper the do file generates

* Tables 1 and A4
do "${code}/Table_1_A5"

* Tables 2 and A2
do "${code}/Table_2_A3"

* Tables 3 and A6
do "${code}/Table_3_A7"

* Tables 4
do "${code}/Table_4"

* Table A1
do "${code}/Table_A1" 

* Table A2
do "${code}/Table_A2" 

* Table A4
do "${code}/Table_A4" 

* Table A7
do "${code}/Table_A6"

* Table A12
do "${code}/Table_A12"

* Figure 2
do "${code}/Figure_2"

* Figure 3
do "${code}/Figure_3"

* Figure A2
do "${code}/Figure_A2"

* Figure A3
do "${code}/Figure_A3"












