Readme
================

To use this code run the following scrpits:

## 1_download_CMS_HCRIS.py
This python script will parallel-download all the zip files in [CMS](https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Cost-Reports/Cost-Reports-by-Fiscal-Year)
in a local directory.

To make this script work go to line 47 and change the directory where you want to download the files local='your directory'.

## 2_unzip_files.sh
This Shell script will unzip the compressed files and delete the zip version. It has suggested code for Windows and Mac OS. Works faster in Mac. To make it work update the relative paths based on the changed in the directory.

## 3_HCRIS_Data.r
This R script will combine the multiple cvs files into a unique data set. It takes into account the 2 form reporting versions (1996, 2010). Note the 1996 version goes from 1998-2011 and the 2010 runs from 2011-2022.
Note this file will source H1_HCRISv1996.r and H2_HCRISv2010.r  update the relative paths and working directory. 
The script is authored by Dr. Ian McCarthy, I forked from [here](https://github.com/imccart/HCRIS). (Note I adjust the relative path).
This script creates the merged data base (and 2 auxiliary datasets) Output/HCRIS/HCRIS_Data.txt

