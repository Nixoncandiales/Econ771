clear
cd  "~/Documents/Github/Econ771/Assigments/As 3/"

* Run the Analysis and sabe the data
do Code/RunAnalysis Output/Analysis
* Save in CSV format
export delimited using "Data/Data.csv", replace
* Save in Stata format
save "Data/Data.dta", replace

* Reload the data
use Data/Data.dta, clear
*Q2
do Code/Q2 20 10 Output/fig
