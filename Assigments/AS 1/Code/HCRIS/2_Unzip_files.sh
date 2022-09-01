
######  Windows ######
Get-ChildItem 'C:\Users\16083\Documents\GitHub\Econ771\Assigments\AS 1\Data\HCRIS' -Filter *.zip | Expand-Archive -DestinationPath 'C:\Users\16083\Documents\GitHub\Econ771\Assigments\AS 1\Data\HCRIS' -Force
Get-ChildItem 'C:\Users\16083\Documents\GitHub\Econ771\Assigments\AS 1\Data\HCRIS' -Filter *.zip | rm

######  Mac ######
# set the path
cd /Users/nix/Documents/GitHub/Econ771/Assigments/AS\ 1/Data/HCRIS
# unzip the files
unzip \*.zip 
#delete the zip files
rm -rf ./*.zip