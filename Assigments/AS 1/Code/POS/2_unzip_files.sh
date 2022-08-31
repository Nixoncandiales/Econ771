
######  Windows ######
Get-ChildItem 'C:\Users\16083\Documents\GitHub\Econ771\Assigments\AS 1\Data\POS' -Filter *.zip | Expand-Archive -DestinationPath 'C:\Users\16083\Documents\GitHub\Econ771\Assigments\AS 1\Data\POS' -Force
Get-ChildItem 'C:\Users\16083\Documents\GitHub\Econ771\Assigments\AS 1\Data\POS' -Filter *.zip | rm

######  Mac ######
# set the path
#cd /Users/nix/Documents/GitHub/Econ771/Assigments/AS 1/Data
# unzip the files
#unzip \*.zip 
#delete the zip files
#rm -rf ./*.zip