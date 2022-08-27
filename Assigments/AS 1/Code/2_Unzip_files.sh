
#windows path
Get-ChildItem 'C:\Users\16083\Downloads\Data\' -Filter *.zip | Expand-Archive -DestinationPath 'C:\Users\16083\Downloads\Data\' -Force

######  Mac ######
# set the path
cd /Users/nix/Documents/GitHub/Econ771/Assigments/AS 1/Data
# unzip the files
unzip \*.zip 
#delete the zip files
rm -rf ./*.zip