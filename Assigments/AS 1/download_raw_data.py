# I adapt the code presented in 
# https://opensourceoptions.com/blog/use-python-to-download-multiple-files-or-urls-in-parallel/
# for downloading the Raw data from CMS

#Call libraries
import requests
import time
from multiprocessing import cpu_count
from multiprocessing.pool import ThreadPool
import numpy as np

######## Functions ###############

#Function to download a URL
def download_url(args):
    t0 = time.time()
    url, fn = args[0], args[1]
    try:
        r = requests.get(url)
        with open(fn, 'wb') as f:
            f.write(r.content)
        return(url, time.time() - t0)
    except Exception as e:
        print('Exception in download_url():', e)

 #Function to handle the parallel download
def download_parallel(args):
    cpus = cpu_count()
    results = ThreadPool(cpus - 1).imap_unordered(download_url, args)
    for result in results:
        print('url:', result[0], 'time (s):', result[1])

##### CReate the lists

# Create a list with the files URL to download HHAFY
csm = 'https://downloads.cms.gov/Files/hcris/'
types = ['HHAFY', 'HHA20FY', 'SNFFY', 'SNF10F', 'HOSPFY', 'HOSP10FY']
years = list(range(1994, 2022+1))
urls = [csm + str(i) + str(j) + '.zip' for i in types for j in years]

# Create a list for the local path to download the files.
local = r'C:\Users\16083\Downloads\Data'
fns = [local + '\\' + str(i) + '_' + str(j) + '.zip' for i in types for j in years] 

#zip both in one argument, neccesary for the next step
inputs = zip(urls, fns)

download_parallel(inputs)


