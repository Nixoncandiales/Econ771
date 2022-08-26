# I adapt the code presented in 
# https://opensourceoptions.com/blog/use-python-to-download-multiple-files-or-urls-in-parallel/
# for downloading the Raw data from CMS

#Call libraries
import requests
import time
from multiprocessing import cpu_count
from multiprocessing.pool import ThreadPool
import numpy as np

# Create a list with the files URL to download
url = 'https://downloads.cms.gov/Files/hcris/HHAFY'
years = list(range(1994, 2022+1))
urls = [url + str(i) +'.ZIP' for i in years]

# Create a list for the local path to download the files.
fns=r'C:\Users\16083\Downloads\Data\HHAFY_'
fns= [fns+str(i)+'.ZIP' for i in years]

#zip both in one argument, neccesary for the next step
inputs = zip(urls, fns)

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

 download_parallel(inputs)