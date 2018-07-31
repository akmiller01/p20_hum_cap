import requests
from bs4 import BeautifulSoup
import zipfile
import glob
import pdb
import os

def unzip(source_filename, dest_dir):
    zip_ref = zipfile.ZipFile(source_filename,'r')
    zip_ref.extractall(dest_dir)
    zip_ref.close()

url = "https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1"
dataRoot = "https://stats.oecd.org/FileView2.aspx?IDFile="
filePath = "/home/alex/git/p20_hum_cap/project_data/zips"

download_page = requests.get(url).content
soup = BeautifulSoup(download_page,"lxml")
links = soup.findAll("a")
for link in links:
    onclick = link.attrs["onclick"]
    fileName = link.text.split("/")[0].strip()+".zip"
    print(fileName)
    fileID = onclick[15:-3].replace("_","-")
    file_url = dataRoot+fileID
    r = requests.get(file_url,stream=True)
    with open(filePath+fileName,'wb') as f:
        for chunk in r.iter_content(chunk_size=1024):
            if chunk:
                f.write(chunk)

#Find .zip in folder
paths = glob.glob(filePath+"*.zip")

#Iterate through paths and unzip
for inPath in paths:
    filename = os.path.basename(inPath)
    unzip(inPath,filePath)
