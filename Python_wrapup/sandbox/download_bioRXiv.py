#!/usr/bin/python

import requests # for downloading web pages
import os # to interface with the operating system
import re # regular expressions

import sys # for command line arguments

def download_category(category = "ecology"):
    # check if directory exists 
    if os.path.exists('data/' + category) == False:
        # if the directory does not exist, create it using command line
        os.system('mkdir -p data/' + category)
    # download first page: first form the right url
    url = 'https://www.biorxiv.org/collection/' + category + '?page=0'
    # and then download it
    first_page = requests.get(url, allow_redirects=True)
    # save the first page for parsing later
    with open('data/' + category + '/0.html', 'wb') as f:
        f.write(first_page.content)
    # now try to find how many pages are available
    num_pages = re.findall(r'Go to page (\d+)', first_page.content.decode('UTF8'))[-1]
    num_pages = int(num_pages)
    # download all of the pages
    print('about to download', num_pages, 'pages')
    for i in range(1, num_pages):
        print('downloading', category, i, 'of', num_pages)
        url = 'https://www.biorxiv.org/collection/' + category + '?page=' + str(i)
        page = requests.get(url, allow_redirects=True)
        with open('data/' + category + '/' + str(i) + '.html', 'wb') as f:
            f.write(page.content)
    return None


# If run from command line, launch category 
# stored in first argument
if __name__ == "__main__":
   download_category(sys.argv[1])

