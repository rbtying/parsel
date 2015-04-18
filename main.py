from os import listdir
from os.path import isfile, join
from time import sleep
import sys
import os
from test.builder import Builder
from test.base import Base
from test.tester import Tester
def main():    
    options = {1 : moreArgs, 2 : moreArgs, 3 : moreArgs, 4 : enoughArgs, 5: enoughArgs }
    options[len(sys.argv)]()
    
    b = Builder(sys.argv[1], sys.argv[2])
    t = Tester(sys.argv[2], sys.argv[3], 0)
    onlyfiles = [ f for f in listdir("src") if isfile(join("src",f)) ]
    files = []
    for f in onlyfiles:
        if len(f.split(".")) == 2:
            if f.split(".")[1] == "hs":
                files.append("src/" + f)
    times = []
    for f in files:
        times.append(os.stat(f).st_mtime)
    flag = False
    if len(sys.argv) == 5 and sys.argv[4] == "-v":
        flag = True
    changes = 1
    while True and flag:
        try:
            counter = 0
            while counter < len(files):
                if os.path.isfile(files[counter]):
                    current = os.stat(files[counter]).st_mtime
                    if current != times[counter]:
                        b = Builder(sys.argv[1], getCppName(sys.argv[2], changes%2))
                        t = Tester(sys.argv[2], sys.argv[3], 1)
                        changes = changes + 1
                        times[counter] = current
                counter = counter + 1
        except:
            print ""
def moreArgs():
    print "\npython main.py <psl file> <cpp file to compile to> <example compiled cpp> (-v for running log)\n"
    sys.exit()

def enoughArgs():
    pass

def getCppName(filename, change):
    splitted = filename.split(".")
    if change == 0:
        return splitted[0] + "1." + splitted[1]
    else:
        return splitted[0] + "2." + splitted[1]
        

main()
