from os import listdir
from os.path import isfile, join
from time import sleep
import sys
import os
from test.builder import Builder
from test.base import Base
from test.tester import Tester
def main():    
    options = {1 : moreArgs, 2 : enoughArgs, 3 : enoughArgs, 4 : enoughArgs, 5: enoughArgs }
    options[len(sys.argv)]()
    if len(sys.argv) > 2:
        sample_file = sys.argv[2]
    b = Builder(sys.argv[1], "out.cpp")
    t = Tester("out.cpp", "out.cpp", 0)
    onlyfiles = [ f for f in listdir("src") if isfile(join("src",f)) ]
    files = []
    for f in onlyfiles:
        if len(f.split(".")) == 2:
            if f.split(".")[1] == "hs":
                files.append("src/" + f)
    onlyfiles = [ f for f in listdir("cpp") if isfile(join("cpp",f)) ] 
    for f in onlyfiles:
        files.append("cpp/" + f)
    times = []
    for f in files:
        times.append(os.stat(f).st_mtime)
    flag = False
    if len(sys.argv) == 4 and sys.argv[3] == "-v":
        flag = True
    if len(sys.argv) == 3 and sys.argv[2] == "-v":
        flag = True
    changes = 1
    while True and flag:
        try:
            sleep(1)
            try:
                counter = 0
                while counter < len(files):
                    if os.path.isfile(files[counter]):
                        current = os.stat(files[counter]).st_mtime
                        if current != times[counter]:
                            b = Builder(sys.argv[1], getCppName("out.cpp", changes%2))
                            if len(sys.argv) > 2:
                                t = Tester("out.cpp", sample_file, 2)
                            else:
                                t = Tester("out.cpp", "out.cpp", 1)
                            changes = changes + 1
                            times[counter] = current
                    counter = counter + 1
            except:
                print ""
        except KeyboardInterrupt:
            sys.exit()
def moreArgs():
    print "\npython main.py <psl file> (-v for running log)\n"
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
