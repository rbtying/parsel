from os import listdir
from os.path import isfile, join
from time import sleep
import sys
import os
from test.builder import Builder
from test.base import Base
from test.tester import Tester
def main():    
    options = {1 : moreArgs, 2 : enoughArgs, 3 : enoughArgs, 4 : enoughArgs, 5: enoughArgs, 6: enoughArgs, 7: enoughArgs, 8: enoughArgs, 9: enoughArgs }
    options[len(sys.argv)]()
    FILETYPE = ""
    if len(sys.argv) > 3:
        if sys.argv[2].split(".")[1] == "cpp":
            output_file = sys.argv[2]       
            FILETYPE = "cpp"
        if sys.argv[2].split(".")[1] == "wav":
            sample_file = sys.argv[2]
            FILETYPE = "wav"
    arg_files = getArgFiles(sys.argv)
    b = Builder(sys.argv[1], "out.cpp", arg_files , 1)
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
    files.append(sys.argv[1])
    times = []
    for f in files:
        times.append(os.stat(f).st_mtime)
    flag = False
    if sys.argv[len(sys.argv)-1] == "-v":
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
                            b = Builder(sys.argv[1], getCppName("out.cpp", changes%2), arg_files, getBuilderOption(files[counter]))
                            if getBuilderOption(files[counter]) == 1:
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

def getBuilderOption(filename):
    string = filename.split(".")
    if string[1] == "cpp" or string[1] == "h" or string[1] == "c" or string[1] == "psl":
        return 2
    elif string[1] == "hs":
        return 1

def getCppName(filename, change):
    splitted = filename.split(".")
    if change == 0:
        return splitted[0] + "1." + splitted[1]
    else:
        return splitted[0] + "2." + splitted[1]

def getArgFiles(args):
    arg_files = []
    for arg in sys.argv:
        if len(arg.split(".")) == 2:
            if arg.split(".")[1] == "wav":
                arg_files.append(arg)
        else:
            try:
                check = int(arg)
                check += 1
                arg_files.append(arg)
            except TypeError:
                pass
            except ValueError:
                pass
    return arg_files

main()
