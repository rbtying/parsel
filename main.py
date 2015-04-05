import sys
from test.builder import Builder
from test.base import Base
from test.tester import Tester
def main():    
    options = {1 : moreArgs, 2 : moreArgs, 3 : moreArgs, 4 : enoughArgs }
    options[len(sys.argv)]()
    
    b = Builder(sys.argv[1], sys.argv[2])
    t = Tester(sys.argv[2], sys.argv[3])

def moreArgs():
    print "\npython main.py <psl file> <cpp file to compile to> <example compiled cpp>.\n"
    sys.exit()

def enoughArgs():
    pass

main()
