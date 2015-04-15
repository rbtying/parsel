
from base import Base

class Builder(Base):
    def __init__(self, parsel_file, cpp_file):
        self.parsel_file = parsel_file
        self.cpp_file = cpp_file
        self.build()
        self.compileToCpp()

    def build(self):
        self.toStringOutput("\nRunning cabal build ... \n")
        cmd = "cabal build"
        self.startSubprocess(cmd)

    def compileToCpp(self):
        self.toStringOutput("\nCompiling parsel file to C++ ... \n") 
        cmd = "dist/build/parsel/parsel " + self.parsel_file + " " + self.cpp_file
        self.startSubprocess(cmd)
