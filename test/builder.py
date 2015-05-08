import os.path
from base import Base

class Builder(Base):
    def __init__(self, parsel_file, cpp_file, program_files, options):
        self.parsel_file = parsel_file
        self.cpp_file = cpp_file
        self.program_files = program_files
        self.options = options
        if options == 1:
            self.build()
            self.deleteOldBinary()
            self.compileToCpp()
            self.runProgram()
        elif options == 2:
            self.deleteOldBinary()
            self.compileToCpp()
            self.runProgram()

    def build(self):
        self.toStringOutput("\nRunning cabal build ... \n")
        cmd = "cabal build"
        self.startSubprocess(cmd)

    def deleteOldBinary(self):
        self.toStringOutput("\nDeleting old binary ...\n")
        cmd = "rm compiled_sample/bin/" + self.getName(self.parsel_file)
        self.startSubprocess(cmd)

    def compileToCpp(self):
        self.toStringOutput("\nCompiling parsel file to C++ ... \n") 
        cmd = "dist/build/parsel/parsel " + self.parsel_file
        self.startSubprocess(cmd)

    def runProgram(self):
        if self.programAvailable() == True:
            self.toStringOutput("\nRunning binary ...\n")
            cmd = "compiled_sample/bin/" + self.getName(self.parsel_file) + " "
            for f in self.program_files:
                cmd = cmd + f + " "
            self.startSubprocess(cmd)
        else:
            self.toStringOutput("\nMake failed! \n")
        
    def programAvailable(self):
        if os.path.isfile(self.getName(self.parsel_file)):
            return True
        else:
            return False

    def getName(self, parsel_file):
        filename = parsel_file.split("/")[1]
        filename = filename.split(".")[0]
        return filename
