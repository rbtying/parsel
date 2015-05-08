from base import Base

class Tester(Base):
    def __init__(self, compiled_file, example_file, option):
        self.compiled_file = compiled_file
        self.example_file = example_file
        self.option = option
        self.expanded_file = self.compiled_file.split(".")
        if self.option == 1:
            self.toStringOutput("\nComparing compiled files ...\n") 
            cmd = "diff " + self.expanded_file[0] + "1.cpp " + self.expanded_file[0] + "2.cpp -p"
            self.startSubprocess(cmd)
        if self.option == 2:
            self.toStringOutput("\nComparing compiled files ...\n")
            cmd = "diff " + self.compiled_file + " " + self.example_file + " -p"
            self.startSubprocess(cmd)
    def testStuff():
        pass
