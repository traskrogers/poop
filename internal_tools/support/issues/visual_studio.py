import logging
from new_issue import New_Issue
import os
import subprocess
import re
#import argparse

class Visual_Studio:

    def __init__(self, visualize, bits, debug, incredibuild, compiler):
        self.visualize = visualize
        self.bits = bits
        self.debug = debug

        self.compiler = compiler
        self.builder = ""
        if bits == 32:
            framework = "Framework"
        elif bits == 64:
            framework = "Framework64"
        else:
            raise Exception
        if incredibuild:
            self.builder = os.path.join(os.environ['ProgramFiles(x86)'], "Xoreax", "Incredibuild", "buildconsole")
        else:

            self.builder = os.path.join(os.environ['WINDIR'], "Microsoft.NET", framework, "v4.0.30319", "MSBuild.exe")
        if self.vc10():
            self.project = "vc10.sln"

        elif self.vc9():
            self.project = "vc9.sln"
                
        self.solution = os.path.join(visualize, "hoops_3df")

    def vc10(self):
        return self.compiler == 2010 or self.compiler == 10 or self.compiler == "vc10"

    
    def vc9(self):
        return self.compiler == 2008 or self.compiler == 9 or self.compiler == "vc9"
        

    def bin_dir(self):
        bin_dir = "nt_"
        try:
            if self.bits == 32:
                bin_dir += "i386_"
            elif self.bits == 64:
                bin_dir += "x64_"
            else:
                raise Exception
            if self.vc10():
                bin_dir += "vc10"
            elif self.vc9():
                bin_dir += "vc90"
            else:
                raise Exception
            if self.debug:
                bin_dir += "d"
        except:
            pass
        return bin_dir

    def debug_ending(self):
        if self.debug:
            return "d"



    def make_project(self, project, action):
        #action is either build, rebuild, or clean
        cfg = ""
        if self.debug:
            cfg = "Debug|"
        else:
            cfg = "Release|"
        if self.bits==32:
            cfg += "Win32"
        else:
            cfg += "x64"
        
        
        logging.debug([self.builder, os.path.join(self.solution, "dev_hoops_3df_" + self.project), "/" + action, "/prj="+project, "/cfg=" + cfg])
        subprocess.call([self.builder, os.path.join(self.solution, "dev_hoops_3df_" + self.project), "/" + action, "/prj="+project, "/cfg=" + cfg])

    def run_project(self, project, *args):
        path = os.path.join(self.visualize, "hoops_3df", "bin", self.bin_dir(), project + self.debug_ending())
        logging.debug(path)
        subprocess.call(path)

    def create_dict(self):
        with open(os.path.join(self.solution, "dev_hoops_3df_" + self.project)) as f:
            
            f.read()


if __name__ == '__main__':
    pass


#./visual_studio.py build smoke -bits 32 -mode debug
#./visual_studio.py run partviewer 
