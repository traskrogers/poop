import os
import subprocess
from issue import Issue 
import logging
import imp
import sys




class New_Issue:

    def __init__(self, issue, download_directory):

        self.issue = issue
        cwd = os.getcwd()
        subprocess.call(["git", "checkout", "-b", str(issue)])
        Issue(issue, download_directory, [])

        os.chdir(os.path.join(cwd, "..", "..", "..", "hoops_3df", "demo", "common", "sanity"))

        f, name, desc = imp.find_module("diet_smoke", [os.path.join(cwd, "..", "..", "..", "hoops_3df", "demo", "common", "sanity")])
        module = imp.load_module("diet_smoke", f, name, desc)

        self.prompt_filename()
        module.Diet_Smoke([issue])

    def check_in(self):

        filenames = glob(str(self.issue) + ".cpp")
        for filename in filenames:
            subprocess.call(["git", "add", filename])
        subprocess.call(["git", "commit", "-a", "-m", "'" + str(self.issue) + "'"])
        subprocess.call(["git", "checkout", "master"])
        for filename in filenames:
            subprocess.call(["git", "checkout", str(self.issue), "--", filename])

    def prompt_filename(self):
        name = raw_input("Enter Filename (with spaces)")
        print name.split()
        subprocess.call(["cp", "9999_key_to_str.cpp", str(self.issue)+"_"+"_".join(name.split())+".cpp"])








    








