import glob
import os
import tempfile
import re
import fileinput
#import sys
import parser
#import tempfile
import map_define
#import test
#import shutil
import file_finder


code_gen_path = os.path.join("c:", "code_gen")
hoops_path = os.path.join('c:', 'git', 'visualize_dev')

#code_gen_path = os.path.join(os.path.expanduser("~"),"Documents","projects", "code_gen")
#hoops_path = os.path.join(os.path.expanduser("~"), "Documents", "projects", "techsoft", "visualize")
#code_gen_dir = os.path.join("/","cygdrive", "c", "git", "visualize_dev", "internal_tools", "support", "code_gen")

os.chdir(code_gen_path)



#substitute keys/defines/lookups
map_define.Map()
parsedfilenames = glob.glob('parsedcode*.c')
length = range(0,len(parsedfilenames)
file_finder.CriticalFinder(length, code_gen_path, hoops_path)



def parse_code(filename):
    x = parser.Parser(filename.read())
    x.comment_remover()
    x.first_parse()
    x.write(filename)




    
    

#    works = True
#    while works:
#        parse_code(filename)
#        works = raw_input("isworking")

#        pass
  
#print newfilenames
#parse individual code files
