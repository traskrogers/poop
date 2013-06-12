import os
import fileinput
import re
import glob
import sys
import xml.etree.ElementTree
#import tempFile

class Code2Test:
    def __init__(self, code_gen_path, hoops_path):
        
        os.chdir(code_gen_path)
        cfiles = glob.glob('newcode*.c*')
        hfiles = glob.glob('*.h*')

        test_dir = os.path.join(hoops_path,'hoops_3df','demo','common','standard')
        proj = os.path.join(test_dir, 'std_test_vc10.vcxproj')
        
        clist = ""
        hlist = ""

        for cname in cfiles:
            cname = os.path.join(code_gen_path, cname)
            clist += '      <ClCompile Include="' + cname + '" >\n</ClCompile>\n'

        for hname in hfiles:
            hlist += '      <ClInclude Include="' + hname + '" />\n'    


        itemgroup_open = re.compile("<ItemGroup>")
        itemgroup_close = re.compile("</ItemGroup>")
        com = re.compile("ClCompile Include")
        inc = re.compile("ClInclude Include")
        a = re.compile("<ClCompile[^>]*>")
        b = re.compile("</ClCompile")
        enditem = re.compile('/ItemGroup')

        erase = False
 
        it = iter(fileinput.input(proj, inplace=1))
        for line in it:
            if itemgroup_close.search(line):
                erase = False
            if erase:
                line = ''
            if itemgroup_open.search(line):
                erase = True
            sys.stdout.write(line)

        
        count = False

        it = iter(fileinput.input(proj, inplace=1))
        for line in it:
            if count and itemgroup_open.search(line):
                line = line + clist                
            if not count and itemgroup_open.search(line):
                count = True
            if "SubSystem" in line:
                    line = "      <SubSystem>Console</SubSystem>\n"
            if "IgnoreAllDefaultLibraries" in line:
                    line = "      <IgnoreAllDefaultLibraries>false</IgnoreAllDefaultLibraries>\n"
            if "CompileAs" in line:
                    line = "      <CompileAs>CompileAsCpp</CompileAs>\n"
            sys.stdout.write(line)

                        

        f = open(proj, "r")
        text = f.read()
        f.close()
        f = open(proj, "w")
        f.write(text)
        f.close()



        lastfile = re.compile(cfiles[-1])

        changed = False
        count = 0

#        print proj

        # for line in fileinput.input(proj,inplace=1):
        #     if not changed and com.search(line) and count == 0:
        #         line = ''
        #         changed = True
        #         count = 1
        #     elif changed and not b.search(line) and count == 1:
        #         line = ''
        #     elif changed and b.search(line) and count == 1:
        #         line = ''
        #         changed = False
        #     sys.stdout.write(line)

        # changed = False
        # it = iter(fileinput.input(proj,inplace=1))
        # for line in it:
        #     if com.search(line):
        #         line = ''
        #         next(it)
        #         line = ''
        #     sys.stdout.write(line)



