# To flush out the contents in case of crash


import fileinput
import sys
import subprocess

filewrite = "../../../hoops_3df/Dev_Tools/hoops_3dgs/source/generic_system.cpp"
subprocess.call(["git", "checkout", filewrite])

for line in fileinput.input(filewrite, inplace=1):
    if "fwrite" in line:
        line = "\tint f = fwrite(buf, 1, count, m_fp);\n\tfflush(m_fp);\n\treturn f;\n"
    sys.stdout.write(line)
