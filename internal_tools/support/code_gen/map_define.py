from itertools import dropwhile
from pprint import pprint
import re
import fileinput
import glob

class Map:
    def __init__(self):
        filenames = glob.glob('code*.c')

        chain = re.compile("code_chain")
        ret = re.compile("return\s+0;")
        mapre = re.compile("stdlib.h")

        define = re.compile("(\s*)DEFINE")
        hashtag = re.compile(r"\s*\#")
        subre = re.compile("(.*)LOOKUP\s*\((0x[0-9A-F]*),.*?\)(.*)")

        for filename in filenames:
            with open("parsed" + filename, "wb") as f:
                with open(filename, "rb") as g:
                    changed = False
                    text = g.readlines()
                    for line in text:
                        if not changed and mapre.search(line):
                            line +="\n#include <map>\n#include <string>\n"
                            if filename =="code.c":
                                line += "std::map<HC_KEY,HC_KEY> key_map;\n"
                            else:
                                line += "extern std::map<HC_KEY,HC_KEY> key_map;\n"
                            changed = True
                        else:
                            if not hashtag.search(line):
                                m = define.search(line)
                                if m:
                                    begin = "(".join(line.split('(',1)[1:])
                                    item = ")".join(begin.split(')')[:-1])
                                    val = item.split(",")
                                    value2 = ','.join(val[:-2])
                                    line = m.group(1) + 'key_map[' + val[-2].lstrip(' ') + '] = ' + value2 + ';\n'
                                n = subre.search(line)
                                if n:
                                    line = n.group(1) + 'key_map[' + n.group(2) + ']' + n.group(3) + "\n"
                        f.write(line)

