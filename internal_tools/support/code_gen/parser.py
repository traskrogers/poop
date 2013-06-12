from itertools import dropwhile
import re
import os
import tempfile

class Parser:
    def __init__(self, text):
        self.split_main_code(text)
        self.list = []

    #From stackoverflow - Markus Jarderot
    def comment_remover(self):
        def replacer(match):
            s = match.group(0)
            if s.startswith('/'):
                return ""
            else:
                return s
        pattern = re.compile(
            r'//.*?$|/\*.*?\*/|\'(?:\\.|[^\\\'])*\'|"(?:\\.|[^\\"])*"',
            re.DOTALL | re.MULTILINE
        )
        self.top = re.sub(pattern, replacer, self.top)
        self.code = re.sub(pattern, replacer, self.code)

    def print_data(self):
        print self.top
        print self.code
        print self.bottom
                    
    def write(self, filename):
        f = open(filename, 'w+')
#        temp = '\n'.join(map(str, self.data))
        f.write(self.top)
        f.write("\n")
        f.write(self.code)
        f.write("\n")
        f.write(self.bottom)
#        f.write(self.data)

    def split_main_code(self, text):
        mainre = re.compile(r"int\s*?main")
        otherre = re.compile(r"int\s*?code_chain_")
        s = ''
        if mainre.search(text):
            s = text.partition("int main (int argc, char **argv) {")
            t = s[2].partition("return code_")
            self.top = s[0] + s[1]
            self.code = t[0]
            self.bottom = t[1]+ t[2]
        elif otherre.search(text):
            s = text.partition("int code_chain_")
            self.top = s[0] + s[1]
            s = s[2].partition("(void) {")
            self.top = self.top + s[0] + s[1]
            t = s[2].partition("return code_")
            self.code = t[0]
            self.bottom = t[1]+ t[2]
        else:
            print "exception"
            
    def first_parse(self):
        x = CodeParser(self.code)
        x.remove_blank_lines()
        x.switch_driver("opengl")
        self.code = x.return_string()

class CodeParser:

    def __init__(self, code):
        self.code = code

    #From stackoverflow - Lawrence Johnston
    def remove_blank_lines(self):
        self.code = os.linesep.join([s for s in self.code.splitlines() if s.strip()])

#    from stackoverflow - thg435
    def parse(self, l):
        openre = re.compile("HC_Open_(.*)")
        closere = re.compile("HC_Close_(.*)")
        for line in l:
            if not openre.search(line):
                yield line
                if closere.search(line):
                    break
            else:
                yield [line] + list(self.parse(l))

    def switch_driver(self,driver):
        self.code = self.code.replace("dx9", driver)

    def return_list(self):
        l = list(self.parse(iter(self.code.splitlines())))
        for i in l:
            print i            
            pass
        return l

    def return_string(self):
        return self.code
                  
class HCList:
    def __init__(self):
        self.l = []
        self.critical = True

    def append(self, line):
        self.l.append(line)

    def sublist(self):
        x = HCList()
        self.l.append(x)

    def print_list(self):
        print self.l
