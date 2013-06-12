import glob
import os
import shutil
import fileinput
import sys
import test

class CriticalFinder:
    def __init__(self, l, code_gen_path, hoops_path):
        self.code_gen_path = code_gen_path
        self.hoops_path = hoops_path
        begin = 1
        end = len(l)
        isworking = "Does bug show? Enter 0 or 1"
        self.copy_files(end)
        bug = raw_input(isworking)
        assert bug == '1', 'code gen is not working if it is doing what it is supposed to'

        critical = []
        newfilenames = self.copy_files(end)

        while len(critical) < len(newfilenames):
            end = len(newfilenames)-len(critical)
            newfilenames = self.bin_search(begin, end, isworking)
            len_critical = len(critical)
            critical.append(len(newfilenames)-len(critical))
            print critical
            print len_critical_files
            print newfilenames


    def string(self, fileno ):
        
        s = str(fileno)
        if fileno == 0:
            s = ''
        return s

    def clean_up(self):
        newfiles = glob.glob('newcode*.c')
        for i in newfiles:
            os.remove(i)

    def copy_file(self, l):
        for i in l:
            s = self.string(i)
            shutil.copy2('parsedcode' + s + '.c', 'newcode' + s + '.c')

    #copy files to test
    def copy_files(self, end):
        self.clean_up()
        self.copy_file(range(0,end))
        newfilenames = glob.glob('newcode*.c')
        for line in fileinput.input(newfilenames[-1], inplace=1):
            if "extern int code_chain" in line:
                line = ''
            if "return code_chain" in line:
                line = 'return 0;'
            sys.stdout.write(line)
        test.Code2Test(self.code_gen_path, self.hoops_path)
        return newfilenames


    #search for the last file that shows bug
    def bin_search(self, begin, end, teststr):
        while not begin==end:
            mid = (begin+end)/2
            self.copy_files(mid)
            bug = raw_input(teststr)
            if bug == '0':
                begin = mid + 1
                end = end
            elif bug == '1':
                begin = begin
                end = mid
        return self.copy_files(end)
        




        

        
        
