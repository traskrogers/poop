#!/usr/bin/env python
import logging
import os
import sys
from visual_studio import Visual_Studio 

from new_issue import New_Issue

class Support:
    def __init__(self, issue, visualize, bits, debug, incredibuild, compiler, download_directory, build_pv, build_smoke):
        vs = Visual_Studio(visualize, bits, debug, incredibuild, compiler)

        if issue:
            n = New_Issue(issue, download_directory)
            vs.make_project("hoopspartviewer_vc10", "build")         
            vs.make_project("smoke_vc10", "build")


class Build:
    def __init__(self):
        pass

    def build_partviewer(self, partviewer):
        self.partviewer = partviewer

    def build_smoke(self, smoke):
        self.smoke = smoke

    

    
        



if __name__ == '__main__':
    logging.basicConfig(level = logging.DEBUG)
    visualize = os.path.join("C:", "git", "visualize")
    bits = 64
    incredibuild = True
    debug = True
    b = Build()
    build_smoke = True
    build_partviewer = False

    compiler = 2010
    issue = []
    if len(sys.argv):
        issue = sys.argv[1]
    download_directory = os.path.join("C:", "current", str(issue))
    Support(issue, visualize, bits, debug, incredibuild, compiler, download_directory, build_partviewer, build_smoke)
    
