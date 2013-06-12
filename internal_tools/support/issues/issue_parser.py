from bs4 import BeautifulSoup
#requires beautifulsoup - www.crummy.com/software/BeautifulSoup/bs4/doc
import urllib2
import re
import os
import parser
import note_parser

#parse issue number

baseurl = "http://support.techsoft3d.com/"
class Parse(parser.Parse):

    def download(self, path):
        for link in self.soup.find_all('a'):
            name = link.get('title')
            dl = link.get('href')
            download = re.compile("download file \((.*) -")
            for (i,j) in zip(str(name).splitlines(), str(dl).splitlines()):
                m = download.search(i)
                if m:
                    dlpath = os.path.join(path,m.group(1))
                    if not os.path.isfile(dlpath):  # security vulnerabilities may be possible
                        f = open(dlpath, "w")
                        url = baseurl + j
                        f.write(urllib2.urlopen(url).read())
            
    def description(self):
        text = self.between(self.text, "fixed width font)\n\n","Description is currently").splitlines()
        text = os.linesep.join([line for line in text if line.strip()])
        self.printif("Description:\n ", text)

    def summary(self):
        self.printif("Summary: ", self.between(self.text, "Summary:","Initial Description"))

    def associated_issues(self):
        self.printif("Issues: ", self.between(self.text, "Associated Issues:", "Reporter"))

    def driver(self):
        self.printif("Driver: ", self.between(self.text, "Driver:", "File Format:"))

    def version(self):
        self.printif("Version: ", self.between(self.text, "Version Tested:","Target Release:"))

    def platform(self):
        self.printif("Platform: ", self.between(self.text, "Platform:", "Driver:"))

    def status(self):
        self.printif("Status: ", self.between(self.text, "Status:", "Sales Account Manager:"))

    def severity(self):
        self.printif("Severity: ", self.between(self.text, "Severity:","Bug/Feature Summary:"))

    def notes(self, form):
        i = [0]
        for link in self.soup.find_all('a'):
            dl = link.get('href')
            self.parse_notes(form, dl, i)

    @parser.Parse.accepts("Note", "Email")
    def parse_notes(self, form, dl, i):
        match = re.compile("")
        if form == "Note":
            match = re.compile("expand\(.*note.*?(\d*)'\)")
        elif form == "Email":
            match = re.compile("expand\(.*email.*?(\d*)-(\d*)'\)")
        for item in str(dl).splitlines():
            m = match.search(item)
            if m:
                i[0] +=1
                url = self.url(form, m)
                soup = BeautifulSoup(urllib2.urlopen(url))
                x = note_parser.Parse(form, urllib2.urlopen(url).read())
                x.print_notes(form, m, i)

    @parser.Parse.accepts("Note", "Email")
    def url(self, form, m):
        if form == "Note":
            return baseurl + "view_note.php?id=" + m.group(1)
        elif form == "Email":
            return baseurl + "view_email.php?ema_id=" + m.group(1) + "&id=" + m.group(2)
