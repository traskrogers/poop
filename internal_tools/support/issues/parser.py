from bs4 import BeautifulSoup
#requires beautifulsoup - www.crummy.com/software/BeautifulSoup/bs4/doc
import re

#parse html files

class Parse:
    def __init__(self, html):
        self.html = html
        self.soup = BeautifulSoup(html)
        text = self.soup.get_text()
        self.text = re.sub("&quot;", '"', text) # may need to add more as needed to 

    def pprint(self):
        print self.soup.prettify()

    def title(self):
        return self.soup.title.string

    def text(self):
        print self.text

############## following are utility functions and at some point should be moved to separate module ##################

    def printif(self, header, text):
        if text:
            print header + text

    def between(self, text, begin, end):
        return text.split(begin)[1].split(end)[0].strip()

    #decorator to check if first argument is in list of allowed keywords
    @staticmethod
    def accepts(*allowed):
        def wrap(function):
            def wrapper(self, *form):
                try:
                    if form[0] not in allowed:
                        raise Exception
                except Exception:
                    print "Error in acceptable parameter in " + self.__module__ + "." + self.__class__.__name__ + "." + function.func_name
                    exit(0)
                return function(self, *form)
            return wrapper
        return wrap

