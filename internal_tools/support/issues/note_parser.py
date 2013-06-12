import parser
import re
import urllib2

class Parse(parser.Parse):
    def __init__(self, form, html):
        parser.Parse.__init__(self,html)
        
    def print_notes(self, form, m, i):
        title = self.title(form, i)
        begin = self.begin(form)
        end = self.end(form)
        self.printif(title, self.between(self.text, begin, end))
        print "\n---------------------------------------\n"

    @parser.Parse.accepts("Note", "Email")
    def title(self, form, i):
        if form == "Note":
            begin = "Title:"
            end = "Message:"
        elif form == "Email":
            begin = "Subject:"
            if "Attachments:" in self.text:   
                end = "Attachments:"
            else:
                end = "Message"
        title = self.between(self.text, begin, end)
        title = form + " " + str(i[0]) + ": " + title + "\n"
        return title

    @parser.Parse.accepts("Note", "Email")
    def begin(self, form):
        if form == "Note":
            return "Message:\n(display in fixed width font)"
        elif form == "Email":
            return "Raw Headers"

    @parser.Parse.accepts("Note", "Email")
    def end(self, form):
        if form == "Note":
            return u"\xa0\xa0"
        elif form == "Email":
            if  "-----Original Message-----" in self.text:
                end = "-----Original Message-----"
            else:
                end = "Eventum 2.2"
            return end
