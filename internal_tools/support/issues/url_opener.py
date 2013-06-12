#much of the code is gotten from these websites:
#www.voidspace.org.uk/python/articles/cookielib.shtml
#stackoverflow.com/questions/189555/how-to-use-python-to-login-to-a-webpage-and-retrieve-cookies-for-later-usage

import urllib
import urllib2
import cookielib

# handles passwords and cookies
class URL:
    def __init__(self, baseurl, loginurl, theurl, email, password):

        passman = urllib2.HTTPPasswordMgrWithDefaultRealm()
        passman.add_password(None,loginurl,email,password)

        authhandler = urllib2.HTTPBasicAuthHandler(passman)
        cookiehandler = urllib2.HTTPCookieProcessor(cookielib.LWPCookieJar())
        opener = urllib2.build_opener(cookiehandler, authhandler)
        login_data = urllib.urlencode({'email':email, 'passwd':password})
        opener.open(loginurl, login_data)
        urllib2.install_opener(opener)
        self.url = urllib2.urlopen(theurl)
    
    def get(self):
        return self.url.read()
