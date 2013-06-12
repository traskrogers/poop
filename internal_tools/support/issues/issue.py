import urllib
import urllib2
import sys
import encrypt
import base64
import os.path
import cookielib
import url_opener
import issue_parser


class Issue:
    
    def __init__(self, issue, download_directory, display):

#        if len(sys.argv)==1:
#            print "Must supply issue number"
#            sys.exit()
        # elif len(sys.argv)>=2:
        #     issue_number = sys.argv[1]
        #     display = sys.argv[2:]
        
        issue_number = issue

        baseurl = 'http://support.techsoft3d.com'
        loginurl = baseurl + '/login.php'
        theurl = baseurl + '/view.php?id=' + str(issue)

        email = base64.b64decode(encrypt.User().get_email())
        password = base64.b64decode(encrypt.Password().get_password())

        url = url_opener.URL(baseurl, loginurl, theurl, email, password)
        p = issue_parser.Parse(url.get())

#        if os.name == 'nt'
#        down_dir = os.path.join("/", os.path.expanduser("~"), issue_number)
        if not os.path.exists(download_directory):
            os.makedirs(download_directory)
            os.chdir(download_directory)

        if not display:
            p.summary()
            p.description()
            p.download(download_directory)
            p.severity()
            p.platform()
            p.driver()
            p.version()
            p.associated_issues()

        else:
            for i in display:
                if i =="n":
                    p.notes("Note")
                elif i == "e":
                    p.notes("Email")


    
