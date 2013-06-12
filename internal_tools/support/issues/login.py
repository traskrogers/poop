import base64

class Password:
    def __init__(self):
        self.password = base64.b64encode('password')

    def get_password(self):
        return self.password

class User:
    def __init__(self):
        self.email = base64.b64encode('user@techsoft3d.com')
    
    def get_email(self):
        return self.email
