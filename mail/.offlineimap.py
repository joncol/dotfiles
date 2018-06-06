import os
import subprocess

def mailpasswd(account):
    path = "%s/.passwd/%s.gpg" % (os.environ["HOME"], account)
    return subprocess.check_output(["gpg2", "--quiet", "--batch", "-d",
                                    path]).strip()
