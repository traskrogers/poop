#!/usr/bin/env python
import os
import argparse
from sys import argv, exit
from subprocess import call
import json
import multiprocessing

# Script to unify the outward interface to building, packaging
# and running tests in hps and 3df.  The primary function is to
# remove this logic from Buildbot and put it into source-controlled
# files versioned with visualize.
#

# Argparse
ap = argparse.ArgumentParser(
    description='Unified control building/packaging/testing of HPS and 3DF.')
ap.add_argument('product', choices=['3df', 'hps'],
    help='Product name')
ap.add_argument('platform',
    help='I.e. win64_vc10/osx32/lin64/srcpkg')
ap.add_argument('action', choices=['build', 'package', 'test'],
    help='Action to take')
ap.add_argument('--all-properties', dest='json_all_props',
    help='Used by Buildbot')
ap.add_argument('-v', '--verbose', dest='verbose', help='Verbose output',
    action='store_true', default=False)
a = ap.parse_args()

# fill out "all_props" dict if provided on command line
all_props = {}
if a.json_all_props:
    all_props = json.loads(a.json_all_props)

# figure out directory paths
mcdir = os.path.dirname(os.path.abspath(__file__))
HPSDIR = os.path.abspath(os.path.join(mcdir, '../../hps'))
TDFDIR = os.path.abspath(os.path.join(mcdir, '../../hoops_3df'))
PKGDIR = os.path.abspath(os.path.join(mcdir, '../../pkgs'))


def main(argv):
    # Shortcut fuctions; if there is a function named
    # action_product (i.e. 'build_3df' or 'test_hps') then
    # just call that function with 'platform' as the only
    # parameter.  Otherwise proceed to 'elif's below.
    funcname = '{}_{}'.format(a.action, a.product)
    if funcname in globals():
        commands = globals()[funcname](a.platform)
    else:
        print 'I got an unknown product of {}'.format(a.product)

    for build_command in commands:
        vlog('Running cmd [{}] ({})'.format(build_command.name, build_command.cmd))
        rv = build_command.run()
        if rv != 0:
            exit(rv)
    # done
    exit(0)


def vlog(msg):
    if a.verbose:
        print msg


def getHoopsPlatform(bbplat):
    # Translates "Buildbot-style" platform names to "hoops-style"
    if bbplat.startswith('win'):
        # i.e. win32_vc90 or win64_vc10
        front, vc = bbplat.split('_')
        if '64' in front:
            return 'nt_x64_{}'.format(vc)
        return 'nt_i386_{}'.format(vc)
    elif bbplat.startswith('lin'):
        return 'linux_x86_64' if '64' in bbplat else 'linux_x86'
    elif bbplat.startswith('osx'):
        return 'osx_x86_64' if '64' in bbplat else 'osx_x86'
    #else
        # TODO: what happens here?  Just return None?  Exception?
    return None


def build_3df(plat):
    # TODO: implement
    if plat == 'srcpkg':
        short_rev = all_props['short_revision']
        return [BuildCommand(
            command=['perl', 'build/bin/srcpkgr.pl', '--outfile',
                '{}/{}.tgz'.format(PKGDIR, short_rev), '--git-revision',
                short_rev],
            dir=TDFDIR)]

    hplat = getHoopsPlatform(plat)
    if plat.startswith('win'):
        return [BuildCommand(
            command='perl win-make.pl -b -o {}'.format(hplat).split(),
            dir=TDFDIR)]
    elif plat.startswith('lin'):
        cores = str(multiprocessing.cpu_count())
        bv = all_props.get('build_variant')
        myplat = hplat
        return [BuildCommand(
            command=['make', '-j', cores, myplat],
            dir=TDFDIR)]
    elif plat.startswith('osx'):
        arch = 'x86_64' if '64' in plat else 'i386'
        bv = all_props.get('build_variant')
        scheme = 'hoops_osx_package' if 'official' in bv else 'hoops_osx'
        bcmd = [BuildCommand(
            command=['xcodebuild', '-workspace', 'dev_hoops_3df_osx.xcworkspace',
                 '-scheme', scheme, '-arch', arch, '-configuration', 'Debug'],
                dir=TDFDIR)]
        if bv == 'official':
            bcmd.append(BuildCommand(
            command=['xcodebuild', '-workspace', 'dev_hoops_3df_osx.xcworkspace',
                 '-scheme', scheme, '-arch', arch, '-configuration', 'Release'],
                dir=TDFDIR))
        return bcmd
    # else... what?


def test_3df(plat):
    if not plat.startswith('win)'):
        return None

    hplat = getHoopsPlatform(plat)
    return [BuildCommand(
        command=['python', 'miscellaneous/nightly_testing/autosmoke.py',
            '-d', 'opengl', '-t', '0000', 'bin/{}'.format(hplat)],
        dir=TDFDIR)]


def package_3df(plat):
    hplat = getHoopsPlatform(plat)
    if plat == 'commonsrc':
        return [
            BuildCommand(
                command=['perl', 'build/bin/pkg_readablesource.pl'], dir=TDFDIR),
            BuildCommand(
                command=['perl', 'build/bin/pkg_commonsource.pl'], dir=TDFDIR)]
    elif plat.startswith('win'):
        return [BuildCommand(
            command='perl win-make.pl -p -o {}'.format(hplat).split(),
            dir=TDFDIR)]
    elif plat.startswith('lin') or plat.startswith('osx'):
        return [BuildCommand(
            command='perl build/bin/pkg_nonwin.pl {}'.format(hplat).split(),
            dir=TDFDIR)]


def build_hps(plat):
    if plat.startswith('lin'):
        cores = str(multiprocessing.cpu_count())
        return [BuildCommand(
            command=['make', '-j', cores, plat],
            dir=HPSDIR)]


def test_hps(plat):
    # TODO: implement
    print 'Here is where I would test hps'


def package_hps(plat):
    # TODO: implement
    print 'Package hps'


class BuildCommand:

    def __init__(self, *args, **kwargs):
        """
        Initialize a BuildCommand object

        Args:
            command: a string or array to be passed directly
                to subprocess.call
            name: (opt) Name for this command
            dir: (opt) Chdir to this directory before executing
                command
        Returns:
            A BuildCommand object

        """
        if 'command' not in kwargs:
            raise Exception('You must provide a "command" to create a BuildCommand')
            return None
        self.cmd = kwargs['command']
        self.dir = kwargs.get('dir')
        self.name = kwargs.get('name')

    def run(self):
        # this chdir junk seems janky and fragile
        here = os.getcwd()
        if self.dir:
            print 'Changing dir to {}'.format(self.dir)
            os.chdir(self.dir)
        print 'Running cmd: "{}"'.format(self.cmd)
        retval = call(self.cmd)
        if self.dir:
            print 'Changing dir back to {}'.format(here)
            os.chdir(here)
        return retval


main(argv[1:])
