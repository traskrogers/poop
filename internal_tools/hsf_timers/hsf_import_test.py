#! /usr/bin/env python
# hsf_import_test.py
from __future__ import print_function
import os
from subprocess import call, Popen, PIPE
from glob import glob
from sys import exit, argv, stderr
from time import sleep

NUM_TRIES = 3


def main(argv):
    build_first = False
    for arg in argv:
        if arg in ('-b', '--build'):
            build_first = True

    script_dir = os.path.abspath(os.path.dirname(__file__))
    bin_dir_3df = '{}/../../hoops_3df/bin/nt_i386_vc11'.format(script_dir)
    bin_dir_hps = '{}/../../hps/bin/win32_vc11'.format(script_dir)
    test_files_dir = r'C:\sample_data'
    test_file_subdirs = ['aec', 'cae_fea_cfd', 'cam', 'mcad', 'medical', 'plant_process']

    test_files = []
    for subdir in test_file_subdirs:
        test_files += glob('{}/{}/*.hsf'.format(test_files_dir, subdir))
    if not test_files:
        print('no test files found - check that test_files_dir is set to a valid local directory')
        exit(1)

    # Build 3df and hps projects
    if build_first:
        slnfile = '{}/hsf_timers_vc11.sln'.format(script_dir)
        projs = ['3df_hsf_timer_vc11', 'hps_hsf_timer_vc11']
        for proj in projs:
            print('Building {}'.format(proj), file=stderr)
            cmd = ['buildconsole', slnfile, '/prj={}'.format(proj), '/cfg=Release^|Win32', '/BUILD']
            with open(os.devnull, 'w') as devnull:
                rc = call(cmd, stdout=devnull, stderr=devnull)
                if rc != 0:
                    print('Build of project {} failed'.format(proj), file=stderr)
                    exit(1)

    results = []
    for f in test_files:
        # Test 3df hsf import
        os.chdir(bin_dir_3df)
        perf_3df = test_model(f, '3df')

        # Test hps hsf import
        os.chdir(bin_dir_hps)
        perf_hps = test_model(f, 'hps')

        # Make TestResult instance and add to results list
        tr = TestResult(f.replace('/', os.sep).replace('\\', os.sep), perf_3df, perf_hps)
        results.append(tr)

    results_sorted = sorted(results, key=lambda r: r.percent_change(), reverse=True)
    for r in results_sorted:
        print(r)
        print('')


def test_model(model, whichframework):
    result = 0.0
    num_fails = 0
    if whichframework not in ('hps', '3df'):
        print('please specify hps or 3df for the second argument')
        exit(2)
    for x in range(NUM_TRIES):
        timer_output = Popen(['{}_hsf_timer.exe'.format(whichframework), model], stdout=PIPE).communicate()[0]
        try:
            result += float(timer_output)
        except ValueError:
            num_fails += 1
        sleep(1)
    if num_fails == NUM_TRIES:
        return 0
    else:
        return round(result / (NUM_TRIES - num_fails))


class TestResult:
    def __init__(self, model, result_3df=0, result_hps=0):
        self.model = model
        self.result_3df = result_3df
        self.result_hps = result_hps

    def percent_change(self):
        if 0 not in (self.result_hps, self.result_3df):
            delta = self.result_hps - self.result_3df
            return delta / self.result_3df
        else:
            return 0

    def __str__(self):
        result_3df = self.result_3df or 'failed'
        result_hps = self.result_hps or 'failed'
        result = '{}:\n'.format(self.model) + \
            '3dF:\t{}\n'.format(result_3df) + \
            'HPS:\t{}'.format(result_hps)

        if 'failed' not in (result_3df, result_hps):
            delta = result_hps - result_3df
            percent = self.percent_change()
            result += '\ndelta:\t{}\n'.format(delta) + \
                'change:\t{:+.0%}'.format(percent)
        return result


main(argv[1:])
