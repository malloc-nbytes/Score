#!/bin/python3

# This was the original file that was used
# for running the tests. However, Score is
# mature enough to self-host the tests so
# this file purely exists as a backup in
# case the Score test file fails.

import os
import sys
import subprocess

def list_files(directory):
    file_paths = []
    try:
        for filename in os.listdir(directory):
            file_path = os.path.join(directory, filename)
            if os.path.isfile(file_path):
                file_paths.append(file_path)
    except OSError as e:
        print(f"Error reading files in directory {directory}: {e}")

    return file_paths

def process_files(file_paths, directory_path, show):
    passed, failed = 0, 0
    for file_path in file_paths:
        exe = f'{file_path}.out'
        compile_ = f"./scr {file_path}"
        # print(f'[COMPILE] {compile_} ...', end='')
        result = subprocess.run(compile_, shell=True, check=False)
        if result.returncode == 1:
            print(f'\n=== COMPILATION FAILED {file_path} ===\n(return code: {result.returncode})')
            exit(1)

    for file_path in file_paths:
        exe = f'{file_path}.out'
        run = f'{exe}'
        try:
            newline = '\n' if show else ''
            print(f'[TEST RUN] {exe} ...{newline}', flush=True, end='')
            if show:
                result = subprocess.run(run, shell=True, check=False)
            else:
                result = subprocess.run(run, shell=True, check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            if result.returncode == 0:
                passed += 1
                print(' ok')
            else:
                codes = {-11: 'segfault'}
                err = codes[result.returncode] if result.returncode in codes else 'UNKNOWN'
                print(f' FAILED [err: {err} ({result.returncode})]')
                failed += 1
        except subprocess.CalledProcessError as e:
            print(f"Error executing command on file {file_path}: {e}")
    return passed, failed

subprocess.run("./clean.sh")
subprocess.run("./build.sh")
directory_path = './tests/'

print(f'tests directory: {directory_path}')
show = True if len(sys.argv) != 1 and sys.argv[1] == '-s' else False
file_paths = list_files(directory_path)
print(f'gathered tests: {file_paths}')

passed, failed = process_files(file_paths, directory_path, show)

print(f'files passed: {passed}')
print(f'files failed: {failed}')

