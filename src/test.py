#!/bin/python3

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
    for file_path in file_paths:
        exe = f'{file_path}.out'
        compile_ = f"./scr -o {exe} {file_path}"
        run = f'{exe}'

        try:
            print(f'Compiling {compile_} ...', end='')
            result = subprocess.run(compile_, shell=True, check=False)
            if result.returncode == 0:
                print(' ok')
            elif result.returncode == 1:
                print(f'\n=== COMPILATION FAILED ===\n(return code: {result.returncode})')
                exit(1)
            print(f'Running {exe} ...', flush=True, end='')
            if show:
                result = subprocess.run(run, shell=True, check=False)
            else:
                result = subprocess.run(run, shell=True, check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            if result.returncode == 0:
                print(' ok')
            elif result.returncode == 1:
                print(f'\n=== TEST FAILED ===\n(return code: {result.returncode})')
                exit(1)
            else:
                print(f' failed with an unexpected return code: {result.returncode}')
        except subprocess.CalledProcessError as e:
            print(f"Error executing command on file {file_path}: {e}")

subprocess.run("./clean.sh")
subprocess.run("./build.sh")
directory_path = './tests/'

show = True if len(sys.argv) != 1 and sys.argv[1] == '-s' else False

file_paths = list_files(directory_path)
process_files(file_paths, directory_path, show)
