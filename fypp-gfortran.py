#!/usr/bin/env python3
import sys
import subprocess
from pathlib import Path

# Read the system arguments.
args = sys.argv[1:]

# Set the output directory.
output_dir = Path("build" , "preprocessed_files")

# If output directory does not exist, create it.
if not output_dir.exists():
    output_dir.mkdir(parents=True)

# Get the filenames with .fypp extension and convert to string.
source_file = [arg for arg in args if arg.endswith(".fypp")]
output_file = [arg for arg in args if arg.endswith(".o")]

if len(source_file) != len(output_file):
    subprocess.run(["gfortran"] + args, check=True)
    sys.exit(0)

source_file = source_file[0]
output_file = output_file[0]
preprocessed = output_file.replace(".o", ".f90")

# Filter out the macro definitions.
macros = [arg for arg in args if arg.startswith("-D")]

# Filter out the include paths with -I prefix.
include_paths = [arg for arg in args if arg.startswith("-I")]

subprocess.run(
    ["fypp", "-n", source_file, preprocessed] + macros + include_paths,
    check=True
)

args = [arg for arg in args if arg != source_file and not arg in macros] + [preprocessed]
subprocess.run(["gfortran"] + args, check=True)
