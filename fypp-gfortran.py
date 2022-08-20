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
filenames = [str(f) for f in Path(".").glob("**/*.fypp")]

# Filter out the macro definitions.
macros = [arg for arg in args if arg.startswith("-D")]

# Filter out the include paths with -I prefix.
include_paths = [arg for arg in args if arg.startswith("-I")]

# Declare preprocessed array.
preprocessed = []

# Preprocess the .fypp files.
for filename in filenames:

    # Get the output filename only without extension and path.
    output_filename = Path(filename).stem

    # Save the output_file to build directory.
    output_file = str(Path(output_dir, f"{output_filename}.f90"))

    subprocess.run(
        [
            "fypp",
            filename,
            *include_paths,
            *macros,
            output_file,
        ],
    )

    # Append the output_file to preprocessed array.
    # Along with the path to store object files.
    # This will save the object files in preprocessed_files directory.
    preprocessed.append(f"-c {output_file} -o {str(Path(output_dir, f'{output_filename}.o'))}")

# Run gfortran for each preprocessed file.
for file in preprocessed :
    file_args = file.split()
    subprocess.run(
        ["gfortran"]  + file_args,
        check=True,
    )

subprocess.run(["gfortran"] + args)
