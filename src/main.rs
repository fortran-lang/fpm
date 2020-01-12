use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Cli {
    /// fpm command
    command: String,
}

fn collect_source_files() -> Vec<std::path::PathBuf> {
    let mut files: Vec<std::path::PathBuf> = Vec::new();
    for entry in std::fs::read_dir(".").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if !path.is_dir() {
            let ext = match path.extension() {
                None => "None",
                Some(ext) => ext.to_str().unwrap(),
            };
            if ext == "f90" {
                files.push(path);
            }
        }
    }
    files
}

fn build() {
    let files = collect_source_files();
    for file in &files {
        println!("File: {}", file.to_str().unwrap());
    }
    println!("Files: {:?}", files);
    let s = "\
cmake_minimum_required(VERSION 3.5.0 FATAL_ERROR)

enable_language(Fortran)

project(p1)

set(SRC
    a.f90
    b.f90
)

add_executable(p1 main.f90 ${SRC})
";
    std::fs::write("CMakeLists.txt", s).unwrap();
    let output = std::process::Command::new("cmake")
                           .args(&["-B", "build", "."])
                           .output().unwrap();
    println!("status: {}", output.status);
    println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
    if !output.status.success() {
        panic!("Command failed.")
    }

    let output = std::process::Command::new("cmake")
                           .args(&["--build", "build"])
                           .output().unwrap();
    println!("status: {}", output.status);
    println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
    if !output.status.success() {
        panic!("Command failed.")
    }
}

fn run() {
    let output = std::process::Command::new("build/p1")
                                        .output().unwrap();
    println!("status: {}", output.status);
    println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
    if !output.status.success() {
        panic!("Command failed.")
    }
}

fn main() {
    let args = Cli::from_args();
    println!("{:?}", args);
    if args.command == "build" {
        println!("Command: build");
        build();
    } else if args.command == "run" {
        println!("Command: run");
        build();
        run();
    } else {
        panic!("Unknown command");
    }
}
