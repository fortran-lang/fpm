use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Cli {
    /// fpm command
    command: String,
}

fn collect_source_files() -> Vec<String> {
    let mut files: Vec<String> = Vec::new();
    for entry in std::fs::read_dir(".").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if !path.is_dir() {
            let ext = match path.extension() {
                None => "None",
                Some(ext) => ext.to_str().unwrap(),
            };
            if ext == "f90" {
                files.push(path.to_str().unwrap().to_string());
            }
        }
    }
    files
}

fn build() {
    let files = collect_source_files();
    let mut files2: String = String::new();
    for file in &files {
        println!("File: {}", file);
        if !file.ends_with("main.f90") {
            files2 = files2 + " " + &file.replace("\\", "/");
        }
    }
    println!("Files: {:?}", files);
    let s = format!("\
cmake_minimum_required(VERSION 3.5.0 FATAL_ERROR)

enable_language(Fortran)

project(p1)

add_executable(p1 main.f90 {})
", files2);
    std::fs::write("CMakeLists.txt", s).unwrap();

    let mut args: Vec<&str> = vec![];
    if cfg!(windows) {
        args.extend(vec!["-G", "MinGW Makefiles",
                    "-DCMAKE_SH=CMAKE_SH-NOTFOUND"])
    };
    args.extend(vec!["-B", "build", "."]);
    println!("[+] cmake {:?}", args);
    let output = std::process::Command::new("cmake")
                           .args(&args)
                           .env("FC", "gfortran")
                           .output().unwrap();
    println!("status: {}", output.status);
    println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
    if !output.status.success() {
        panic!("Command failed.")
    }

    println!("");
    let args = vec!["--build", "build"];
    println!("[+] cmake {:?}", args);
    let output = std::process::Command::new("cmake")
                           .args(&args)
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
