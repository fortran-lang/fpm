use structopt::StructOpt;
use toml::Value;
use std::path::{Path, PathBuf};
use std::env;

#[derive(Debug, StructOpt)]
struct Cli {
    /// fpm command
    command: String,

    /// Directory for all generated artifacts
    #[structopt(long, name="DIRECTORY", default_value = "target")]
    target_dir : PathBuf,
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

fn build(target_dir: &Path) {
    let target = PathBuf::from(target_dir);
    println!("TARGET_DIR: {}", target_dir.to_str().unwrap());
    let value = std::fs::read_to_string("fpm.toml")
        .unwrap()
        .parse::<Value>().unwrap();
    println!("TOML: {:?}", value);
    let files = collect_source_files();
    let mut files2: String = String::new();
    for file in &files {
        println!("File: {}", file);
        if !file.ends_with("main.f90") {
            files2 = files2 + " ../" + &file.replace("\\", "/");
        }
    }
    println!("Files: {:?}", files);
    let s = format!("\
cmake_minimum_required(VERSION 3.5.0 FATAL_ERROR)

enable_language(Fortran)

project(p1)

add_executable(p1 ../main.f90 {})
", files2);
    let mut cmakelists = target;
    cmakelists.push("CMakeLists.txt");
    std::fs::create_dir_all(target_dir).unwrap();
    std::fs::write(cmakelists.to_str().unwrap(), s).unwrap();

    let mut args: Vec<&str> = vec![];
    if cfg!(windows) {
        args.extend(vec!["-G", "MinGW Makefiles",
                    "-DCMAKE_SH=CMAKE_SH-NOTFOUND"])
    };
    args.extend(vec!["-B", "build", "."]);
    println!("[+] cmake {:?}", args);
    let fc : String = match env::var("FC") {
        Ok(val) => val,
        Err(_) => "gfortran".to_string(),
    };
    let output = std::process::Command::new("cmake")
                           .args(&args)
                           .current_dir(target_dir)
                           .env("FC", fc)
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
                           .current_dir(target_dir)
                           .output().unwrap();
    println!("status: {}", output.status);
    println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
    if !output.status.success() {
        panic!("Command failed.")
    }
}

fn p1_bin(target_dir: &Path) -> std::process::Command {
    let mut fpm_bin_relative: std::path::PathBuf = target_dir.to_path_buf();
    fpm_bin_relative.push("build");
    fpm_bin_relative.push("p1");
    fpm_bin_relative.set_extension(std::env::consts::EXE_EXTENSION);
    let fpm_bin_absolute = std::fs::canonicalize(fpm_bin_relative).unwrap();
    std::process::Command::new(fpm_bin_absolute.to_str().unwrap())
}

fn run(target_dir: &Path) {
    let output = p1_bin(target_dir)
                                        .current_dir(target_dir)
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
        build(args.target_dir.as_path());
    } else if args.command == "run" {
        println!("Command: run");
        build(args.target_dir.as_path());
        run(args.target_dir.as_path());
    } else {
        panic!("Unknown command");
    }
}
