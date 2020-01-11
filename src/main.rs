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
}

fn main() {
    let args = Cli::from_args();
    println!("{:?}", args);
    if args.command == "build" {
        println!("Command: build");
        build();
    } else {
        panic!("Unknown command");
    }
}
