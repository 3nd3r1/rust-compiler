use clap::{Parser, Subcommand};
use clap_stdin::FileOrStdin;
use std::fs;

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Compile {
        #[arg(default_value = "-")]
        input_file: FileOrStdin,
        #[arg(long)]
        output: String,
    },
    Serve {
        #[arg(long, default_value = "127.0.0.1")]
        host: String,
        #[arg(long, default_value_t = 3000)]
        port: u16,
    },
}

fn main() {
    let args = Args::parse();

    match args.command {
        Commands::Compile { input_file, output } => {
            let source_code = input_file.contents().expect("Failed to read input");
            match call_compiler(&source_code) {
                Ok(executable) => {
                    fs::write(output, &executable).expect("Failed to write output file");
                }
                Err(e) => {
                    eprintln!("Compilation error: {}", e);
                    std::process::exit(1);
                }
            }
        }
        Commands::Serve { host, port } => {
            run_server(&host, port);
        }
    }
}

fn run_server(host: &str, port: u16) {
    let addr = format!("{}:{}", host, port);
    let listener = std::net::TcpListener::bind(&addr).expect("Failed to bind");
    println!("Starting server at {}", addr);

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                std::thread::spawn(|| {
                    handle(stream);
                });
            }
            Err(e) => {
                eprintln!("Connection failed: {}", e);
            }
        }
    }
}

fn handle(stream: std::net::TcpStream) {
    todo!()
}

fn call_compiler(source_code: &str) -> Result<Vec<u8>, String> {
    todo!()
}
