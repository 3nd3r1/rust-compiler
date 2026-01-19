use clap::{Parser, Subcommand};
use clap_stdin::FileOrStdin;

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
            println!("Compile command with {:?}, {}", input_file.contents().unwrap(), output);
        }
        Commands::Serve { host, port } => {
            println!("Server command with {}, {}", host, port);
            run_server(&host, port);
        }
    }
}

fn run_server(host: &str, port: u16) {
    todo!()
}

fn call_compiler(source_code: &str) -> Result<Vec<u8>, String> {
    todo!()
}
