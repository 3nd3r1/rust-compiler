use clap::{Parser, Subcommand};

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Compile {
        input_file: Option<String>,
        #[arg(long)]
        output_file: String,
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
        Commands::Compile { input_file, output_file } => {
            println!("Compile command with {:?}, {}", input_file, output_file);
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
