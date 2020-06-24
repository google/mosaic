use std::{
    env,
    error::Error,
    io::{self, Write},
    path::{Path, PathBuf},
    process::{self, Command, Output},
};

fn main() -> Result<(), Box<dyn Error>> {
    let binary = PathBuf::from(&env::args_os().next().unwrap());
    let source_root = {
        let mut root = binary.parent().unwrap();
        while !root.join("Cargo.toml").is_file() {
            root = root.parent().unwrap();
        }
        root
    };
    let nocapture = env::args().skip(1).any(|a| a == "--nocapture");

    // Run cargo build with output so it doesn't look like a test is taking a long time to run.
    let build_result = Command::new("cargo")
        .arg("build")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    if !build_result.success() {
        eprintln!("cargo build failed with code {}", build_result);
        process::exit(102);
    }

    println!("");

    let mut failures = vec![];
    let mut passes = 0usize;
    for test in source_root.join("test").join("run-make").read_dir()? {
        let test = test?.path();
        if !test.is_dir() {
            continue;
        }
        let test_name = test.to_string_lossy().to_string();

        print!("test {} ... ", test_name);
        io::stdout().flush().unwrap();

        let result = run_make_test(test.as_path(), nocapture);
        match result {
            TestResult::Ok => {
                println!("ok");
                passes += 1;
            }
            TestResult::Failed(output) => {
                println!("failed");
                failures.push((test_name, output));
            }
        }
    }

    let failed = !failures.is_empty();
    if failed && !nocapture {
        println!("");
        println!("failures:");
        println!("");
        for (test, output) in &failures {
            let output = output.as_ref().unwrap();
            if !output.stdout.is_empty() {
                println!("---- {} stdout ----", test);
                io::stdout().lock().write_all(&output.stdout)?;
            }
            if !output.stderr.is_empty() {
                println!("---- {} stderr ----", test);
                io::stdout().lock().write_all(&output.stderr)?;
            }
        }

        println!("");
        println!("failures:");
        for (test, _) in &failures {
            println!("    {}", test);
        }
    }

    let result = if failed { "FAILED" } else { "ok" };
    println!("");
    println!(
        "test result: {}. {} passed, {} failed",
        result,
        passes,
        failures.len()
    );

    if failed {
        process::exit(101);
    }

    Ok(())
}

enum TestResult {
    Ok,
    Failed(Option<Output>),
}

fn run_make_test(test: &Path, nocapture: bool) -> TestResult {
    let tmpdir = tempfile::tempdir().expect("Could not create temporary directory");

    let mut cmd = Command::new("make");
    cmd.current_dir(test).env("TMPDIR", tmpdir.path());
    let (status, output) = if nocapture {
        let status = cmd.spawn().expect("failed to run test").wait().unwrap();
        (status, None)
    } else {
        let output = cmd.output().expect("failed to run test");
        (output.status, Some(output))
    };
    if status.success() {
        TestResult::Ok
    } else {
        TestResult::Failed(output)
    }
}
