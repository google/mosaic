// Copyright (c) 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use cc_crate as cc;
use itertools::Itertools;
use std::{
    env,
    error::Error,
    io::{self, Write},
    path::{Path, PathBuf},
    process::{self, Command, Output},
};
use trybuild as tb;

struct Opts {
    nocapture: bool,
    noclean: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let binary = PathBuf::from(&env::args_os().next().unwrap());
    let source_root = {
        let mut root = binary.parent().unwrap();
        while !root.join("Cargo.toml").is_file() {
            root = root.parent().unwrap();
        }
        root
    };
    let nocapture = env::args().skip(1).any(|a| a == "--no-capture");
    let noclean = env::args().skip(1).any(|a| a == "--no-clean");
    let opts = Opts { nocapture, noclean };

    // Run cargo build with output so it doesn't look like a test is taking a long time to run.
    let build_result = Command::new("cargo")
        .arg("build")
        .arg("--all")
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    if !build_result.success() {
        eprintln!("cargo build failed with code {}", build_result);
        process::exit(102);
    }

    println!("");

    let ui_failed = std::panic::catch_unwind(|| {
        let strategy = RunGenerator {
            source_root: source_root.into(),
        };
        let t = trybuild::TestCases::new_with_strategy(strategy);
        t.compile_fail("test/ui/*.cc");
        t.compile_fail("test/ui/*.rs");
    })
    .is_err();

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

        let result = run_make_test(test.as_path(), &source_root, &opts);
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

    if !failures.is_empty() && !opts.nocapture {
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

    let failed = !failures.is_empty() || ui_failed;
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

#[derive(Debug)]
struct RunGenerator {
    source_root: PathBuf,
}
impl tb::Strategy for RunGenerator {
    fn prepare(&mut self, _tests: &[tb::Test]) -> tb::Result<tb::Project> {
        let mut project = tb::Project::new()?;
        project.source_dir = self.source_root.clone();
        Ok(project)
    }

    fn build(&self, test: &tb::Test) -> tb::Result<Command> {
        let mut cmd = if cfg!(windows) {
            Command::new(self.source_root.join("target\\debug\\peasy.exe"))
        } else {
            Command::new(self.source_root.join("target/debug/peasy"))
        };
        cmd.arg(test.path()).env("TERM", "dumb");
        Ok(cmd)
    }

    fn run(&self, _test: &tb::Test) -> tb::Result<Command> {
        unimplemented!()
    }

    fn normalize(&self, _project: &tb::Project, test: &tb::Test, output: &str) -> String {
        output
            .lines()
            .map(|line| {
                const PAT: &str = "┌─ ";
                if line.trim_start().starts_with(PAT) {
                    let cut_start = line.find(PAT).unwrap() + PAT.len();
                    if let Some(cut_end) = line.rfind(&['/', '\\'][..]) {
                        return String::from(&line[..cut_start]) + "$DIR/" + &line[cut_end + 1..];
                    }
                }
                line.trim_end().replace(&test.crate_name(), "$CRATE")
            })
            .join("\n")
    }

    fn matches(
        &self,
        project: &tb::Project,
        test: &tb::Test,
        expected: &str,
        actual: &str,
    ) -> bool {
        let actual = self.normalize(project, test, actual);
        expected == actual
    }
}

enum TestResult {
    Ok,
    Failed(Option<Output>),
}

fn run_make_test(test: &Path, source_root: &Path, opts: &Opts) -> TestResult {
    let tmpdir = tempfile::tempdir().expect("Could not create temporary directory");

    let mut cmd = Command::new("make");
    cmd.current_dir(test).env("TMPDIR", tmpdir.path()).env(
        "PROC_MACRO_DIR",
        format!(
            "'{}'",
            source_root
                .join("target")
                .join("debug")
                .canonicalize()
                .unwrap()
                .to_str()
                .unwrap()
                // Remove UNC path on windows, which rustc can't handle in --extern.
                .trim_start_matches(r#"\\?\"#)
        ),
    );

    let compiler = {
        let mut cfg = cc::Build::new();
        cfg.cargo_metadata(false).opt_level(0).cpp(true);
        let triple = if cfg!(unix) {
            Some(format!("{}-unknown-linux-gnu", env::consts::ARCH))
        } else if cfg!(windows) {
            Some(format!("{}-pc-windows-msvc", env::consts::ARCH))
        } else {
            None
        };
        if let Some(triple) = triple {
            cfg.target(&triple).host(&triple);
        }
        cfg.get_compiler()
    };
    if cfg!(windows) {
        let lib_tool = compiler.path().parent().unwrap().join("lib.exe");
        cmd.env(
            "CXX",
            &format!(
                "'{}' {}",
                compiler.path().to_str().unwrap(),
                itertools::join(compiler.args().iter().map(|x| x.to_str().unwrap()), " ")
            ),
        )
        .envs(compiler.env().iter().cloned())
        .env("IS_WINDOWS", "1")
        .env("IS_MSVC", "1")
        .env("LD_LIB_PATH_ENVVAR", "PATH")
        .env(
            "MSVC_LIB",
            &format!("'{}' -nologo", lib_tool.to_str().unwrap()),
        );
    } else {
        cmd.env("CXX", compiler.path())
            .env("LD_LIB_PATH_ENVVAR", "LD_LIBRARY_PATH");
    }

    let (status, output) = if opts.nocapture {
        let status = cmd.spawn().expect("failed to run test").wait().unwrap();
        (status, None)
    } else {
        let output = cmd.output().expect("failed to run test");
        (output.status, Some(output))
    };
    if opts.noclean {
        std::mem::forget(tmpdir);
    }
    if status.success() {
        TestResult::Ok
    } else {
        TestResult::Failed(output)
    }
}
