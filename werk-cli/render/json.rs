pub struct JsonWatcher;

impl JsonWatcher {
    pub fn new() -> Self {
        JsonWatcher
    }
}

impl werk_runner::Render for JsonWatcher {
    fn will_build(
        &self,
        task_id: werk_runner::TaskId,
        num_steps: usize,
        _outdatedness: &werk_runner::Outdatedness,
    ) {
        #[derive(serde::Serialize)]
        #[serde(tag = "type")]
        struct WillBuild<'a> {
            task: &'a str,
            num_steps: usize,
        }
        serde_json::to_writer(
            std::io::stdout(),
            &WillBuild {
                task: task_id.as_str(),
                num_steps,
            },
        )
        .unwrap();
        println!();
    }

    fn did_build(
        &self,
        task_id: werk_runner::TaskId,
        result: &Result<werk_runner::BuildStatus, werk_runner::Error>,
    ) {
        #[derive(serde::Serialize)]
        #[serde(tag = "type")]
        struct DidBuild<'a> {
            task: &'a str,
            result: Result<&'a str, String>,
        }
        serde_json::to_writer(
            std::io::stdout(),
            &DidBuild {
                task: task_id.as_str(),
                result: match result {
                    Ok(werk_runner::BuildStatus::Complete(..)) => Ok("rebuilt"),
                    Ok(werk_runner::BuildStatus::Exists(..)) => Ok("exists"),
                    Ok(werk_runner::BuildStatus::Ignore(..)) => Ok("ignored"),
                    Err(err) => Err(err.to_string()),
                },
            },
        )
        .unwrap();
        println!();
    }

    fn will_execute(
        &self,
        task_id: werk_runner::TaskId,
        command: &werk_runner::ShellCommandLine,
        step: usize,
        num_steps: usize,
    ) {
        #[derive(serde::Serialize)]
        #[serde(tag = "type")]
        struct WillExecute<'a> {
            task: &'a str,
            command: String,
            step: usize,
            num_steps: usize,
        }
        serde_json::to_writer(
            std::io::stdout(),
            &WillExecute {
                task: task_id.as_str(),
                command: command.to_string(),
                step,
                num_steps,
            },
        )
        .unwrap();
        println!();
    }

    fn did_execute(
        &self,
        task_id: werk_runner::TaskId,
        command: &werk_runner::ShellCommandLine,
        status: &std::io::Result<std::process::ExitStatus>,
        step: usize,
        num_steps: usize,
    ) {
        #[derive(serde::Serialize)]
        #[serde(tag = "type")]
        struct DidExecute<'a> {
            task: &'a str,
            command: String,
            status: Result<i32, String>,
            step: usize,
            num_steps: usize,
        }
        serde_json::to_writer(
            std::io::stdout(),
            &DidExecute {
                task: task_id.as_str(),
                command: command.to_string(),
                status: match status {
                    Ok(status) => {
                        if status.success() {
                            Ok(0)
                        } else {
                            Err(format!("exited with status: {}", status))
                        }
                    }
                    Err(err) => Err(err.to_string()),
                },
                step,
                num_steps,
            },
        )
        .unwrap();
        println!();
    }

    fn message(&self, task_id: Option<werk_runner::TaskId>, message: &str) {
        #[derive(serde::Serialize)]
        #[serde(tag = "type")]
        struct Warning<'a> {
            task: Option<&'a str>,
            message: &'a str,
        }
        serde_json::to_writer(
            std::io::stdout(),
            &Warning {
                task: task_id.map(|id| id.as_str()),
                message,
            },
        )
        .unwrap();
        println!();
    }

    fn warning(&self, task_id: Option<werk_runner::TaskId>, message: &werk_runner::Warning) {
        #[derive(serde::Serialize)]
        #[serde(tag = "type")]
        struct Warning<'a> {
            task: Option<&'a str>,
            message: String,
        }
        serde_json::to_writer(
            std::io::stdout(),
            &Warning {
                task: task_id.map(|id| id.as_str()),
                message: message.to_string(),
            },
        )
        .unwrap();
        println!();
    }
}
