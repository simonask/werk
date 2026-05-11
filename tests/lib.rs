use werk_runner::BuildStatus;
use werk_util::Annotated;

// This file intentionally left blank.
pub mod mock_io;

stringleton::enable!(werk_runner);

pub async fn plan_build_and_get_status(
    workspace: &werk_runner::Workspace,
    goal: &str,
) -> Result<BuildStatus, werk_runner::Error> {
    let mut planner = werk_planner::Planner::new(&workspace.manifest);
    let task_id = planner
        .add_goal_by_name(goal)
        .map_err(Annotated::into_inner)?;
    let task_graph = planner.plan(workspace).map_err(Annotated::into_inner)?;
    let runner = werk_runner::Runner::new(workspace);
    runner.run(task_graph).await.map_err(|e| e.error)?;
    runner.get_status(task_id).expect("not built")
}
