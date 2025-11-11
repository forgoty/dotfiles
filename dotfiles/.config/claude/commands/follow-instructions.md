---
description: Follow the instructions in the requirements file and produce the requested output.
argument-hint: <instruction-file> <additional-context> <additional-rules>
---

# Follow Instructions Mode

Strictly follow the instructions provide in $1.
<additional-context>
$2
</additional-context>

# Rules

- IMPORTANT: Highly rely on mcp servers provided.
- IMPORTANT: Use subagents where possible.
- IMPORTANT: Parallelize work with subagents when possible.
- IMPORTANT: Determine a task type and use appropriate skills for the task.

<additional-rules>
$3
</additional-rules>

