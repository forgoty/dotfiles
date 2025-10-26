---
description: Generate a detailed implementation plan for a feature (no code execution)
argument-hint: <feature-requirements-file> <output-plan-file>
---

# Feature Implementation Planning Mode

## Context

<feature_requirements>
@$1
</feature_requirements>

## Your Task

You are operating in **PLAN MODE ONLY**. Do not write, modify, or apply any code to the project. Your sole objective is to create a comprehensive implementation plan.

## Plan Structure

IMPORTANT: Create a detailed implementation plan in @$2 that includes:

### 1. Overview
- Feature summary
- Goals and success criteria
- Assumptions and constraints

### 2. Technical Analysis
- Current codebase patterns to follow
- Relevant MCP servers or tools to leverage
- Dependencies and prerequisites
- Potential risks or challenges

### 3. File Organization
- New files to create (with paths and purposes)
- Existing files to modify (with specific sections)
- Naming conventions to follow

### 4. Implementation Sequence
Break down into logical phases:
- Phase 1: [Initial setup/scaffolding]
- Phase 2: [Core functionality]
- Phase 3: [Integration points]
- Phase 4: [Testing and validation]

For each phase, specify:
- Files involved
- Key changes or additions
- Order of operations
- Validation checkpoints

### 5. Testing Strategy

IMPORTANT: Strictly follow the @$1 testing rules if any.

If none, propose a testing strategy including:
- Unit tests needed
- Integration tests needed
- Manual testing steps

### 6. Documentation Updates
- README changes
- API documentation
- Code comments strategy

### 7. Implementation Checklist
- [ ] Actionable task items in order
- [ ] Each with clear acceptance criteria

## Output Requirements

- Use clear markdown formatting with headers
- Include code structure examples (not actual implementation)
- Reference specific file paths from the project
- Make the plan actionable for another developer
- Save the complete plan to @$2

## Constraints

**DO NOT**:
- Write actual implementation code
- Modify any project files
- Execute any bash commands
- Apply changes to the codebase
- Include emojis into the plan

**DO**:
- IMPORTANT: Highly rely on MCP servers for code navigation and documentation
- Be short and precise without losing necessary detail
- Analyze existing code patterns via @file references
- Propose file structures and architectures
- Suggest specific approaches and patterns
- Create a step-by-step actionable plan
