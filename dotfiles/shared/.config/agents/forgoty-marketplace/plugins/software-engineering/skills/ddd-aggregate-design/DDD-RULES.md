# DDD Aggregate Design Rules

## Usage Guide

- Rules have severity: [C]ritical, [H]igh, [M]edium, [L]ow
- Rules have ID: [DDD-{category}{position}-{severity}]
- When rules conflict: Higher severity wins
- Examples use Go syntax but rules apply to any language

## Encapsulation [E]

- **[DDD-E1-C]** All aggregate fields MUST be private (not directly accessible from outside)
- **[DDD-E2-C]** NO getter methods except `ID()` for identity
- **[DDD-E3-H]** State access ONLY via `Export()` method returning DTO with primitives/strings

## Factory Functions [F]

- **[DDD-F1-C]** Aggregates MUST be created via factory functions/methods
- **[DDD-F2-H]** Factory validates all invariants before creation
- **[DDD-F3-H]** Factory emits creation event (e.g., `OrderCreated`)

## Actionable Methods [M]

- **[DDD-M1-C]** Methods represent domain operations, NOT data access
- **[DDD-M2-H]** Method pattern: guards → idempotency check → capture old state → mutate → emit event
- **[DDD-M3-H]** Return success (not error) when operation is idempotent and already done
- **[DDD-M4-M]** Method names use ubiquitous language: `Ship()`, `Cancel()`, `Approve()`

## Domain Events [V]

- **[DDD-V1-C]** Every state change MUST emit a domain event
- **[DDD-V2-H]** Events include aggregate ID and relevant state changes
- **[DDD-V3-H]** Include old/new values for state transitions (e.g., `OldStatus`)
- **[DDD-V4-M]** Event names are past tense: `OrderShipped`, `PaymentReceived`

## Domain Errors [R]

- **[DDD-R1-H]** Errors represent business rule violations, NOT validation failures
- **[DDD-R2-H]** Use descriptive named errors: `ErrOrderAlreadyCancelled`
- **[DDD-R3-M]** Error messages explain WHY operation is disallowed

## Testing [T]

- **[DDD-T1-H]** Place test factories in same package/module for private field access
- **[DDD-T2-H]** Test factories can directly set private fields for state setup
- **[DDD-T3-M]** Clear events register in test factories for cleaner assertions
