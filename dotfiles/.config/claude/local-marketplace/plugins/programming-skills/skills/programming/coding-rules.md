# Coding Rules

## Usage Guide

- Rules have severity: [C]ritical, [H]igh, [M]edium, [L]ow
- Rules have ID in following format: [{category}{position}-{severity}]
- When rules conflict: Higher severity wins → Existing code patterns take precedence
- Process rules by severity (Critical first)

## Architecture & Structure [A]

- **[A1-C]** The existing code structure must not be changed without a strong reason.
- **[A2-C]** Every bug must be reproduced by a unit test before being fixed.
- **[A3-C]** Every new feature must be covered by a unit test before it is implemented.
- **[A4-M]** Minor inconsistencies and typos in the existing code may be fixed.
- **[A5-H]** All CI workflows must pass before code changes may be reviewed.
- **[A6-H]** Focus on domain and application logic using DDD tactical design

## Code Style & Patterns [S]

- **[S1-H]** Method and function bodies may not contain comments.
- **[S2-M]** Error and log messages should not end with a period.
- **[S3-M]** Error and log messages must always be a single sentence, with no periods inside.
- **[S4-H]** Favor "fail fast" paradigm over "fail safe": throw exception earlier.
- **[S5-H]** Classes must avoid using public static literals.
- **[S6-H]** CQS is a core principle on designing functions and methods

## Class Requirements [C]

- **[C1-C]** Every interface must have a supplementary documentation preceding it.
- **[C2-H]** A class docblock must explain the purpose of the class and provide usage examples.
- **[C3-C]** Implementation inheritance must be avoided at all costs (not to be confused with subtyping).
- **[C4-H]** Getters must be avoided, as they are symptoms of an anemic object model.
- **[C5-H]** Setters must be avoided, as they make objects mutable.
- **[C6-H]** Immutable objects must be favored over mutable ones.
- **[C7-H]** Every class may have only one primary constructor; any secondary constructor must delegate to it.
- **[C8-H]** Every class must encapsulate at least one attribute.
- **[C9-C]** Utility classes are strictly prohibited.
- **[C10-C]** Static methods in classes are strictly prohibited.
- **[C11-C]** All classes must be declared final, thus prohibiting inheritance.

## Method Requirements [M]

- **[M1-C]** Methods must never return `null`.
- **[M2-M]** Methods should avoid checking incoming arguments for validity.
- **[M3-C]** `null` may not be passed as an argument.
- **[M4-C]** Type introspection is strictly prohibited.
- **[M5-C]** Reflection on object internals is strictly prohibited.
- **[M6-H]** Exception messages must include as much context as possible.

## Documentation [D]

- **[D1-H]** The README.md file must explain the purpose of the repository.
- **[D2-H]** The README.md file must be free of typos, grammar mistakes, and broken English.
- **[D3-M]** The README.md file must be as short as possible and must not duplicate code documentation.
- **[D4-H]** Docblocks must be written in English only, using UTF-8 encoding.

## Testing Standards [T]

- **[T1-C]** Every test must test only public behavior
- **[T2-C]** Every domain logic change must be covered by a unit test to guarantee repeatability.
- **[T3-H]** Every test should follow "Given-When-Then" structure
- **[T4-M]** Test cases must be as short as possible.
- **[T5-H]** Every test must assert at least once.
- **[T6-M]** Each test file must have a one-to-one mapping with the feature file it tests.
- **[T7-H]** Every assertion must include a failure message that is a negatively toned claim about the error.
- **[T8-M]** Tests must use irregular inputs, such as non-ASCII strings.
- **[T9-H]** Tests may not share object attributes.
- **[T10-H]** Tests may not use static literals or other shared constants.
- **[T11-M]** Tests must be named as full English sentences describing the behavior of SUT (System under test).
- **[T12-H]** Tests may not test functionality irrelevant to their stated purpose.
- **[T13-H]** Tests must close resources they use, such as file handlers, sockets, and database connections.
- **[T14-H]** Objects must not provide functionality used only by tests.
- **[T15-M]** Tests may not assert on side effects such as logging output.
- **[T16-L]** Tests may not check the behavior of setters, getters, or constructors.
- **[T17-M]** Tests must not clean up after themselves; instead, they must prepare a clean state at the start.
- **[T18-M]** The best tests consist of a single statement.
- **[T19-H]** Each test must verify only one specific behavioral pattern of the SUT.
- **[T20-M]** Tests must use random values as inputs.
- **[T21-M]** Tests should store temporary files in temporary directories, not in the codebase directory.
- **[T22-H]** Tests are not allowed to print any log messages.
- **[T23-H]** The testing framework must be configured to disable logging from the objects under test.
- **[T24-H]** Tests must not wait indefinitely for any event; they must always stop waiting on a timeout.
- **[T25-H]** Tests must verify object behavior in multi-threaded, concurrent environments.
- **[T26-H]** Tests must retry potentially flaky code blocks.
- **[T27-H]** Tests must assume the absence of an Internet connection.
- **[T28-H]** Tests must not assert on error messages.
- **[T29-H]** Tests must not rely on default configurations of the objects they test, providing custom arguments.
- **[T30-H]** Tests must not mock the file system, sockets, or memory managers.
- **[T31-M]** Tests must use ephemeral TCP ports, generated using appropriate library functions.
- **[T32-M]** Tests should inline small fixtures instead of loading them from files.
- **[T33-M]** Tests should create large fixtures at runtime rather than store them in files.
- **[T34-M]** Tests may create supplementary fixture objects to avoid code duplication.
- **[T35-L]** Test method names must spell "cannot" and "dont" without apostrophes.
- **[T36-L]** Tests may not assert on returned code.
- **[T37-L]** Tests may not assert on error type.

## AI Code Generation Process [AI]

- **[AI1-H]** Analyze existing code patterns first
- **[AI2-H]** Write tests before implementation
- **[AI3-H]** Design interfaces before classes
- **[AI4-H]** Implement with immutability in mind
- **[AI5-H]** Error handling: validate early, use Optionals, throw specific exceptions
