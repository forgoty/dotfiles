---
allowed-tools: Write, Read, Edit, Grep, Bash(go test:*), Bash(gofumpt:*), Bash(golangci-lint run:*)
argument-hint: <description> [package-or-file]
description: Generate application/communication tests for command handlers
skills: golang,programming
---

Generate **APPLICATION/COMMUNICATION TESTS** for: "$1" in ${2:-.}

## Rules

- **[Go-T1-C]** One test function per execution path; table tests for variants of same path
- **[Go-T4-H]** Build tags: `//go:build unit`
- **[Go-T2-H]** Use `_test` package suffix for black-box testing
- Use `require.NoError` for success, `require.ErrorIs` for errors
- Mock ALL dependencies; always call `AssertExpectations(t)`
- Use `AssertNotCalled` to verify methods NOT invoked
- Reuse existing project mocks; add nil guard for pointer/slice returns
- **Naming**: `TestHandler_GivenCondition_WhenAction_ThenOutcome`

## Template

```go
//go:build unit

package handler_test

import (
    "context"
    "errors"
    "testing"

    "github.com/stretchr/testify/mock"
    "github.com/stretchr/testify/require"
)

func TestHandler(t *testing.T) {
    dep1 := &MockDep1{}
    dep1.On("Load", mock.Anything, mock.Anything).Return(validEntity, nil)
    dep2 := &MockDep2{}
    dep2.On("Save", mock.Anything, mock.Anything).Return(nil)

    handler := NewHandler(dep1, dep2)
    _, err := handler.Handle(context.Background(), Command{})

    r := require.New(t)
    r.NoError(err)
    dep1.AssertExpectations(t)
    dep2.AssertExpectations(t)
}

func TestHandler_GivenLoadFails_WhenHandled_ThenReturnsError(t *testing.T) {
    tests := []struct {
        name          string
        depError      error
        expectedError error
    }{
        {"given not found, when handled, then conflict", ErrNotFound, ErrConflict},
        {"given unknown error, when handled, then internal", errors.New("fail"), ErrInternal},
    }

    for _, tc := range tests {
        t.Run(tc.name, func(t *testing.T) {
            dep1 := &MockDep1{}
            dep1.On("Load", mock.Anything, mock.Anything).Return(nil, tc.depError)
            dep2 := &MockDep2{} // No expectations - fails if called

            handler := NewHandler(dep1, dep2)
            _, err := handler.Handle(context.Background(), Command{})

            require.ErrorIs(t, err, tc.expectedError)
        })
    }
}

func TestHandler_GivenNoEntities_WhenHandled_ThenSkipsProcessing(t *testing.T) {
    dep1 := &MockDep1{}
    dep1.On("Find", mock.Anything).Return([]Entity{}, nil)
    dep2 := &MockDep2{} // Should NOT be called

    handler := NewHandler(dep1, dep2)
    _, err := handler.Handle(context.Background(), Command{})

    r := require.New(t)
    r.NoError(err)
    dep1.AssertExpectations(t)
    dep2.AssertNotCalled(t, "Process")
}

// Mock with nil guard for pointer/slice returns
type MockDep1 struct{ mock.Mock }

var _ Dep1Interface = (*MockDep1)(nil)

func (m *MockDep1) Load(ctx context.Context, id string) (*Entity, error) {
    args := m.Called(ctx, id)
    if args.Get(0) == nil {
        return nil, args.Error(1)
    }
    return args.Get(0).(*Entity), args.Error(1)
}
```

## Mock Expectations

```go
dep.On("Method", mock.Anything).Return(result, nil)           // any args
dep.On("Method", mock.MatchedBy(func(e *Entity) bool {        // custom matcher
    return e.ID == "expected"
})).Return(nil)
dep.On("Method", mock.Anything).Return(nil).Once()            // exactly once
dep.On("Method", mock.Anything).Return(nil).Times(3)          // exactly N times
dep.AssertNumberOfCalls(t, "Method", 3)                       // verify count
```

## Anti-Patterns

```go
// ❌ Never pass nil deps     → ✅ Use mock without expectations
// ❌ Share mocks across cases → ✅ Fresh mock per subtest
// ❌ Skip AssertExpectations  → ✅ Always verify expectations
```

## Run

```bash
go test -tags=unit -v -race ./...
```
