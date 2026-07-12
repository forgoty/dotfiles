---
allowed-tools: Write, Read, Edit, Grep, Bash(go test:*), Bash(gofumpt:*), Bash(golangci-lint run:*)
argument-hint: <description> [package-or-file]
description: Generate unit tests with mocks for isolated testing
skills: golang,programming
---

Generate **UNIT TESTS** for: "$1" in ${2:-.}

## Rules

- **[Go-T1-C]** Split into `_Success` and `_Error` functions (never mix)
- **[Go-T4-H]** Build tags: `//go:build unit`
- Use `require.NoError` for success, `require.Error` for errors
- Table-driven tests with testify, mock all dependencies

## Template

```go
//go:build unit

package mypackage_test

import (
    "testing"

    "github.com/stretchr/testify/assert"
    "github.com/stretchr/testify/mock"
    "github.com/stretchr/testify/require"
)

func TestFunctionName_Success(t *testing.T) {
    tests := []struct {
        name      string
        mockSetup func(*MockDep) // omit if no dependencies
        input     InputType
        want      OutputType
    }{
        {
            name:      "valid input returns expected output",
            mockSetup: func(m *MockDep) { m.On("Method", mock.Anything).Return(value, nil) },
            input:     validInput,
            want:      expectedOutput,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            // Arrange (omit mock setup if no dependencies)
            mockDep := new(MockDep)
            tt.mockSetup(mockDep)
            svc := NewService(mockDep)

            // Act
            got, err := svc.Method(tt.input)

            // Assert
            require.NoError(t, err)
            assert.Equal(t, tt.want, got)
            mockDep.AssertExpectations(t)
        })
    }
}

func TestFunctionName_Error(t *testing.T) {
    tests := []struct {
        name      string
        mockSetup func(*MockDep)
        input     InputType
    }{
        {
            name:      "returns error on invalid input",
            mockSetup: func(m *MockDep) { m.On("Method", mock.Anything).Return(nil, ErrFailed) },
            input:     invalidInput,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            mockDep := new(MockDep)
            tt.mockSetup(mockDep)
            svc := NewService(mockDep)

            _, err := svc.Method(tt.input)

            require.Error(t, err)
        })
    }
}

// Mock pattern
type MockDep struct{ mock.Mock }

func (m *MockDep) Method(ctx context.Context, id string) (*Entity, error) {
    args := m.Called(ctx, id)
    return args.Get(0).(*Entity), args.Error(1)
}
```

## Run

```bash
go test -tags=unit -v ./...
```
