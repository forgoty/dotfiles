---
name: ddd-aggregate-design
description: Design rich domain aggregates with proper encapsulation, domain events, and the Export pattern. Use when implementing DDD aggregates with event sourcing patterns.
---

# Rich Domain Aggregate Design

Design and implement rich domain aggregates with proper encapsulation, domain events, and the Export pattern.

## When to Use This Skill

- Designing new domain aggregates from scratch
- Refactoring anemic models to rich domain models
- Adding domain events to existing aggregates
- Reviewing aggregate design for encapsulation

## Core Rules

Read [DDD-RULES.md](DDD-RULES.md) for all aggregate design rules. Key principles:

1. **Hide all fields** - All struct fields must be private
2. **Actionable methods only** - Expose domain operations, not getters
3. **Domain Events** - Track state changes through events
4. **Export pattern** - Use `Export()` to expose state externally
5. **Factory functions** - Create aggregates only through validated factories

## Quick Reference (Go Examples)

### Factory Function Pattern

```go
func NewOrder(customerID CustomerID, items []OrderItem) (*Order, error) {
    if len(items) == 0 {
        return nil, ErrEmptyOrder
    }
    o := &Order{
        id:     NewOrderID(),
        status: OrderStatusPending,
        events: &eventRegister{},
    }
    o.addEvent(applayer.NewEvent(OrderCreated{OrderID: o.id}))
    return o, nil
}
```

### Actionable Method Pattern

```go
func (o *Order) Ship(warehouse WarehouseID) error {
    if o.status == OrderStatusCancelled {
        return ErrOrderAlreadyCancelled
    }
    if o.status == OrderStatusShipped {
        return nil // Idempotent
    }
    oldStatus := o.status
    o.status = OrderStatusShipped
    o.addEvent(applayer.NewEvent(OrderShipped{OldStatus: oldStatus}))
    return nil
}
```

### Export Pattern

```go
func (o *Order) Export() ExportedOrder {
    return ExportedOrder{
        ID:     o.id.String(),
        Status: string(o.status),
    }
}
func (o *Order) ID() OrderID { return o.id } // Only identity getter allowed
```
