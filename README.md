# Numis

This project aims to create a domain specific language for describing balance sheets.

It is heavily inspired by:

- [Borja Clavero's paper on the 4 primitive balance sheet operations](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4032398)
- [Various posts by Daniel H. Neilson on his "Soon Parted" substack](https://www.soonparted.co/)
- [Discussions about the nature of money at the mimbres school](https://www.patreon.com/MimbresSchool)


## Language Grammar

There are two types of statements: assertions and balance sheet operations

### Assertions

An assertion allows you to make a claim about the initial conditions for the balance sheet.

The grammar goes `<Name> <Operator> <Amount>`

- `Name`: a single word that will be used to identify this agent, they will be given a balance sheet with assets and liabilities
- `Operator`: Either `has` or `owes`
- `Amount`: A number

#### Has

The `has` operator lets you assign an asset

```
A has 100
```

yields 

| A Assets | A Liabilities | Description |
|----------|---------------|-------------|
| +100     |               |             |

#### Owes

The `owes` operator lets you assign a liability

```
A has 100
```

yields 

| A Assets | A Liabilities | Description |
|----------|---------------|-------------|
|          | +100          |             |

### Balance Sheet Operations

A balance sheet operation allows you to create a relationship between two agents.

The grammar goes `<Payer> <Operator> <Amount> to <Payee> [as "<Description>"]`

- `Payer` & `Payee`: a single word that will be used to identify this agent, they will be given a balance sheet with assets and liabilities
- `Operator`: Either `assigns`, `issues`, `sets-off`, `novates`
- `Amount`: A number
- `Description`: A textual description of the line item, the `[]` in the grammar indicate that this is optional

#### Assignment

The `assigns` operator lets you remove an asset from the payer and add an asset to the payee

```
A assigns 100 to B
```

yields 

| A Assets | A Liabilities | B Assets | B Liabilities | Description |
|----------|---------------|----------|---------------|-------------|
| -100     |               | +100     |               |             |

#### Issuance

The `issues` operator lets you add a liability to the payer and add an asset to the payee

```
A issues 100 to B
```

yields 

| A Assets | A Liabilities | B Assets | B Liabilities | Description |
|----------|---------------|----------|---------------|-------------|
|          | +100          | +100     |               |             |

#### Set off

The `sets-off` operator lets you remove an asset from the payer and remove a liability from the payee

```
A sets-off 100 to B
```

yields 

| A Assets | A Liabilities | B Assets | B Liabilities | Description |
|----------|---------------|----------|---------------|-------------|
| -100     |               |          | -100          |             |

#### Novation

The `novates` operator lets you add a liability to the payer and remove an asset from the payee

```
A novates 100 to B
```

yields 

| A Assets | A Liabilities | B Assets | B Liabilities | Description |
|----------|---------------|----------|---------------|-------------|
|          | +100          |          | -100          |             |

#### Descriptions

You can add a description by appending `as "<some text>"` to the end of a balance sheet operation

```
A assigns 100 to B as "a payment for goods and services"
```

yields

| A Assets | A Liabilities | B Assets | B Liabilities | Description                      |
|----------|---------------|----------|---------------|----------------------------------|
| -100     |               | +100     |               | a payment for goods and services |

### Comments

You can add single line comments with `--`

```
-- This is a comment
A assigns 100 to B as "a payment for goods and services"
```

## A Full Example

```
-- The initial obligation
Taker assigns 100 to Payer as "export of goods"
Payer issues 100 to Taker as "taker opens foreign account"

-- The bill of exchange transaction
Deliverer assigns 100 to Taker as "payment in currency"
Taker assigns 100 to Deliverer as "bill of exchange"

-- The bill of exchange transfer from Deliverer to Payee
Deliverer assigns 100 to Payee as "bill of exchange"
Payee issues 100 to Deliverer as "deliverer opens foreign account"

-- The Payee gives bill of exchange to Payer
Payer assigns 100 to Payee as "payment in currency"
Payee sets-off 100 to Payer as "extinguish bill of exchange"
```

yields

| Deliverer Assets | Deliverer Liabilities | Payee Assets | Payee Liabilities | Payer Assets | Payer Liabilities | Taker Assets | Taker Liabilities | Description                     |
|------------------|-----------------------|--------------|-------------------|--------------|-------------------|--------------|-------------------|---------------------------------|
|                  |                       |              |                   | +100         |                   | -100         |                   | export of goods                 |
|                  |                       |              |                   |              | +100              | +100         |                   | taker opens foreign account     |
| -100             |                       |              |                   |              |                   | +100         |                   | payment in currency             |
| +100             |                       |              |                   |              |                   | -100         |                   | bill of exchange                |
| -100             |                       | +100         |                   |              |                   |              |                   | bill of exchange                |
| +100             |                       |              | +100              |              |                   |              |                   | deliverer opens foreign account |
|                  |                       | +100         |                   | -100         |                   |              |                   | payment in currency             |
|                  |                       | -100         |                   |              | -100              |              |                   | extinguish bill of exchange     |
