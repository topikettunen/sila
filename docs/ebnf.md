# EBNF

> **Note**: EBNF will be changed during the early phases of development, this
> is not final.

```
statement-node := "return" expression-node ";"
                | "if" expression-node statement-node ("else" statement-node)?
                | "for" "expression-statement-node expression-node? ";" expression-node? ")" statement-node
                | "loop" statement-node
                | "{" compound-statement-node
                | expression-statement-node

compound-statement-node := statement-node* "}"

expression-statement-node := expression-node? ";"

expression-node := assign

equality-node := relational-node ( "==" relational-node | "!=" relational-node )*

assign-node := equality ( "<-" assign )?

relational-node := add ( "<"  add | "<=" add | ">"  add | ">=" add )*

add-node := multiplicative-node ( "+" multiplicative-node | "-" multiplicative-node )*

multiplicative-node := unary-node ( "*" unary-node | "/" unary-node )*

unary-node := ( "+" | "-" ) unary | primary-node

primary-node := "(" expression-node ")" | ident | number

program := statement-node*
```
