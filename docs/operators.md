### Leaf Operators

```leaf
'('
')'
'['
']'
'{'
'}'

','
';'
':'
'::'
'.'
'->'

'='
':='
'+='
'-='
'*='
'/='
'%='

'+'
'-'
'*'
'/'
'%'

'=='
'!='
'!'
'>'
'>='
'<'
'<='
'=>'

'<<'
'<<='
'>>'
'>>='

'&&'
'||'
'&'
'|'
'^'
'&='
'|='
'^='

'..'
'..='

'#'
```

### Operator Precedence (Rust-style)

From the highest precedence to lowest (within expression operators used by Leaf):

1. `::` (path/qualified names)
2. Unary operators: `!`, unary `-`
3. `*`, `/`, `%`
4. `+`, `-`
5. `<<`, `>>`
6. `&`
7. `^`
8. `|`
9. `==`, `!=`, `<`, `<=`, `>`, `>=`
10. `&&`
11. `||`
12. `..`, `..=`
13. Assignment operators (right-associative): `=`, `:=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`

Notes:
- Parentheses `(...)` can always be used to make grouping explicit.