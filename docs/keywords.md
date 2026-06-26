### Leaf Keywords

```leaf
affine
as
break
classical
coisometry
const
continue
else
ensures
enum
false
fn
for
general
if
impl
in
isometry
let
linear
loop
match
mod
mut
pub
qif
qelse
qenum
qmatch
requires
return
scratch
self
selse
sif
smatch
struct
supports
then
true
unitary
uncompsafe
use
while
```

### Adjoint

Adjoint is used both as built-in function and as a block keyword operator:

```leaf
adjoint()
adjoint {
    // block of code
}
```

### Leaf Reserved Built-in Functions

Built-in Functions CANNOT be shadowed by a local declaration:

```leaf
adjoint
barrier
ctrl + on + apply
basis
clean
discard
isolated
measr
product
qalloc
reset
tensor
separable
stabilized
uncompute
weaken
```

### Global Language Constants

Globally reserved quantum state literals:

```leaf
one
minus
minusi
plus
plusi
zero
```

### Leaf Prelude Functions

Prelude functions CAN be shadowed by a local declaration:

```leaf
abs
acos
asin
atan
ceil
cos
exp
floor
ln
log2
log10
max
min
Param
phase
round
sin
sqrt
tan
turns
```
