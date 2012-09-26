-- Import the showval

type Ident a = a
unitIdent a = a
a `bindIdent` k = k a
show Ident a = Prelude.show a
