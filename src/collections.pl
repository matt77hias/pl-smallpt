list_set(List, Index, Value, NList) :-
	Functor =.. [dummy|List],
    NIndex is Index + 1,
    setarg(NIndex, Functor, Value),
    Functor =.. [dummy|NList].

list_fill(0, _Value, []).
list_fill(Size, Value, [Value|Tail]) :-
	NSize is Size - 1,
	list_fill(NSize, Value, Tail).