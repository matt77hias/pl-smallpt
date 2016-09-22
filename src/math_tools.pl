pi(Pi) :- 
	Pi is 3.14159265358979323846.

positive_infinity(Inf) :- 
	Inf is 1e20.

clamp(X, Low, _High, Low) :- X @< Low.
clamp(X, _Low, High, High) :- X @> High.
clamp(X, _Low, _High, X).
	
to_byte(X, Gamma, R) :-
	E is 1.0 / Gamma, 
	T is 255.0 * (X**E),
	clamp(T, 0.0, 255.0, R0),
	R is truncate(R0).