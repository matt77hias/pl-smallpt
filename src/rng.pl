seed_rng() :-
	seed_rng(606418532).
seed_rng(Seed) :- 
	set_random(seed(Seed)).

uniform_float(Random) :- 
	random(Random).