:- ensure_loaded(math_tools).

x_v3([Vx, _Vy, _Vz], Vx).
y_v3([_Vx, Vy, _Vz], Vy).
z_v3([_Vx, _Vy, Vz], Vz).

get_v3([Vx, _, _], 0, Vx).
get_v3([_, Vy, _], 1, Vy).
get_v3([_, _, Vz], 2, Vz).

minus_v3([Vx, Vy, Vz], [Rx, Ry, Rz]) :-
	Rx is -Vx, Ry is -Vy, Rz is -Vz.
add_v3([V1x, V1y, V1z], [V2x, V2y, V2z], [Rx, Ry, Rz]) :-
	Rx is V1x + V2x, Ry is V1y + V2y, Rz is V1z + V2z.
add_v3([Vx, Vy, Vz], A, [Rx, Ry, Rz]) :-
	Rx is Vx + A, Ry is Vy + A, Rz is Vz + A.
add_v3(A, [Vx, Vy, Vz], [Rx, Ry, Rz]) :-
	Rx is A + Vx, Ry is A + Vy, Rz is A + Vz.
sub_v3([V1x, V1y, V1z], [V2x, V2y, V2z], [Rx, Ry, Rz]) :-
	Rx is V1x - V2x, Ry is V1y - V2y, Rz is V1z - V2z.
sub_v3([Vx, Vy, Vz], A, [Rx, Ry, Rz]) :-
	Rx is Vx - A, Ry is Vy - A, Rz is Vz - A.
sub_v3(A, [Vx, Vy, Vz], [Rx, Ry, Rz]) :-
	Rx is A - Vx, Ry is A - Vy, Rz is A - Vz.
mul_v3([V1x, V1y, V1z], [V2x, V2y, V2z], [Rx, Ry, Rz]) :-
	Rx is V1x * V2x, Ry is V1y * V2y, Rz is V1z * V2z.
mul_v3([Vx, Vy, Vz], A, [Rx, Ry, Rz]) :-
	Rx is Vx * A, Ry is Vy * A, Rz is Vz * A.
mul_v3(A, [Vx, Vy, Vz], [Rx, Ry, Rz]) :-
	Rx is A * Vx, Ry is A * Vy, Rz is A * Vz.	
div_v3([V1x, V1y, V1z], [V2x, V2y, V2z], [Rx, Ry, Rz]) :-
	Rx is V1x / V2x, Ry is V1y / V2y, Rz is V1z / V2z.
div_v3([Vx, Vy, Vz], A, [Rx, Ry, Rz]) :-
	IA is 1.0 / A,
	Rx is Vx * IA, Ry is Vy * IA, Rz is Vz * IA.
div_v3(A, [Vx, Vy, Vz], [Rx, Ry, Rz]) :-
	Rx is A / Vx, Ry is A / Vy, Rz is A / Vz.
	
dot_v3([V1x, V1y, V1z], [V2x, V2y, V2z], R) :-
	R is V1x * V2x + V1y * V2y + V1z * V2z.
cross_v3([V1x, V1y, V1z], [V2x, V2y, V2z], [Rx, Ry, Rz]) :-
	Rx is V1y * V2z - V1z * V2y, Ry is V1z * V2x - V1x * V2z, Rz is V1x * V2y - V1y * V2x.
	
eq_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) :-
	V1x =@= V2x, V1y =@= V2y, V1z =@= V2z.
ne_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) :-
	V1x \=@= V2x; V1y \=@= V2y; V1z \=@= V2z.
le_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) :-
	V1x @=< V2x, V1y @=< V2y, V1z @=< V2z.
lt_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) :-
	V1x @< V2x, V1y @< V2y, V1z @< V2z.
ge_v3v([V1x, V1y, V1z], [V2x, V2y, V2z]) :-
	V1x @>= V2x, V1y @>= V2y, V1z @>= V2z.
gt_v3([V1x, V1y, V1z], [V2x, V2y, V2z]) :-
	V1x @> V2x, V1y @> V2y, V1z @> V2z.
	
mind_v3([Vx, Vy, Vz], 0) :- 
	Vx @< Vy, Vx @< Vz.
mind_v3([Vx, Vy, Vz], 1) :- 
	not(min_v3([Vx, Vy, Vz], 0)), Vy @< Vz.
mind_v3([Vx, Vy, Vz], 2) :- 
	not(min_v3([Vx, Vy, Vz], 0)), not(min_v3([Vx, Vy, Vz], 1)).
maxd_v3([Vx, Vy, Vz], 0) :- 
	Vx @> Vy, Vx @> Vz.
maxd_v3([Vx, Vy, Vz], 1) :- 
	not(max_v3([Vx, Vy, Vz], 0)), Vy @> Vz.
maxd_v3([Vx, Vy, Vz], 2) :- 
	not(max_v3([Vx, Vy, Vz], 0)), not(max_v3([Vx, Vy, Vz], 1)).
min_v3([Vx, Vy, Vz], R) :- 
	T is min(Vx, Vy), R is min(T, Vz).
max_v3([Vx, Vy, Vz], R) :- 
	T is max(Vx, Vy), R is max(T, Vz).
	
sqrt_v3([Vx, Vy, Vz], [Rx, Ry, Rz]) :-
	Rx is sqrt(Vx), Ry is sqrt(Vy), Rz is sqrt(Vz).
pow_v3([Vx, Vy, Vz], E, [Rx, Ry, Rz]) :-
	Rx is Vx**E, Ry is Vy**E, Rz is Vz**E.
abs_v3([Vx, Vy, Vz], [Rx, Ry, Rz]) :-
	Rx is abs(Vx), Ry is abs(Vy), Rz is abs(Vz).
min_v3([V1x, V1y, V1z], [V2x, V2y, V2z], [Rx, Ry, Rz]) :-
	Rx is min(V1x, V2x), Ry is min(V1y, V2y), Rz is min(V1z, V2z).
max_v3([V1x, V1y, V1z], [V2x, V2y, V2z], [Rx, Ry, Rz]) :-
	Rx is max(V1x, V2x), Ry is max(V1y, V2y), Rz is max(V1z, V2z).
round_v3([Vx, Vy, Vz], [Rx, Ry, Rz]) :-
	Rx is round(Vx), Ry is round(Vy), Rz is round(Vz).
floor_v3([Vx, Vy, Vz], [Rx, Ry, Rz]) :-
	Rx is floor(Vx), Ry is floor(Vy), Rz is floor(Vz).
ceil_v3([Vx, Vy, Vz], [Rx, Ry, Rz]) :-
	Rx is ceiling(Vx), Ry is ceiling(Vy), Rz is ceiling(Vz).
trunc_v3([Vx, Vy, Vz], [Rx, Ry, Rz]) :-
	Rx is truncate(Vx), Ry is truncate(Vy), Rz is truncate(Vz).
clamp_v3([Vx, Vy, Vz], Low, High, [Rx, Ry, Rz]) :-
	clamp(Vx, Low, High, Rx), clamp(Vy, Low, High, Ry), clamp(Vz, Low, High, Rz).
lerp_v3(A, V1, V2, R) :-
	sub_v3(V2, V1, X), mul_v3(X, A, Y), add_v3(V1, Y, R).
permute_v3(V, X, Y, Z, [Rx, Ry, Rz]) :-
	get_v3(V, X, Rx), get_v3(V, Y, Ry), get_v3(V, Z, Rz).

norm2s_v3([Vx, Vy, Vz], R) :-
	R is Vx * Vx + Vy * Vy + Vz * Vz.
norm2_v3([Vx, Vy, Vz], R) :-
	norm2s_v3([Vx, Vy, Vz], SR),
	R is sqrt(SR).
normalize_v3([Vx, Vy, Vz], [Rx, Ry, Rz]) :-
	norm2_v3([Vx, Vy, Vz], A),
	IA is 1.0 / A,
	Rx is Vx * IA, Ry is Vy * IA, Rz is Vz * IA.
	
explicit_add_v3([V1x, V1y, V1z], [V2x, V2y, V2z], [Rx, Ry, Rz]) :-
	Rx is V1x + V2x, Ry is V1y + V2y, Rz is V1z + V2z.
	
list_add_v3(List, Index, V, NList) :- 
	Functor =.. [dummy|List],
    NIndex is Index + 1,
	nth0(Index, List, V0),
    explicit_add_v3(V0, V, R),
	setarg(NIndex, Functor, R),
    Functor =.. [dummy|NList].