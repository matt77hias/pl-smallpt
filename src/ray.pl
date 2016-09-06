:- ensure_loaded(vector3).

eval_ray([Origin, Direction, _Tmin, _Tmax, _Depth], T, R) :-
	mul_v3(Direction, T, DT),
	add_v3(Origin, DT, R).