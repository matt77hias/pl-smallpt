:- ensure_loaded(vector3).

epsilon_sphere(E) :- E is 1e-4.

intersect_sphere([Radius, Position, _E, _F, _Reflection_t], [Origin, Direction, Tmin, Tmax, _Depth], Hit, NTmax) :-
	sub_v3(Position, Origin, OP),
	dot_v3(Direction, OP, DOP),
	dot_v3(OP, OP, OPOP),
	Discriminant is DOP * DOP - OPOP + Radius * Radius,
	(
		Discriminant @< 0
	->
		(Hit = false, NTmax is Tmax)
	;
		(SDiscriminant is sqrt(Discriminant),
		Smin is DOP - SDiscriminant,
		(
			(Tmin @< Smin, Smin @< Tmax)
		->
			(Hit = true, NTmax is Smin)
		;
			(Smax is DOP + SDiscriminant,
			(
				(Tmin @< Smax, Smax @< Tmax)
			->
				(Hit = true, NTmax is Smax)
			;
				(Hit = false, NTmax is Tmax)
			))
		))
	).