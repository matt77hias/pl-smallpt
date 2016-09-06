:- ensure_loaded(vector3), ensure_loaded(rng).

reflectance0(N1, N2, R) :-
	SR is (N1 - N2) / (N1 + N2),
	R is SR * SR.

schlick_reflectance(N1, N2, C, R) :-
	reflectance0(N1, N2, R0),
	R is R0 + (1.0 - R0) * C * C * C * C * C.

ideal_specular_reflect(D, N, R) :-
	dot_v3(N, D, T1), 
	T2 is 2.0 * T1, 
	mul_v3(T2, N, T3),
	sub_v3(D, T3, R).
	
ideal_specular_transmit(D, N, Nout, Nin, R, Pr)	:-
	ideal_specular_reflect(D, N, D_Re),
	dot_v3(N, D, ND),
	(
		ND @< 0
	->
		(Out_to_In = true, NL = N, NN is Nout / Nin)
	;
		(Out_to_In = false, minus_v3(N, NL), NN is Nin / Nout)
	),
	dot_v3(D, NL, CosTheta),
    Cos2Phi is 1.0 - NN * NN * (1.0 - CosTheta * CosTheta),
	(
		Cos2Phi @< 0
	->
		(R = D_Re, Pr is 1.0)
	;
		(
		mul_v3(NN, D, A1),
		A2 is NN * CosTheta + sqrt(Cos2Phi),
		mul_v3(NL, A2, A3),
		sub_v3(A1, A3, A4),
		normalize_v3(A4, D_Tr),
		(
			Out_to_In
		->
			C is 1.0 + CosTheta 
		;
			(dot_v3(D_Tr, N, DTrN), C is 1.0 - DTrN)
		),
		schlick_reflectance(Nout, Nin, C, Re),
		Pr_Re is 0.25 + 0.5 * Re,
		uniform_float(Random),
		(
			Random @< Pr_Re
		->
			(R = D_Re, Pr is Re / Pr_Re)
		;
			(Tr is 1.0 - Re, Pr_Tr is 1.0 - Pr_Re, R = D_Tr, Pr is Tr / Pr_Tr)
		)
		)
	).