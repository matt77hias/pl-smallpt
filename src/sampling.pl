:- ensure_loaded(math_tools).

uniform_sample_on_hemisphere(U1, U2, [Rx, Ry, Rz]) :-
	SinTheta is sqrt(max(0.0, 1.0 - U1 * U1)),
	pi(Pi), Phi is 2.0 * Pi * U2,
	Rx is cos(Phi) * SinTheta, Ry is sin(Phi) * SinTheta, Rz is U1. 

cosine_weighted_sample_on_hemisphere(U1, U2, [Rx, Ry, Rz]) :-
	CosTheta is sqrt(1.0 - U1), SinTheta is sqrt(U1),
	pi(Pi), Phi is 2.0 * Pi * U2,
	Rx is cos(Phi) * SinTheta, Ry is sin(Phi) * SinTheta, Rz is CosTheta.