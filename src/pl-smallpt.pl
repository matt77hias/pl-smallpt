:- 
	ensure_loaded(collections),
	ensure_loaded(math_tools), 
	ensure_loaded(vector3), 
	ensure_loaded(ray),
	ensure_loaded(rng), 
	ensure_loaded(sampling),
	ensure_loaded(image_io),
	ensure_loaded(sphere), 
	ensure_loaded(specular).
	
%:- 
%	set_prolog_stack(global, limit(1 000 000 000 000)).

% Scene
refractive_index_out(Nout) :-
	Nout is 1.0.
refractive_index_in(Nin) :-
	Nin is 1.5.
	
get_scene(Scene) :-
		Scene = 
		[
        [1e5,	[1.00001, 40.8, 81.6], 	[0.0, 0.0, 0.0],  	[0.75,0.25,0.25], 		diffuse],
	    [1e5,   [-99901, 40.8, 81.6], 	[0.0, 0.0, 0.0], 	[0.25,0.25,0.75], 		diffuse],
	    [1e5,   [50.0, 40.8, 1e5],      [0.0, 0.0, 0.0], 	[0.75, 0.75, 0.75], 	diffuse],
	    [1e5,   [50.0, 40.8, -99830],	[0.0, 0.0, 0.0],	[0.0, 0.0, 0.0], 		diffuse],
	    [1e5,   [50.0, 1e5, 81.6],      [0.0, 0.0, 0.0], 	[0.75, 0.75, 0.75], 	diffuse],
	    [1e5,   [50.0, -99918.4, 81.6], [0.0, 0.0, 0.0], 	[0.75, 0.75, 0.75], 	diffuse],
	    [16.5, 	[27.0, 16.5, 47.0],     [0.0, 0.0, 0.0], 	[0.999, 0.999, 0.999], 	specular],
	    [16.5,  [73.0, 16.5, 78.0],     [0.0, 0.0, 0.0], 	[0.999, 0.999, 0.999], 	refractive],
	    [600.0, [50.0, 681.33, 81.6],   [12.0, 12.0, 12.0],	[0.0, 0.0, 0.0],		diffuse]
		].

intersect_scene(Scene, Ray, Hit, Tmax, Sphere) :-
	intersect_scene(Scene, Ray, none, none, Hit, Tmax, Sphere).
intersect_scene([], _Ray, CTmax, CSphere, Hit, HTmax, HSphere) :-
	(
		CSphere \== none
	->
		(Hit = true, HTmax = CTmax, HSphere = CSphere)
	;
		(Hit = false, HTmax = none, HSphere = none)
	).
intersect_scene([Head|Tail], [Origin, Direction, Tmin, Tmax, Depth], CTmax, CSphere, Hit, HTmax, HSphere) :-
	intersect_sphere(Head, [Origin, Direction, Tmin, Tmax, Depth], SHit, Smax),
	(
		SHit
	->
		intersect_scene(Tail, [Origin, Direction, Tmin, Smax, Depth], Smax, Head, Hit, HTmax, HSphere)
	;
		intersect_scene(Tail, [Origin, Direction, Tmin, Tmax, Depth], CTmax, CSphere, Hit, HTmax, HSphere)
	).
	
radiance(Scene, [Origin, Direction, Tmin, Tmax, Depth], L) :-
	intersect_scene(Scene, [Origin, Direction, Tmin, Tmax, Depth], Hit, NTmax, [_Radius, Position, E, F, Reflection_t]),
	(
		Hit = false
	->
		L = [0.0, 0.0, 0.0]
	;
		(
		eval_ray([Origin, Direction, Tmin, Tmax, Depth], NTmax, P),
		sub_v3(P, Position, C1),
		normalize_v3(C1, N),
		(
			Depth @> 4
		->
			(
			max_v3(F, PrC),
			uniform_float(RC),
			(
				RC @>= PrC
			->
				(L = E, Quit = true)
			;
				(div_v3(F, PrC, NF0), Quit = false)
			)
			)
		;
			(NF0 = F, Quit = false)
		),
		(
			Quit
		->
			true
		;
		(
		(
			Reflection_t == refractive
		->
			(
			refractive_index_out(Nout),
			refractive_index_in(Nin),
			ideal_specular_transmit(Direction, N, Nout, Nin, D, Pr1),
			mul_v3(NF0, Pr1, NF)
			)
		;
			(
			NF = NF0,
			(
				Reflection_t == specular
			->
				ideal_specular_reflect(Direction, N, D)
			;
				(
					dot_v3(N, Direction, C2),
					(
						C2 @< 0
					->
						W = N
					;
						minus_v3(N, W)
					),
					x_v3(W, Wx),
					(
						abs(Wx) @> 0.1
					->
						U1 = [0.0, 1.0, 0.0]
					;
						U1 = [1.0, 0.0, 0.0]
					),
					cross_v3(U1, W, U2),
					normalize_v3(U2, U),
					cross_v3(W, U, V),
					uniform_float(R1),
					uniform_float(R2),
					cosine_weighted_sample_on_hemisphere(R1, R2, [Dx, Dy, Dz]),
					mul_v3(Dx, U, D0),
					mul_v3(Dy, V, D1),
					mul_v3(Dz, W, D2),
					add_v3(D0, D1, D3),
					add_v3(D2, D3, D4),
					normalize_v3(D4, D)
				)
			)
			)
		),
		epsilon_sphere(Eps),
		positive_infinity(Inf),
		NDepth is Depth + 1,
		radiance(Scene, [P, D, Eps, Inf, NDepth], L0),
		mul_v3(NF, L0, L1),
		add_v3(E, L1, L)
		)
		)
		)
	).

main() :-
	Width is 2, %1024,
	Height is 2, %768,
	get_camera(Width, Height, Camera),
	get_scene(Scene),
	current_prolog_flag(argv, Argv),
	get_samples(Argv, Smax),
	loop_main(Scene, Camera, Width, Height, Smax, Ls),
	write_ppm(Width, Height, Ls).

get_samples([], 1).
get_samples(Argv, R) :-
	list_get(Argv, 0, Arg0),
	R is Arg0 / 4.

get_camera(Width, Height, [Eye, Cx, Cy, Gaze]) :-
	Eye = [50.0, 52.0, 295.6],
	C1 = [0.0, -0.042612, -1.0],
	normalize_v3(C1, Gaze),
    Fov is 0.5135,
	Temp is Width * Fov / Height,
    Cx = [Temp, 0.0, 0.0],
	cross_v3(Cx, Gaze, C2),
	normalize_v3(C2, C3),
	mul_v3(C3, Fov, Cy).	

loop_main(Scene, Camera, Height, Width, Smax, Ls) :-
	Size is Height * Width,
	list_fill(Size, [0.0, 0.0, 0.0], Ls0),
	loop_y(Scene, Camera, 0, Height, Width, Smax, Ls0, Ls).
loop_y(_Scene, _Camera, Height, Height, _Width, _Smax, Ls, Ls).
loop_y(Scene, Camera, Y, Height, Width, Smax, Ls0, Ls2) :- 
	write('\rRendering ('),
	F1 is Smax * 4,
	write(F1),
	write(' spp) '),
	F2 is 100.0 * Y / (Height - 1),
	sformat(F3, '~2f', F2),
	write(F3),
	write('%'),
	loop_x(Scene, Camera, Y, Height, 0, Width, Smax, Ls0, Ls1), 
	NY is Y + 1, 
	loop_y(Scene, Camera, NY, Height, Width, Smax, Ls1, Ls2).
loop_x(_Scene, _Camera, _Y, _Height, Width, Width, _Smax, Ls, Ls).
loop_x(Scene, Camera, Y, Height, X, Width, Smax, Ls0, Ls2) :-
	loop_sy(Scene, Camera, Y, Height, X, Width, 0, 2, Smax, Ls0, Ls1), 
	NX is X + 1,
	loop_x(Scene, Camera, Y, Height, NX, Width, Smax, Ls1, Ls2).
loop_sy(_Scene, _Camera, _Y, _Height, _X, _Width, 2, 2, _Smax, Ls, Ls).
loop_sy(Scene, Camera, Y, Height, X, Width, Sy, 2, Smax, Ls0, Ls2) :-
	loop_sx(Scene, Camera, Y, Height, X, Width, Sy, 0, 2, Smax, Ls0, Ls1),
	NSy is Sy + 1,
	loop_sy(Scene, Camera, Y, Height, X, Width, NSy, 2, Smax, Ls1, Ls2).
loop_sx(_Scene, _Camera, _Y, _Height, _X, _Width, _Sy, 2, 2, _Smax, Ls, Ls).
loop_sx(Scene, Camera, Y, Height, X, Width, Sy, Sx, 2, Smax, Ls0, Ls2) :-
	loop_s(Scene, Camera, Y, Height, X, Width, Sy, Sx, 0, Smax, [0.0, 0.0, 0.0], L0),
	clamp_v3(L0, 0.0, 1.0, L1),
	mul_v3(L1, 0.25, L2),
	Index is (Height - 1 - Y) * Width + X,
	list_add_v3(Ls0, Index, L2, Ls1),
	NSx is Sx + 1,
	loop_sx(Scene, Camera, Y, Height, X, Width, Sy, NSx, 2, Smax, Ls1, Ls2).
loop_s(_Scene, _Camera, _Y, _Height, _X, _Width, _Sy, _Sx, Smax, Smax, L, L).
loop_s(Scene, Camera, Y, Height, X, Width, Sy, Sx, S, Smax, L0, L2) :-
	do_s(Scene, Camera, Y, Height, X, Width, Sy, Sx, Smax, L0, L1),
	NS is S + 1,
	loop_s(Scene, Camera, Y, Height, X, Width, Sy, Sx, NS, Smax, L1, L2).
do_s(Scene, [Eye, Cx, Cy, Gaze], Y, Height, X, Width, Sy, Sx, Smax, L, NL) :-
	uniform_float(R1), 
	U1 is 2.0 * R1,
	(
		U1 @< 1
	->
		Dx is sqrt(U1) - 1.0
	;
		Dx is 1.0 - sqrt(2.0 - U1)
	),
	Coefx is (((Sx + 0.5 + Dx) / 2.0 + X) / Width - 0.5),
	uniform_float(R2),
	U2 is 2.0 * R2,
	(
		U2 @< 1
	->
		Dy is sqrt(U2) - 1.0
	;
		Dy is 1.0 - sqrt(2.0 - U2)
	),
	Coefy is (((Sy + 0.5 + Dy) / 2.0 + Y) / Height - 0.5),
	mul_v3(Cx, Coefx, C1),
	mul_v3(Cy, Coefy, C2),
	add_v3(C1, C2, C3),
	add_v3(C3, Gaze, Direction),
	normalize_v3(Direction, NDirection),
	mul_v3(Direction, 130.0, C4),
	add_v3(Eye, C4, NEye),
	epsilon_sphere(Eps),
	positive_infinity(Inf),
	radiance(Scene, [NEye, NDirection, Eps, Inf, 0], C5),
	mul_v3(C5, 1.0 / Smax, C6),
	add_v3(L, C6, NL).
