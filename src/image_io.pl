:- ensure_loaded(math_tools).

gamma(Gamma) :- Gamma is 2.2.

write_ppm(Width, Height, Ls) :-
	write_ppm(Width, Height, Ls, 'pl-image.ppm').
write_ppm(Width, Height, Ls, Fname) :- 
	open(Fname, write, Stream), 
    write_ppm_header(Stream, Width, Height),
	nl(Stream),
	write_ppm_Ls(Stream, Ls),
    close(Stream).
	
write_ppm_header(Stream, Width, Height) :-
	write(Stream, 'P3'), 
	nl(Stream), 
	write(Stream, Width), 
	write(Stream, ' '), 
	write(Stream, Height), 
	nl(Stream),
	write(Stream, '255').
	
write_ppm_Ls(Stream, [Head | []]) :-
	write_ppm_L(Stream, Head).
write_ppm_Ls(Stream, [Head | Tail]) :-
	write_ppm_L(Stream, Head),
	write(Stream, ' '),
	write_ppm_Ls(Stream, Tail).
write_ppm_Ls(_, []).
	
write_ppm_L(Stream, [Lx, Ly, Lz]) :-
	gamma(Gamma),
	to_byte(Lx, Gamma, Bx), 
	to_byte(Ly, Gamma, By), 
	to_byte(Lz, Gamma, Bz),
	write(Stream, Bx), 
	write(Stream, ' '), 
	write(Stream, By), 
	write(Stream, ' '), 
	write(Stream, Bz).