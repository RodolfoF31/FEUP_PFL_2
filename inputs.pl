read_number(UpperBound, Result) :-
	repeat,
		format( '| Choose an Option (~d-~d) - ', [0, UpperBound]),
		get_code(ASCIICode),
		peek_char(Enter),
		Enter == '\n',
		ascii_code(ASCIICode, Result),
		skip_line,
		UP is UpperBound +1,
		(between(Result,UP) -> true;
		write('Not a valid number, try again\n')),
		between(Result,UP), !.