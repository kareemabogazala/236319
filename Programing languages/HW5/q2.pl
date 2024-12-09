pythagorean(A, B, C) :- 
    X is A * A,
    Y is B * B,
    Z is C * C,
    X + Y =:= Z.


gcd(X, X, X).
gcd(X, Y, D) :-
    X < Y,
    Y1 is Y - X,
    gcd(X, Y1, D).
gcd(X, Y, D) :-
    Y < X,
    gcd(Y, X, D).


prime_aux(X, X).


prime_aux(X, Y) :- 
    Y < X,
    gcd(X, Y, 1),  
    Y1 is Y + 1,
    prime_aux(X, Y1).

prime(2).
prime(X) :- 
    X > 2, 
    prime_aux(X, 2).
goldbach(2, 2, 4).
goldbach(X, Y, Z) :- 
    between(3, Z, X),  % Generate values for X starting from 3 up to Z
    between(3, Z, Y),  % Generate values for Y starting from 3 up to Z
    prime(X),
    prime(Y),
    X + Y =:= Z.
