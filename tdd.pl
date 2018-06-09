:- include('bonus.pl').

test1(1,
        3,
        [5, 3, 4, 7, 6, 8, 9, 1, 2],
        1
).

test1(2,
        3,
        [1, 3, 5, 7, 4, 6, 9, 8, 2],
        0
).

test1(3,
        6,
        [
            2, 1, 8, 
            3, 7, 6, 
            4, 1, 9, 
            5, 5, 4,
            8, 2, 7,
            9, 3, 6
        ],
        1
).

test1(4,
        6,
        [
            1, 9, 3,
            1, 8, 7,
            2, 6, 4,
            5, 8, 5,
            2, 7, 4,
            3, 9, 6
        ],
        0
).

testVerify :-
    test1(I, N, DigitList, ShouldPass),
    (ShouldPass is 1 -> 
        (verify(N, DigitList) -> writeln(I: ok);writeln(I: failed)) ;
        (verify(N, DigitList) -> writeln(I: failed); writeln(I: ok))
    ),
    fail.
testVerify.

:-
    testVerify.