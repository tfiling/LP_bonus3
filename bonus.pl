:- module('bonus', [ crtRepresentationToDecimal/2, solve/2 ]).
:- use_module('./bee/bApplications/auxs/auxRunExpr',[runExpr/5, runExprMax/5, runExprMin/5, decodeInt/2, decodeIntArray/2]).
:- use_module('./bee/bApplications/auxs/auxMatrix',[matrixCreate/3, matrixGetCell/4, matrixTranspose/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% part 1: verify
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify(N, DigitList) :-
    length(DigitList, Len),
    0 is mod(Len, 3),
    N is div(Len, 3),
    validateDigitAppearances(N, DigitList), % validate digits appearances
    parseDigitList(DigitList, NumeratorsList, DenominatorsList),    % parse digit list into numerators and denominators of the equation's fractions
    length(SummedTopNumerator, N),
    findSummedTopNumerator(1, NumeratorsList, DenominatorsList, SummedTopNumerator),    
    crtSumAll(SummedTopNumerator, AdditionResultNumerator), % the equation is molded into one fraction, calculate this fraction's numerator
    crtMultiplyAll(DenominatorsList, AdditionResultDenominator),    % the equation is molded into one fraction, calculate this fraction's denominator
    AdditionResultDenominator == AdditionResultNumerator.   % this is a valid solution if the one big fraction's numerator is equal to its denominator, which means the equation (equals to 1) is true

% validateDigitAppearances(N+, Instance+) 
% satisfies if and only if the appearances of each digit in Instance doesn't exceed the allowed ceiling(3*N / 9)
validateDigitAppearances(N, Instance) :-
    N3 is N * 3,
    MaxAppearances is ceiling(N3 / 9),
    findall(X, between(1, 9, X), Digits),
    validateMaxAppearancesPerDigit(Instance, MaxAppearances, Digits).

% validate appearances per digit
validateMaxAppearancesPerDigit(_, _, []).
validateMaxAppearancesPerDigit(Instance, MaxAppearances, [Digit | RestDigits]) :-
    count(Instance, Digit, Appearances),
    Appearances =< MaxAppearances,
    validateMaxAppearancesPerDigit(Instance, MaxAppearances, RestDigits).

% count(Digits+, SpecificDigit+, Count-) - returns via Count how many times SpecificDigit appears in Digits
count([],_,0).
count([Element|T],Element,Y):- 
    count(T,Element,Z), 
    Y is 1+Z.
count([Other|T],Element,Z):- 
    Other \= Element,
    count(T,Element,Z).

% returns the crt representations of the denominator and numerators in the list
% for each three digits, composes the numerator and denominator of the current fraction 
% and converts them to the Chinese Remainder Theorem representation
parseDigitList([], [], []).
parseDigitList([ Xi, Yi, Zi | Rest], [CurrentCrtNumerator | RestNumerators], [CurrentCrtDenominator | RestDenominators]) :-
    DecimalDenominator is Yi * 10 + Zi,
    crtRepresentation(DecimalDenominator, CurrentCrtDenominator),
    crtRepresentation(Xi, CurrentCrtNumerator),
    parseDigitList(Rest, RestNumerators, RestDenominators).

% SummedTopNumerator = a*ef*hi + d*bc*hi + g*bc*ef (for N = 3)
% findSummedTopNumerator(I+, NumeratorsList+, DenominatorsList+, SummedTopNumerator-)
% the equation is molded into one fraction, calculate this fraction's nominator and return it with SummedTopNumerator
findSummedTopNumerator(I, _, _, SummedTopNumerator) :-
    length(SummedTopNumerator, Len),
    I > Len.

findSummedTopNumerator(I, NumeratorsList, DenominatorsList, SummedTopNumerator) :-
    length(SummedTopNumerator, Len),
    I =< Len,
    calculateIthTopSumComponent(I, NumeratorsList, DenominatorsList, Component),
    nth1(I, SummedTopNumerator, Component),
    I1 is I + 1,
    findSummedTopNumerator(I1, NumeratorsList, DenominatorsList, SummedTopNumerator).


calculateIthTopSumComponent(I, NumeratorsList, DenominatorsList, Component) :-
    nth1(I, NumeratorsList, NumeratorI),
    nth1(I, DenominatorsList, DenominatorI),
    select(DenominatorI, DenominatorsList, DenominatorsForMultiplication),
    crtMultiplyAll([NumeratorI | DenominatorsForMultiplication], Component).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% crt representation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the base used for calculating the remainders representing a number according  the Chinese Remainder Theorem
base([2, 3, 5, 7, 11, 13, 17]).

% crtRepresentation(Num+, Repr-) - returns with Repr the representation of Num in remainders
crtRepresentation(Num, Repr) :-
    base(Base),
    crtRepresentation(Num, Base, Repr-[]).


crtRepresentation(_, [], Tail-Tail).
crtRepresentation(Num, [HBase | RestBase], [ X | RestRepr]-Tail) :-
    X is mod(Num, HBase),
    crtRepresentation(Num, RestBase, RestRepr-Tail).

% crtAddition(Num1+, Num2+, Result-) - adds two numbers represented by remainders
crtAddition(Num1, Num2, Result) :-
    base(Base),
    crtAddition(Base, Num1, Num2, Result-[]).

crtAddition([], [], [], Tail-Tail).
crtAddition([HBase | RestBase], [H1 | Rest1], [H2 | Rest2], [Res | RestRes]-Tail) :-
    Sum is H1 + H2,
    Res is mod(Sum, HBase),
    crtAddition(RestBase, Rest1, Rest2, RestRes-Tail).


% crtSumAll(NumList+, Result-) - aggregative add all numbers in NumList into the sum Result
crtSumAll([Result], Result).
crtSumAll([X1, X2 | Rest], Result) :-
    crtAddition(X1, X2, Res),
    crtSumAll([Res | Rest], Result).

% crtMultiplication(Num1+, Num2+, Result-) - multiply two numbers
crtMultiplication(Num1, Num2, Result) :-
    base(Base),
    crtMultiplication(Base, Num1, Num2, Result-[]).

crtMultiplication([], [], [], Tail-Tail).
crtMultiplication([HBase | RestBase], [H1 | Rest1], [H2 | Rest2], [Res | RestRes]-Tail) :-
    Mul is H1 * H2,
    Res is mod(Mul, HBase),
    crtMultiplication(RestBase, Rest1, Rest2, RestRes-Tail).

% crtMultiplyAll(NumList+, Result-) - aggregative multiply all numbers in NumList into Result
crtMultiplyAll([Result], Result).
crtMultiplyAll([X1, X2 | Rest], Result) :-
    crtMultiplication(X1, X2, Res),
    crtMultiplyAll([Res | Rest], Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEE compatible crt 
%% similar API to the above one only that this one can get unknown variables and returns BEE constraints
%% that can be satisfied only when the result is as expected from the arithmetic expression


% beeCrtAddition(Num1+, Num2+, Result-, Cs-) - sums Num1 and Num2 into Result
beeCrtAddition(Num1, Num2, Result, Cs-Tail) :-
    base(Base),
    beeCrtAddition(Base, Num1, Num2, Result-[], Cs-Tail).

% beeCrtSumAll(Args+, Result-, Cs-) - sums all of the numbers in args into Result
beeCrtSumAll(Args, Result, Cs-Tail) :-
    matrixTranspose(Args, ArgsT),
    base(Base),
    beeCrtSumAll(ArgsT, Result, Base, Cs-Tail).


beeCrtSumAll([], [], [], Tail-Tail).
beeCrtSumAll([H | T], [HR | TR], [HB | TB], Cs-Tail) :-
    Cs = [
        new_int(HR, 0, HB), 
        int_array_sum_modK(H, HB, HR) | Cs2
    ],
    beeCrtSumAll(T, TR, TB, Cs2-Tail).

% beeCrtMultiplication(Num1+, Num2+, Result-, Cs-) - multiply Num1 and Num2 into Result
beeCrtMultiplication(Num1, Num2, Result, Cs-Tail) :-
    base(Base),
    beeCrtMultiplication(Base, Num1, Num2, Result, Cs-Tail).

% beeCrtMultiplyAll(Args+, Result+, Cs-) - aggregative multiply all numbers in Args into Result
beeCrtMultiplyAll(Args, Result, Cs-Tail) :-
    matrixTranspose(Args, ArgsT),
    base(Base),
    tmpMul(ArgsT, Result, Base, Cs-Tail).


tmpMul([], [], [], Tail-Tail).
tmpMul([H | T], [HR | TR], [HB | TB], Cs-Tail) :-
    Cs = [
        new_int(Product, 0, 1000), % TODO gal find suitable number
        int_array_times(H, Product),
        new_int(HR, 0, HB),
        int_mod(Product, HB, HR) | Cs2
    ],
    tmpMul(T, TR, TB, Cs2-Tail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% convert crt representation into decimal representation utility
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% verify
crtToDecimalVerify(CrtNum, DecimalRepr) :-
    crtRepresentation(DecimalRepr, ExpectedCrt),
    ExpectedCrt == CrtNum.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode
crtToDecimalEncode(CrtNum, map(Decimal), [new_int(Decimal, 1, 1000) | Constraints]) :-%TODO gal find a suitable number
    base(Base),
    crtToDecimalEncode(CrtNum, Base, Decimal, Constraints).


crtToDecimalEncode([], [], _, []).
crtToDecimalEncode([ HCrtNum | RestCrtNum], [HBase | RestBase], Decimal, [ int_mod(Decimal, HBase, HCrtNum) | RestConstraints]) :-
    crtToDecimalEncode(RestCrtNum, RestBase, Decimal, RestConstraints).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode

crtToDecimalDecode(map(X), Decimal) :-
    decodeInt(X, Decimal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% crtRepresentationToDecimal

crtRepresentationToDecimal(CrtNum, DecimalRepr) :-
    runExpr(CrtNum,DecimalRepr,
        bonus:crtToDecimalEncode,
        bonus:crtToDecimalDecode,
        bonus:crtToDecimalVerify).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% part 2: solve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode

fractionEncode(fraction(N), map(SolutionDigits), Constraints) :-
    N2 is N * 2,
    length(SolutionDigits, N2),
    declareNumerators(SolutionDigits, Constraints-Cs2),
    declareDenominators(SolutionDigits, Cs2-Cs3),
    length(SummedTopNumerator, N),
    extractCrtSolutionDigits(SolutionDigits, CrtSolutionDigits),
    beeFindSummedTopNumerator(1, CrtSolutionDigits, SummedTopNumerator, Cs3-Cs4),
    beeCrtSumAll(SummedTopNumerator, AdditionResultNumerator, Cs4-Cs5),
    extractDenominators(CrtSolutionDigits, DenominatorsList),
    beeCrtMultiplyAll(DenominatorsList, AdditionResultDenominator, Cs5-Cs6),
    applyDigitAppearancesConstraints(SolutionDigits, N, Cs6-Cs7),
    Cs7 = [int_arrays_eq(AdditionResultDenominator,AdditionResultNumerator)].
    
% SummedTopNumerator = a*ef*hi + d*bc*hi + g*bc*ef (for N = 3)
beeFindSummedTopNumerator(I, _, SummedTopNumerator, Tail-Tail) :-
    length(SummedTopNumerator, Len),
    I > Len.

beeFindSummedTopNumerator(I, SolutionDigits, SummedTopNumerator, Cs-Tail) :-
    length(SummedTopNumerator, Len),% 2 * N
    I =< Len,
    beeCalculateIthTopSumComponent(I, SolutionDigits, Component, Cs-Cs2),
    nth1(I, SummedTopNumerator, Component),
    I1 is I + 1,
    beeFindSummedTopNumerator(I1, SolutionDigits, SummedTopNumerator, Cs2-Tail).


beeCalculateIthTopSumComponent(I, SolutionDigits, Component, Cs-Tail) :-
    NumeratorIndex is (I * 2) - 1,
    nth1(NumeratorIndex, SolutionDigits,  NumeratorI),
    extractDenominators(SolutionDigits, DenominatorsList),
    subtractIthElement(I, 1, DenominatorsList, DenominatorsForMultiplication),
    beeCrtMultiplyAll([NumeratorI | DenominatorsForMultiplication], Component, Cs-Tail).

extractDenominators([], []).
extractDenominators([_, Denominator | Rest], [Denominator | RestExtracted]) :-
    extractDenominators(Rest, RestExtracted).


subtractIthElement(_, _, [], []).
subtractIthElement(I, I, [_ | T], SubtractedList) :-
    I1 is I + 1,
    subtractIthElement(I, I1, T, SubtractedList).
subtractIthElement(I, J, [H | T], [H | SubT]) :-
    I \== J,
    J1 is J + 1,
    subtractIthElement(I, J1, T, SubT).

declareDenominators([], Tail-Tail).
declareDenominators([_, Num = CrtNum | Rest], [new_int(Num, 11, 99) | RestConstraints]-Tail) :-
% declareDenominators([_, Num = CrtNum | Rest], RestConstraints-Tail) :-
    base(Base),
    createCrtNum(CrtNum, Base, RestConstraints-Cs2), % create the variables of the representation
    setCrtCorrectnessConstraints(CrtNum, Base, Num, Cs2-Cs3), % force the correctness of the representation
    declareDenominators(Rest, Cs3-Tail).


declareNumerators([], Tail-Tail).
declareNumerators([Num = CrtNum, _ | Rest], [new_int(Num, 1, 9) | RestConstraints]-Tail) :-
% declareNumerators([Num = CrtNum, _ | Rest], RestConstraints-Tail) :-
    base(Base),
    createCrtNum(CrtNum, Base, RestConstraints-Cs2), % create the variables of the representation
    setCrtCorrectnessConstraints(CrtNum, Base, Num, Cs2-Cs3), % force the correctness of the representation
    declareNumerators(Rest, Cs3-Tail).

% only declares the CRT representation variables
createCrtNum([], [], Tail-Tail).
createCrtNum([HCrtNum | RestCrtNum], [HBase | RestBase], [new_int(HCrtNum, 0, HBase) | RestConstraints]-Tail) :-
    createCrtNum(RestCrtNum, RestBase, RestConstraints-Tail).

% create the mapping between the decimal and the contents of the crt
setCrtCorrectnessConstraints([], [], _, Tail-Tail).
setCrtCorrectnessConstraints([ HCrtNum | RestCrtNum], [HBase | RestBase], Decimal, [ int_mod(Decimal, HBase, HCrtNum) | RestConstraints]-Tail) :-
    setCrtCorrectnessConstraints(RestCrtNum, RestBase, Decimal, RestConstraints-Tail).

extractCrtSolutionDigits([], []).
extractCrtSolutionDigits([ _ = Crt | Rest], [Crt | RestCrt]) :-
    extractCrtSolutionDigits(Rest, RestCrt).

applyDigitAppearancesConstraints(SolutionDigits, N, Constraints-Tail) :-
    convertRepresentationToDirect(SolutionDigits, ConvertedSolutionDigits, Constraints-Cs2),
    matrixTranspose(ConvertedSolutionDigits, TransposedConvertedDigits),
    maxSumDirectRepresentationDigits(N, MaxSumValue),
    sumDirectRepresentationDigits(TransposedConvertedDigits, MaxSumValue, Cs2-Tail).

% A/BC fraction at a time
convertRepresentationToDirect([], [], Tail-Tail).
convertRepresentationToDirect([AInt = _, BC = _ | Rest], [A, B, C | RestConvertedSolutionDigits], Cs-Tail) :-
    Cs = [
        new_int_dual(ADirect, 1, 9),
        int_eq(ADirect, AInt),
        int_direct2bool_array(ADirect, A, 1),
        new_int_dual(BDirect, 1, 9), 
        int_div(BC, 10, BDirect), 
        int_direct2bool_array(BDirect, B, 1),
        new_int_dual(CDirect, 1, 9), 
        int_mod(BC, 10, CDirect),
        int_direct2bool_array(CDirect, C, 1) | Cs2
        ],
    convertRepresentationToDirect(Rest, RestConvertedSolutionDigits, Cs2-Tail).

sumDirectRepresentationDigits([], _, Tail-Tail).
sumDirectRepresentationDigits([H | T], [new_int(Sum, -1000, MaxSumValue), int_array_sum(H, Sum) | Rest]-Tail) :-%TODO gal find better numbers
    sumDirectRepresentationDigits(T, MaxSumValue, Rest-Tail).


maxSumDirectRepresentationDigits(N, MaxSumValue) :-
    N3 is N * 3,
    MaxAppearances is ceiling(N3 / 9),
    MaxSumValue is MaxAppearances - (N3 - MaxAppearances).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode

fractionDecode(map(SolutionDigits), Solution) :-
    length(SolutionDigits, N2),
    N is N2 / 2,
    N3 is N * 3,
    decodeSolutionDigits(SolutionDigits, SolutionDigits1),
    length(Solution, N3),
    parseSolutionDigits(SolutionDigits1, Solution, 1),
    writeln(Solution).


% the 3 digits represent the fraction A/BC
decodeSolutionDigits([], []).
decodeSolutionDigits([_ = CrtNumeratorNum, _ = CrtDenominatorNum | Rest], [A = CrtNumeratorNum, BC = CrtDenominatorNum | Rest1]) :-
    decodeCrt(CrtNumeratorNum, DecodedCrtNumeratorNum),
    decodeCrt(CrtDenominatorNum, DecodedCrtDenominatorNum),
    crtRepresentationToDecimal(DecodedCrtNumeratorNum, A),
    crtRepresentationToDecimal(DecodedCrtDenominatorNum, BC),
    decodeSolutionDigits(Rest, Rest1).


decodeCrt([], []).
decodeCrt([H | T], [DH | DT]) :-
    decodeInt(H, DH),
    decodeCrt(T, DT).

% SolutionDigits
parseSolutionDigits([], _, _).
parseSolutionDigits([A = _, BC = _ | Rest], Solution, I) :-
    I3 is I * 3,
    I3_2 is I3 - 2,
    I3_1 is I3 - 1,
    nth1(I3_2, Solution, A),
    B is floor(BC / 10),
    C is mod(BC, 10),
    nth1(I3_1, Solution, B),
    nth1(I3, Solution, C),
    I1 is I + 1,
    parseSolutionDigits(Rest, Solution, I1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% runSolveExperiment

solve(fraction(N), Solution) :-
    runExpr(fraction(N),Solution,
        bonus:fractionEncode,
        bonus:fractionDecode,
        bonus:verify).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :-
%     crtRepresentation(17, X1),
%     crtRepresentation(15, X2),
%     crtMultiplication(X1, X2, X),
%     writeln(X),
%     crtRepresentationToDecimal(X, Num),
%     writeln(Num).


% t:-
%     verify(3, [5, 3, 4, 7, 6, 8, 9, 1, 2]).
    % length(SummedTopNumerator, 3),
    % NumeratorsList = [[1,2,0,5,5],[1,1,2,0,7],[1,0,4,2,9]],
    % DenominatorsList = [[0,1,4,6,1],[0,2,3,5,2],[0,0,2,5,1]],
    % findSummedTopNumerator(1, NumeratorsList, DenominatorsList, SummedTopNumerator).