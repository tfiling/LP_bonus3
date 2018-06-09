:- module('bonus', [ crtRepresentationToDecimal/2 ]).
:- use_module('./bee/bApplications/auxs/auxRunExpr',[runExpr/5, runExprMax/5, runExprMin/5, decodeInt/2, decodeIntArray/2]).
:- use_module('./bee/bApplications/auxs/auxMatrix',[matrixCreate/3, matrixGetCell/4]).

% TODO ask mike:
% on verify Im assuming the input a list of digits and just count the appearances
% which base should I use for accuracy with large numbers? it s

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% part 1: verify
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify(N, DigitList) :-
    length(DigitList, Len),
    0 is mod(Len, 3),
    N is div(Len, 3),
    validateDigitAppearances(N, DigitList),
    parseDigitList(DigitList, NumeratorsList, DenominatorsList),
    length(SummedTopNumerator, N),
    findSummedTopNumerator(1, NumeratorsList, DenominatorsList, SummedTopNumerator),
    crtSumAll(SummedTopNumerator, AdditionResultNumerator),
    crtMultiplyAll(DenominatorsList, AdditionResultDenominator),
    AdditionResultDenominator == AdditionResultNumerator.


validateDigitAppearances(N, Instance) :-
    N3 is N * 3,
    MaxAppearances is ceiling(N3 / 9),
    findall(X, between(1, 9, X), Digits),
    validateMaxAppearancesPerDigit(Instance, MaxAppearances, Digits).


validateMaxAppearancesPerDigit(_, _, []).
validateMaxAppearancesPerDigit(Instance, MaxAppearances, [Digit | RestDigits]) :-
    count(Instance, Digit, Appearances),
    Appearances =< MaxAppearances,
    validateMaxAppearancesPerDigit(Instance, MaxAppearances, RestDigits).

count([],_,0).
count([Element|T],Element,Y):- 
    count(T,Element,Z), 
    Y is 1+Z.
count([Other|T],Element,Z):- 
    Other \= Element,
    count(T,Element,Z).

% returns the crt representations of the denominator and numerators in the list
parseDigitList([], [], []).
parseDigitList([ Xi, Yi, Zi | Rest], [CurrentCrtNumerator | RestNumerators], [CurrentCrtDenominator | RestDenominators]) :-
    DecimalDenominator is Yi * 10 + Zi,
    crtRepresentation(DecimalDenominator, CurrentCrtDenominator),
    crtRepresentation(Xi, CurrentCrtNumerator),
    % append([Xi, Yi, Zi], Rest, DigitList),
    parseDigitList(Rest, RestNumerators, RestDenominators).

% SummedTopNumerator = a*ef*hi + d*bc*hi + g*bc*ef (for N = 3)
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

base([2, 3, 5, 7, 11, 13, 17, 19]).
% prime numbers
% 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199

crtRepresentation(Num, Repr) :-
    base(Base),
    crtRepresentation(Num, Base, Repr-[]).


crtRepresentation(_, [], Tail-Tail).
crtRepresentation(Num, [HBase | RestBase], [ X | RestRepr]-Tail) :-
    X is mod(Num, HBase),
    crtRepresentation(Num, RestBase, RestRepr-Tail).


crtAddition(Num1, Num2, Result) :-
    base(Base),
    crtAddition(Base, Num1, Num2, Result-[]).

crtAddition([], [], [], Tail-Tail).
crtAddition([HBase | RestBase], [H1 | Rest1], [H2 | Rest2], [Res | RestRes]-Tail) :-
    Sum is H1 + H2,
    Res is mod(Sum, HBase),
    crtAddition(RestBase, Rest1, Rest2, RestRes-Tail).


% crtSumAll(NumList+, Result-) - aggregative add all crt numbers in NumList into the sum Result
crtSumAll([Result], Result).
crtSumAll([X1, X2 | Rest], Result) :-
    crtAddition(X1, X2, Res),
    crtSumAll([Res | Rest], Result).


crtMultiplication(Num1, Num2, Result) :-
    base(Base),
    crtMultiplication(Base, Num1, Num2, Result-[]).

crtMultiplication([], [], [], Tail-Tail).
crtMultiplication([HBase | RestBase], [H1 | Rest1], [H2 | Rest2], [Res | RestRes]-Tail) :-
    Mul is H1 * H2,
    Res is mod(Mul, HBase),
    crtMultiplication(RestBase, Rest1, Rest2, RestRes-Tail).

% crtMultiplyAll(NumList+, Result-) - aggregative multiply all crt numbers in NumList
crtMultiplyAll([Result], Result).
crtMultiplyAll([X1, X2 | Rest], Result) :-
    crtMultiplication(X1, X2, Res),
    crtMultiplyAll([Res | Rest], Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEE compatible crt

beeCrtAddition(Num1, Num2, Result, Cs-Tail) :-
    base(Base),
    beeCrtAddition(Base, Num1, Num2, Result-[], Cs-Tail).

beeCrtAddition([], [], [], Tail-Tail, CsTail-CsTail).
beeCrtAddition([HBase | RestBase], [H1 | Rest1], [H2 | Rest2], [Res | RestRes]-Tail, Cs-CsTail) :-
    HBase2 is HBase * 2,
    % Sum is H1 + H2,
    % Res is mod(Sum, HBase),
    Cs = [
        new_int(Sum, 0, HBase2), 
        int_plus(H1, H2, Sum),
        new_int(Res, 0, HBase),
        int_mod(Sum, HBase, Res) | RestCs ],
    beeCrtAddition(RestBase, Rest1, Rest2, RestRes-Tail, RestCs-CsTail).


% beeCrtSumAll(NumList+, Result-, Cs-) - aggregative add all crt numbers in NumList into the sum Result
beeCrtSumAll([Result], Result, Tail-Tail).
beeCrtSumAll([X1, X2 | Rest], Result, Cs-Tail) :-
    beeCrtAddition(X1, X2, Res, Cs-Cs2),
    beeCrtSumAll([Res | Rest], Result, Cs2-Tail).


beeCrtMultiplication(Num1, Num2, Result, Cs-Tail) :-
    base(Base),
    beeCrtMultiplication(Base, Num1, Num2, Result-[], Cs-Tail).

beeCrtMultiplication([], [], [], Tail-Tail, CsTail-CsTail).
beeCrtMultiplication([HBase | RestBase], [H1 | Rest1], [H2 | Rest2], [Res | RestRes]-Tail, Cs-CsTail) :-
    HBase2 is HBase * HBase,
    % Mul is H1 * H2,
    % Res is mod(Mul, HBase),
    Cs = [
        new_int(MulRes, 0, HBase2),
        int_mul(H1, H2, MulRes),
        new_int(Res, 0, HBase),
        int_mod(MulRes, HBase, Res) | RestCs ],
    beeCrtMultiplication(RestBase, Rest1, Rest2, RestRes-Tail, RestCs-CsTail).

% beeCrtMultiplyAll(NumList+, Result-, Cs-) - aggregative multiply all crt numbers in NumList
beeCrtMultiplyAll([Result], Result, Tail-Tail).
beeCrtMultiplyAll([X1, X2 | Rest], Result, Cs-Tail) :-
    beeCrtMultiplication(X1, X2, Res, Cs-Cs2),
    beeCrtMultiplyAll([Res | Rest], Result, Cs2-Tail).


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
crtToDecimalEncode(CrtNum, map(Decimal), [new_int(Decimal, 1, 10000) | Constraints]) :-%TODO gal find a suitable number
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
    beeFindSummedTopNumerator(1, SolutionDigits, SummedTopNumerator, Cs3-Cs4),
    beeCrtSumAll(SummedTopNumerator, AdditionResultNumerator, Cs4-Cs5),
    extractDenominators(SolutionDigits, DenominatorsList),
    beeCrtMultiplyAll(DenominatorsList, AdditionResultDenominator, Cs5-Cs6),
    Cs6 = [int_eq(AdditionResultDenominator,AdditionResultNumerator)]-[].
    
% SummedTopNumerator = a*ef*hi + d*bc*hi + g*bc*ef (for N = 3)
beeFindSummedTopNumerator(I, _, SummedTopNumerator, Tail-Tail) :-
    length(SummedTopNumerator, Len),
    I > Len.

beeFindSummedTopNumerator(I, SolutionDigits, SummedTopNumerator, Cs-Tail) :-
    length(SummedTopNumerator, Len),
    I =< Len,
    beeCalculateIthTopSumComponent(I, SolutionDigits, Component, Cs-Cs2),
    nth1(I, SummedTopNumerator, Component),
    I1 is I + 1,
    beeFindSummedTopNumerator(I1, SolutionDigits, SummedTopNumerator, Cs2-Tail).


beeCalculateIthTopSumComponent(I, SolutionDigits, Component, Cs-Tail) :-
    DenominatorIndex is (I * 2),
    NumeratorIndex is (I * 2) - 1,
    nth1(NumeratorIndex, SolutionDigits, [ _ = NumeratorI]),
    nth1(DenominatorIndex, SolutionDigits, [ _ = DenominatorI]),
    extractDenominators(SolutionDigits, DenominatorsList),
    select(DenominatorI, DenominatorsList, DenominatorsForMultiplication),
    beeCrtMultiplyAll([NumeratorI | DenominatorsForMultiplication], Component, Cs-Tail).

extractDenominators([], []).
extractDenominators([_, Denominator | Rest], [Denominator | RestExtracted]) :-
    extractDenominators(Rest, RestExtracted).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declareDenominators([], Tail-Tail).
declareDenominators([_, Num = CrtNum | Rest], [new_int(Num, 11, 99) | RestConstraints]-Tail) :-
    base(Base),
    createCrtNum(CrtNum, Base, RestConstraints-Cs2), % create the variables of the representation
    setCrtCorrectnessConstraints(CrtNum, Base, Num, Cs2-Cs3), % force the correctness of the representation
    declareDenominators(Rest, Cs3-Tail).


declareNumerators([], Tail-Tail).
declareNumerators([Num = CrtNum, _ | Rest], [new_int(Num, 1, 9) | RestConstraints]-Tail) :-
    base(Base),
    createCrtNum(CrtNum, Base, RestConstraints-Cs2), % create the variables of the representation
    setCrtCorrectnessConstraints(CrtNum, Base, Num, Cs2-Cs3), % force the correctness of the representation
    declareNumerators(Rest, Cs3-Tail).


createCrtNum([], [], Tail-Tail).
createCrtNum([HCrtNum | RestCrtNum], [HBase | RestBase], [new_int(HCrtNum, 0, HBase) | RestConstraints]-Tail) :-
    createCrtNum(RestCrtNum, RestBase, RestConstraints-Tail).


setCrtCorrectnessConstraints([], [], _, Tail-Tail).
setCrtCorrectnessConstraints([ HCrtNum | RestCrtNum], [HBase | RestBase], Decimal, [ int_mod(Decimal, HBase, HCrtNum) | RestConstraints]-Tail) :-
    setCrtCorrectnessConstraints(RestCrtNum, RestBase, Decimal, RestConstraints-Tail).


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