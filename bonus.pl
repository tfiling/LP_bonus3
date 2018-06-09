:- module('bonus', [ crtRepresentationToDecimal/2 ]).
:- use_module('./bee/bApplications/auxs/auxRunExpr',[runExpr/5, runExprMax/5, runExprMin/5, decodeInt/2, decodeIntArray/2]).
:- use_module('./bee/bApplications/auxs/auxMatrix',[matrixCreate/3, matrixGetCell/4]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% part 1: verify
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify(N, DigitList) :-
    length(DigitList, Len),
    0 is mod(Len, 3),
    N is div(Len, 3),
    parseDigitList(DigitList, NumeratorsList, DenominatorsList),
    length(SummedTopNumerator, N),
    findSummedTopNumerator(1, NumeratorsList, DenominatorsList, SummedTopNumerator),
    crtSumAll(SummedTopNumerator, AdditionResultNumerator),
    crtMultiplyAll(DenominatorsList, AdditionResultDenominator),
    AdditionResultDenominator == AdditionResultNumerator.



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

base([2, 3, 5, 7, 11]).

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
crtToDecimalEncode(CrtNum, map(Decimal), [new_int(Decimal, 1, 10000) | Constraints]) :-
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










%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :-
%     crtRepresentation(17, X1),
%     crtRepresentation(15, X2),
%     crtMultiplication(X1, X2, X),
%     writeln(X),
%     crtRepresentationToDecimal(X, Num),
%     writeln(Num).


t:-
    verify(3, [5, 3, 4, 7, 6, 8, 9, 1, 2]).
    % length(SummedTopNumerator, 3),
    % NumeratorsList = [[1,2,0,5,5],[1,1,2,0,7],[1,0,4,2,9]],
    % DenominatorsList = [[0,1,4,6,1],[0,2,3,5,2],[0,0,2,5,1]],
    % findSummedTopNumerator(1, NumeratorsList, DenominatorsList, SummedTopNumerator).