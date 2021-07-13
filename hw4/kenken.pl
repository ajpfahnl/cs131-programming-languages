% ------------------------------------
% kenken
% ------------------------------------
kenken(N, C, T) :- 
    generateMatrix(N, T),
    transpose(T, Ttrans),
    maplist(fd_all_different, T),
    maplist(fd_all_different, Ttrans),
    constrain(T, C),
    maplist(fd_labeling, T).

% Extract value at a coordinate
matrixVal(V, [Y|X], T) :-
    nth(Y, T, Row),
    nth(X, Row, V).

% Matrix transposition taken from TA code here:
% https://github.com/CS131-TA-team/UCLA_CS131_CodeHelp/blob/master/Prolog/sudoku_cell.pl
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% Creating the matrix
generateMatrix(N, T) :-
    length(T, N),
    generateMatrixRows(N, T).

generateMatrixRows(_, []).
generateMatrixRows(N, [H|T]) :-
    length(H, N),
    fd_domain(H, 1, N),
    generateMatrixRows(N, T).

% Constraining a matrix where numbers in each row and column
% are unique
constrain(_, []).
constrain(T, [CH|CT]) :-
    constrain(T, CH),
    constrain(T, CT).
% +(S, L)
% means the integer S is the sum of integers in the list L of squares.
constrain(T, +(S,L)) :-
    cageAdd(T, L, Sum),
    S #= Sum.
% *(P, L)
% means the integer P is the product of the integers in the list L of squares.
constrain(T, *(P, L)) :-
    cageMultiply(T, L, Prod),
    P #= Prod.
% −(D, J, K)
% means the integer D is the difference between the integer j in square J and
% the integer k in square K; D could be equal to either j−k or to k−j.
constrain(T, -(D, J, K)) :-
    matrixVal(VJ, J, T),
    matrixVal(VK, K, T),
    D #= VJ - VK.
constrain(T, -(D, J, K)) :-
    matrixVal(VJ, J, T),
    matrixVal(VK, K, T),
    D #= VK - VJ.
% /(Q, J, K)
% means the integer Q is the quotient of the integer j in square J and the
% integer k in square K; Q could be equal to either j÷k or to k÷j. The
% remainder must be zero.
constrain(T, /(Q, J, K)) :-
    matrixVal(VJ, J, T),
    matrixVal(VK, K, T),
    Q #= VJ / VK.
constrain(T, /(Q, J, K)) :-
    matrixVal(VJ, J, T),
    matrixVal(VK, K, T),
    Q #= VK / VJ.

% Helpers
cageAdd(_, [], Sum) :-
    Sum = 0.
cageAdd(T, [Hd|Tl], Sum) :-
    matrixVal(V, Hd, T),
    cageAdd(T, Tl, A),
    Sum = V + A.

cageMultiply(_, [], Prod) :-
    Prod = 1.
cageMultiply(T, [Hd|Tl], Prod) :-
    matrixVal(V, Hd, T),
    cageMultiply(T, Tl, P),
    Prod = V * P.

% ------------------------------------
% plain_kenken
% ------------------------------------

plain_kenken(N, C, T) :-
    plain_generateMatrix(N, T),
    transpose(T, Ttrans),
    maplist(all_different, T),
    maplist(all_different, Ttrans),
    plain_constrain(T, C).

% Creating the matrix (non-fd)
plain_generateMatrix(N, T) :-
    length(T, N),
    plain_generateMatrixRows(N, T).

plain_generateMatrixRows(_, []).
plain_generateMatrixRows(N, [H|T]) :-
    length(H, N),
    domain(H, 1, N),
    all_different(H),
    plain_generateMatrixRows(N, T).

domain(List, Min, Max) :- % same as "elements_between" TA discussion code
    maplist(between(Min, Max), List).

% non-fd all_different, same as "all_unique" TA discussion code
all_different([]).
all_different([Hd|Tl]) :- 
    member(Hd, Tl), !, fail.
all_different([_ | Tl]) :-
    all_different(Tl).

% Non-fd constrain (almost identical, just don't use #_ operators)
plain_constrain(_, []).
plain_constrain(T, [CH|CT]) :-
    plain_constrain(T, CH),
    plain_constrain(T, CT).
% +(S, L)
plain_constrain(T, +(S,L)) :-
    cageAdd(T, L, Sum),
    S =:= Sum.
% *(P, L)
plain_constrain(T, *(P, L)) :-
    cageMultiply(T, L, Prod),
    P =:= Prod.
% −(D, J, K)
plain_constrain(T, -(D, J, K)) :-
    matrixVal(VJ, J, T),
    matrixVal(VK, K, T),
    D =:= VJ - VK.
plain_constrain(T, -(D, J, K)) :-
    matrixVal(VJ, J, T),
    matrixVal(VK, K, T),
    D =:= VK - VJ.
% /(Q, J, K)
plain_constrain(T, /(Q, J, K)) :-
    matrixVal(VJ, J, T),
    matrixVal(VK, K, T),
    Q =:= VJ / VK.
plain_constrain(T, /(Q, J, K)) :-
    matrixVal(VJ, J, T),
    matrixVal(VK, K, T),
    Q =:= VK / VJ.

% ------------------------------------
% Test cases
% ------------------------------------
kenken_testcase_1(
  6,
  [
   +(11, [[1|1], [2|1]]),
   /(2, [1|2], [1|3]),
   *(20, [[1|4], [2|4]]),
   *(6, [[1|5], [1|6], [2|6], [3|6]]),
   -(3, [2|2], [2|3]),
   /(3, [2|5], [3|5]),
   *(240, [[3|1], [3|2], [4|1], [4|2]]),
   *(6, [[3|3], [3|4]]),
   *(6, [[4|3], [5|3]]),
   +(7, [[4|4], [5|4], [5|5]]),
   *(30, [[4|5], [4|6]]),
   *(6, [[5|1], [5|2]]),
   +(9, [[5|6], [6|6]]),
   +(8, [[6|1], [6|2], [6|3]]),
   /(2, [6|4], [6|5])
  ]
).

kenken_testcase_2(
  4,
  [
   +(6, [[1|1], [1|2], [2|1]]),
   *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
   -(1, [3|1], [3|2]),
   -(1, [4|1], [4|2]),
   +(8, [[3|3], [4|3], [4|4]]),
   *(2, [[3|4]])
  ]
).

kenken_testcase_3(1, []).

kenken_testcase_4(2, []).

% ------------------------------------
% statistics (using lnxsrv11)
% ------------------------------------
% kenken on kenken_testcase_2:
% Running Bash command:
%   gprolog --consult-file 'kenken' --query-goal 'fd_set_vector_max(255), kenken_testcase_2(N,C), kenken(N,C,T), !, statistics'
% Results:
% Memory               limit         in use            free

%    trail  stack      16383 Kb            5 Kb        16378 Kb
%    cstr   stack      16383 Kb            9 Kb        16374 Kb
%    global stack      32767 Kb            6 Kb        32761 Kb
%    local  stack      16383 Kb            0 Kb        16383 Kb
%    atom   table      32768 atoms      1793 atoms     30975 atoms

% Times              since start      since last

%    user   time       0.003 sec       0.003 sec
%    system time       0.002 sec       0.002 sec
%    cpu    time       0.005 sec       0.005 sec
%    real   time       0.016 sec       0.016 sec

%
% plain_kenken on kenken_testcase_2:
% Running Bash command:
%   gprolog --consult-file 'kenken' --query-goal 'kenken_testcase_2(N,C), plain_kenken(N,C,T), !, statistics'
% Results:
% Memory               limit         in use            free

%    trail  stack      16383 Kb            0 Kb        16383 Kb
%    cstr   stack      16384 Kb            0 Kb        16384 Kb
%    global stack      32767 Kb            4 Kb        32763 Kb
%    local  stack      16383 Kb            6 Kb        16377 Kb
%    atom   table      32768 atoms      1792 atoms     30976 atoms

% Times              since start      since last

%    user   time       0.110 sec       0.110 sec
%    system time       0.003 sec       0.003 sec
%    cpu    time       0.113 sec       0.113 sec
%    real   time       0.133 sec       0.133 sec

% kenken and plain_kenken use around the same amount of memory,
% but their runtime performances are vastly different. Focusing
% on "real time", kenken (0.016 sec) was 8.3 times faster than
% plain_kenken (0.133 sec). This mirrors our expectations that 
% finite domain solver is way faster.

% ------------------------------------
% no-op KenKen using GNU Prolog
% ------------------------------------
% API:
% predicate: noopkeken/4
% N: a nonnegative integer specifying the number of cells on each side of the
%     KenKen square.
% C: a list of tuples. In each tuple, the first element is the target number
%   and the second element is a list of squares. Squares are represented by
%   [i|j].
% T: a list of list of integers. All the lists have length N. This represents
%     the N×N grid.
% O: a list of operations associated with each cage.
% NOTE: Checking - and / satisfies a cage only happens when the cage has two
% elements
% With a successful call, T should be filled in with numbers and O should be
% filled with operations on the cages associated with the numbers in T that
% satisfies the targets. With an unsuccessful call, there must not have
% been a grid with associated operations that could have satisfied the targets.

% Example test case:
noop_kenken_testcase(
    4,
    [
        (6, [[1|1], [1|2], [2|1]]),
        (96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
        (1, [3|1], [3|2]),
        (1, [4|1], [4|2]),
        (8, [[3|3], [4|3], [4|4]]),
        (2, [[3|4]])
    ]
).
% Query
%   ?- noop_kenken_testcase(N,C), noopkenken(N, C, T, O).
% Output (for one result)
%     C = [(6, [[1|1], [1|2], [2|1]]),
%         (96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
%         (1, [3|1], [3|2]),
%         (1, [4|1], [4|2]),
%         (8, [[3|3], [4|3], [4|4]]),
%         (2, [[3|4]])
%         ]
%     N = 4
%     T = [[1,2,3,4],[3,4,2,1],[4,3,1,2],[2,1,4,3]]
%     O = [+, *, -, -, +, *]
