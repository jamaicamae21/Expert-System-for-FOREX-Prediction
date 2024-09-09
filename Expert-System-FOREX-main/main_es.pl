:- op(800, xfx, ==>).
:- op(500, xfy, :).
:- op(810, fx, rule).
:- op(700, xfy, #).
:- use_module(library(csv)).
:- dynamic ma_10/2.
:- dynamic ma_20/2.
:- dynamic ma_50/2.
:- dynamic isa/2.
:- dynamic zone/2.
:- dynamic trend_is/1.
:- dynamic close_value/2.
:- dynamic open_value/2.
:- dynamic current_count/1.
:- dynamic order/1.
:- dynamic fact/1.
:- dynamic decition_three/1.

main :-
    supervisor.


supervisor :-
    start,
    load,
    order.

doit(X) :-
    do(X).

do(exit) :-
    !.

do(order) :-
    order,
    !.

do(load) :-
    load,
    !.

do(list) :-
    lst,
    !.

do(list(X)) :-
    lst(X),
    !.

do(start) :-
    start,
    !.

load :-
    reconsult('omar_strategy.okb').
    %write('Loading okb file ''omar_strategy.okb'' '), nl.

assert_list([]) :-
    !.

assert_list([H|T]) :-
    assertz(fact(H)),
    !,
    assert_list(T).

start :-
    init_dynamic_fact,
    read_csv_file('ohlc_data.csv', Rows),
    process_rows(Rows),
    %ma_10(_, MA10), ma_20(_, MA20), ma_50(_, MA50),
    isa(_, Color), trend_is(Trend), zone(_, Zone), close_value(Time, _),
    asserta(fact(isa(candlestick, Color))),
    asserta(fact(zone(candlestick, Zone))),
    asserta(fact(trend_is(Trend))),
    asserta(fact(close_value(_, Time))),
    asserta(fact(order(none))),
    write('Date and Time: '), write(Time), nl, !.
    %write('Moving averages: '), nl,
    %write('MA10: '), write(MA10), nl,
    %write('MA20: '), write(MA20), nl,
    %write('MA50: '), write(MA50), nl,
    %write('Candlestick Color: '), write(Color), nl,
    %write('Candlestick Zone: '), write(Zone), nl,
    %write('Trend: '), write(Trend), nl, !.

order :-
    call(rule ID: LHS ==> RHS),
    try(LHS,RHS),
    write('Rule fired '), write(ID), nl,
    fact(order(Order)),
    write('Order: '), write(Order), nl, nl,
    !,
    order.
order.

try(LHS,RHS) :-
    match(LHS),
    process(RHS,LHS),
    !.

match([]) :-
    !.

match([_:Prem|Rest]) :-
    !,
    (fact(Prem)
    ;
    test(Prem)
    ),
    match(Rest).

match([Prem|Rest]) :-
    (fact(Prem) 
    ;
    test(Prem)
    ),
    match(Rest).

test(not(X)) :-
    fact(X),
    !,
    fail.

test(not(_)) :-
    !.

test(X # Y) :-
    X = Y,
    !.

test(X > Y) :-
    X > Y,
    !.

test(X >= Y) :-
    X >= Y,
    !.

test(X < Y) :-
    X < Y,
    !.

test(X =< Y) :-
    X =< Y,
    !.

test(X = Y) :-
    X is Y,
    !.

test(member(X,Y)) :-
    member(X,Y),
    !.

process([],_) :-
    !.

process([Action|Rest],LHS) :-
    take(Action,LHS),
    !,
    process(Rest,LHS).

take(retract(N),LHS) :-
    (N == all
    ;
    integer(N)
    ),
    retr(N,LHS),!.

take(A,_) :-
    take(A),!.

take(retract(X)) :-
    retract(fact(X)),
    !.

take(assert(X)) :-
    asserta(fact(X)),
    %write('You should position as: '), write(X), nl,
    !.

take(X # Y) :-
    X = Y,
    !.

take(X = Y) :-
    X is Y,
    !.

take(write(X)) :-
    write(X),
    !.

take(nl) :-
    nl,
    !.

take(read(X)) :-
    read(X),
    !.

take(prompt(X,Y)) :-
    nl, write(X), read(Y),
    !.

take(member(X,Y)) :-
    member(X,Y),
    !.

take(list(X)) :-
    lst(X),
    !.

% logic for retraction
retr(all,LHS) :-
    retrall(LHS),
    !.

retr(N,[]) :-
    write('retract error, no '-N), nl,
    !.

retr(N,[N:Prem|_]) :-
    retract(fact(Prem)),
    !.

retr(N,[_|Rest]) :-
    !,
    retr(N,Rest).

retrall([]).
retrall([_:Prem|Rest]) :-
    retract(fact(Prem)),
    !, retrall(Rest).

retrall([Prem|Rest]) :-
    retract(fact(Prem)),
    !, retrall(Rest).

retrall([_|Rest]) :- % must have been a test
    retrall(Rest).

% list all of the terms in working storage
lst :-
    fact(X),
    write(X), nl,
    fail.

lst :-
    !.

% lists all of the terms which match the pattern
lst(X) :-
    fact(X),
    write(X), nl,
    fail.

lst(_) :-
    !.

% utilities
member(X,[X|_]).
member(X,[_|Z]) :-
member(X,Z).

% Candlestick Manipulation

init_dynamic_fact :-
    retractall(fact(_)),
    retractall(order(_)),
    asserta(current_count(0)),
    asserta(ma_10([], 0)),
    asserta(ma_20([], 0)),
    asserta(ma_50([], 0)),
    asserta(isa(_, _)),
    asserta(zone(_, _)),
    asserta(close_value(0, 0)),
    asserta(open_value(0, 0)),
    asserta(trend_is(_)).

check_candlestick(Open, Close) :-
    retractall(isa(_, _)),
    ( Open < Close -> asserta(isa(candlestick, green))
    ; Open > Close -> asserta(isa(candlestick, red))
    ; Open is Close -> asserta(isa(candlestick, neutral))).

read_csv_file(File, Rows) :-
    csv_read_file(File, Rows, [functor(row)]).

process_rows([]) :-
    open_value(_, Open),
    close_value(_, Close),
    check_candlestick(Open, Close),
    calculate_and_assert_moving_average(10),
    calculate_and_assert_moving_average(20),
    calculate_and_assert_moving_average(50),
    check_zone,
    check_trend.

process_rows([Row|Rows]) :-
    Row = row(NewTime, O_val, _, _, C_val),
    retractall(close_value(_, _)),
    retractall(open_value(_, _)),
    asserta(close_value(NewTime, C_val)),
    asserta(open_value(NewTime, O_val)),
    update_moving_averages(C_val),
    process_rows(Rows).

check_zone :-
    ma_10(_, ZM10),
    ma_20(_, ZM20),
    close_value(_, Close),
    retractall(zone(_, _)),
    (Close >= ZM10, Close =< ZM20
    ->
    assert(zone(candlestick, thezone))
    ; Close =< ZM10, Close >= ZM20
    ->
    assert(zone(candlestick, thezone))
    ; Close > ZM10, Close > ZM20, ZM10 > ZM20
    ->
    assert(zone(candlestick, above))
    ; Close > ZM10, Close > ZM20, ZM10 < ZM20
    ->
    assert(zone(candlestick, above))
    ; Close < ZM10, Close < ZM20, ZM10 < ZM20
    ->
    assert(zone(candlestick, below))
    ; Close < ZM10, Close < ZM20, ZM10 > ZM20
    ->
    assert(zone(candlestick, below))
    ; true -> assert(zone(candlestick, not_clear))).


check_trend :-
    retractall(trend_is(_)),
    ma_10(_, TMA10),
    ma_20(_, TMA20),
    ma_50(_, TMA50),
    close_value(_, Close),
    open_value(_, Open),
    
    (TMA10 < TMA20, TMA20 > TMA50, TMA10 > TMA50, Close =< Open
    ->
    assert(trend_is(going_bearish))
    ; TMA10 =< TMA20, TMA50 > TMA10, TMA50 > TMA20
    ->
    assert(trend_is(bearish))
    ; TMA10 > TMA20, TMA20 < TMA50, TMA50 > TMA10, Close >= Open
    ->
    assert(trend_is(going_bullish))
    ; TMA50 >= TMA10, TMA50 =< TMA20, TMA10 > TMA20
    ->
    assert(trend_is(going_bullsih))
    ; TMA50 =< TMA10, TMA50 >= TMA20, TMA20 > TMA10
    ->
    assert(trend_is(going_bearish))
    ; TMA10 >= TMA20, TMA20 > TMA50
    ->
    assert(trend_is(bullish))
    ; true -> assert(trend_is(not_clear))
    ).


update_moving_averages(Close) :-
    ma_10(List10, _),
    ma_20(List20, _),
    ma_50(List50, _),
    current_count(C),
    %add_count(Count),
    Count is C + 1,
    retractall(current_count(_)),
    asserta(current_count(Count)),
    ( Count =< 10 ->
    insert_element_last(10, List10, Close)
    ;
     Count > 10 ->
    insert_close_value(10, List10, Close)
    ),

    ( Count =< 20 ->
    insert_element_last(20, List20, Close)
    ;
     Count > 20 ->
    insert_close_value(20, List20, Close)
    ),

    ( Count =< 50 ->
    insert_element_last(50, List50, Close)
    ;
     Count > 50 ->
    insert_close_value(50, List50, Close)
    ).

add_count(Count) :-
    current_count(C),
    ( C > 51 ->
    Count = 51
    ; Count is C + 1 ),
    retract(current_count(_)),
    assert(current_count(Count)).

calculate_and_assert_moving_average(Period) :-
    ( Period = 10
    ->
    retract(ma_10(L10, _)),
    sum_float_list(L10, Sum10),
    MovingAverage10 is Sum10 / Period,
    assert(ma_10(L10, MovingAverage10))
    ;
     Period = 20
    ->
    retract(ma_20(L20, _)),
    sum_float_list(L20, Sum20),
    MovingAverage20 is Sum20 / Period,
    assert(ma_20(L20, MovingAverage20))
    ;
     Period = 50
    ->
    retract(ma_50(L50, _)),
    sum_float_list(L50, Sum50),
    MovingAverage50 is Sum50 / Period,
    assert(ma_50(L50, MovingAverage50))
    ).

insert_element_last(Period, List, Element) :-
    append(List, [Element], Result),
    ( Period = 10 ->
    retractall(ma_10(_, _)),
        asserta(ma_10(Result, 0))
    ; Period = 20 ->
    retractall(ma_20(_, _)),
        asserta(ma_20(Result, 0))
    ; Period = 50 ->
    retractall(ma_50(_, _)),
        asserta(ma_50(Result, 0))).

insert_close_value(Period, List, Close) :-
    List = [_|Tail], % Skip the first element
    append(Tail, [Close], UpdatedList),
    ( Period = 10 ->
        retractall(ma_10(_, _)),
        asserta(ma_10(UpdatedList, 0))
    ; Period = 20 ->
        retractall(ma_20(_, _)),
        asserta(ma_20(UpdatedList, 0))
    ; Period = 50 ->
        retractall(ma_50(_, _)),
        asserta(ma_50(UpdatedList, 0))).

sum_float_list([], 0).

sum_float_list([X|Xs], Sum) :-
    sum_float_list(Xs, TailSum), % Recursively calculate the sum of the tail
    Sum is X + TailSum.  % Calculate the sum of the list by adding the head to the tail sum

sum_float_list([_|Xs], Sum) :-
    sum_float_list(Xs, Sum).


