% Copyright

implement main
    open core, file, stdio

domains
    partoftheworld = america; europe; europe_asia.
    system_of_goverment = republic; monarchy.
    religions = protestants; christianity; catholicism; muslim.
    languages = english; french; turkish.

class facts - countriesDb
    state : (integer Id, string Country, partoftheworld Partoftheworld, real Population).
    capital : (integer Id, string Capital, string Populations).
    system_of_goverment : (integer Id, system_of_goverment System_of_goverment).
    religion : (integer Id, religions Religion, string Procent).
    languag : (integer Id, languages Languag).
    unesco : (integer Id, string Objects).
    unesco1 : (integer Id, integer Objects).
    territory : (integer Id, integer SQinkm).
    ecology : (integer Id, string Place).
    %cost_of_livin : (integer Id, real Cost_of_live, real Rent_index).
    education : (integer Id, string Position, string Uni1, string Uni2).
    access_to_sea : (integer Id, string Sea).
    economy : (integer Id, integer PlaceinGross2022).
    tourism : (integer Id, integer Placeinrating).
    metro_daily_ridership : (integer Id, real Number).
    metro_opening_date : (integer Id, integer Year).
    metro_type : (integer Id, string Type).

class facts
    count : (integer Amount) single.

%LISTS
class predicates
    lists_Capital_In_Partoftheworld : (partoftheworld Partoftheworld) -> string* Capital1 determ.
clauses
    lists_Capital_In_Partoftheworld(X) = List :-
        !,
        List =
            [ NameCapital ||
                state(Id, _, X, _),
                capital(Id, NameCapital, _)
            ].
%Lists by the elements

class predicates
    lists_Capital_In_Partoftheworld1 : (partoftheworld Partoftheworld) -> real* Capital1 determ.
clauses
    lists_Capital_In_Partoftheworld1(X) = List :-
        !,
        List =
            [ Population ||
                state(_, _, X, Population)
                %capital(Id, _, _)
            ].

class predicates
    write_list : (List*).
clauses
%пустой
    write_list([]).
%список из 1-го элемента
    write_list([H]) :-
        write(H, "."),
        !.
    write_list([H | T]) :-
        write(H, ", "),
        nl,
        write_list(T).

%lenght
class predicates
    length_of : (A*, integer Amount [out]).
clauses
    length_of([], 0).
    length_of([_ | T], Amount + 1) :-
        length_of(T, Amount).

%sum
class predicates  %Вспомогательные предикаты
    сумма_элем : (real* List) -> real Sum.
clauses
    сумма_элем([]) = 0.
    сумма_элем([H | T]) = сумма_элем(T) + H.

%search an element
class predicates
    contains : (string Capital, string* List) determ.
clauses
    contains(Capital, []) :-
        write("No").
    contains(Capital, [Capital | _]) :-
        !,
        write("Yes").
    contains(Capital, [_ | T]) :-
        contains(Capital, T).

clauses
    count(0).

class predicates
    amount_of_objects : ().
clauses
    amount_of_objects() :-
        writef("\n Number of objests unesco: \n"),
        assert(count(0)),
        unesco1(_, Unesco),
        count(Amount),
        assert(count(Amount + Unesco)),
        fail.
    amount_of_objects() :-
        count(Amount),
        write("Number of objects: ", Amount).

%capital - country
class predicates
    capital_of_country : (string NameCountry, string NameCapital) nondeterm anyflow.
clauses
    capital_of_country(NameCountry, NameCapital) :-
        %то что подается и выводится
        state(IdCountry, NameCountry, _, _),
        capital(IdCountry, NameCapital, _).

%capital - part of the world
class predicates
    part_of_the_world : (string Capital [out], partoftheworld PartofWorld [out]) nondeterm.
clauses
    part_of_the_world(Capital, PartofWorld) :-
        %то что подается и выводится
        capital(Id_cap, Capital, _),
        Id_cap = Id_st,
        state(Id_st, _, PartofWorld, _).

class predicates
    education_top : (string NameCountry [out], string Ranking [out], string Names_of_uni [out]) nondeterm.
clauses
    education_top(NameCountry, Ranking, Names_of_uni) :-
        state(Id_st, NameCountry, _, _),
        education(Id_con, Ranking, Names_of_uni, _),
        Id_st = Id_con.

class predicates
    unesco_objects : (string Capital [out], partoftheworld PartofWorld [out]) nondeterm.
clauses
    unesco_objects(Capital, PartofWorld) :-
        %то что подается и выводится
        capital(Id_cap, Capital, _),
        Id_cap = Id_st,
        state(Id_st, _, PartofWorld, _).

clauses
    run() :-
        file::consult("../countries.txt", countriesDb),
        fail.

    run() :-
        capital_of_country(NameCountry, NameCapital),
        writef("\nName of the country - %: It's capital - %", NameCountry, NameCapital),
        nl,
        fail.

    run() :-
        part_of_the_world(Capital, PartofWorld),
        writef("\nThis capital of % is located in  % part of the world", Capital, PartofWorld),
        nl,
        fail.

    run() :-
        education_top(NameCountry, _Ranking, Names_of_uni),
        writef("\nThe best uni in - % is  - %", NameCountry, Names_of_uni),
        nl,
        fail.

    run() :-
        amount_of_objects(),
        nl,
        fail.

    %список вывод
    run() :-
        L = lists_Capital_In_Partoftheworld(europe),
        write(L),
        nl,
        nl,
        fail.

    run() :-
        write_list(lists_Capital_In_Partoftheworld(europe)),
        nl,
        nl,
        fail.

%lenght output
    run() :-
        write("Amount of capitals in Europe "),
        length_of(lists_Capital_In_Partoftheworld(europe), Amount),
        write(Amount),
        nl,
        nl,
        fail.

    run() :-
        write("Sum of population of capitals in europe "),
        write(сумма_элем(lists_Capital_In_Partoftheworld1(europe))),
        nl,
        nl,
        fail.

    run() :-
        write("Monte Carlo is in a list 'capital' "),
        nl,
        contains("Monte Carl", lists_Capital_In_Partoftheworld(europe)),
        nl,
        fail.

    run() :-
        write("Monte Carlo is in a list 'capital' "),
        nl,
        contains("Monte Carlo", lists_Capital_In_Partoftheworld(europe)),
        nl,
        fail.

    run().

end implement main

goal
    console::runUtf8(main::run).
