/*
examples of
maritime_border(State1, State2, "English channel" )  - 2 solutions

part_of_the_world(Capital, Country). - 6 solutions

maritime_border (State1, State2, Sea)- 17 solutions

*/




%(id, name, part of the world, population in millions
state(1 , " the US", america, 331.9  ).
state(2 , " the UK", europe ,  67.33).
state(3 ,  " France", europe , 68) .
state(4 ,  " Spain", europe, 47.5 ).
state(5 ,  " Monaco", europe, 0.0367 ).
state(6 ,  " Turkey", europe_asia, 84.7 ).

%(id, name, populations
capital( 1,"Washington D.C ", "7.74 millions").
capital( 2,"London ", "8.98 millions" ).
capital( 3,"Paris", "2.16 millions" ).
capital( 4,"Madrid", "3.22 millions" ).
capital( 5,"Monte Carlo", "3 thousands").
capital( 6,"Ankara", "5.66 millions" ).


%id , kind of system(republic,  monarchy)
system_of_goverment( 1, republic ).
system_of_goverment( 2, monarchy).
system_of_goverment( 3, republic ).
system_of_goverment( 4, monarchy).
system_of_goverment( 5, monarchy).
system_of_goverment( 6, republic).




%id ,  main_religion(muslim, christianity, islam, indigenous American religions, hinduism ), % in country
religion( 1, protestants , "70 procent" ).
religion( 2, christianity, "50 procent").
religion( 3, christian , "66 procent").
religion( 4, catholicism, "50 procent").
religion( 5, christianity, "86 procent").
religion( 6, muslim , "90 procent").


%id, language
language( 1, english ).
language( 2, english ).
language( 3, french).
language( 5, french).
language( 6, turkish).

%id, UNESCO World Heritage Sites (https://whc.unesco.org/en/list/)
unesco( 1, "24 objects ").
unesco( 2, "33 objects" ).
unesco( 3, "52 objects").
unesco( 4, "49 objects").
unesco( 5, " no objects :( ").
unesco( 6, "21 objects").

%id , territory in sq km
territory( 1, 9833517 ).
territory( 2, 243610).
territory( 3, 675417 ).
territory( 4, 506030).
territory( 5, 2).
territory( 6, 783562).


%ecology , place in raring The Environmental Performance Index

ecology( 1, second ).
ecology( 2, fortythree ).
ecology( 3, therteen).
ecology( 3, twentyseven).
ecology( 5, one_hundred ).
ecology( 6, not_found).


% id, cost_of_living index, rent index (numbeo rating)
cost_of_living( 1, 63.6 , 31.0 ).
cost_of_living( 2, 74.2 , 46.4).
cost_of_living( 3, 69.1 , 23.6).
cost_of_living( 4, 49.9, 20.9).
cost_of_living( 5, 80.1 , 75.6 ).
cost_of_living( 6, 27.1 , 8.5 ).

%World University Ranking 2023
education( 1, "top of the list", "University of Oxford , University of Cambridge " ).
education( 2, "top of the list", "The Ivy League: Harvard University, Massachusetts Institute of Technology ").
education( 3,"47-1500 positions" , "Paris Sciences et Lettres, Sorbonne University").
education( 3, "182-1500 positions", "University of Barcelona, Autonomous University of Barcelona").
education( 5,"101", "International University Monaco - one university" ).
education( 6, "350 - 1500", "Cankaya University, Koç University  ").


%aaccess_to_sea
access_to_sea( 1,  "Beaufort Sea").
access_to_sea( 1,  "Bering Sea‎").
access_to_sea( 1,  "Chukchi Sea‎").

access_to_sea( 2,  "Irish Sea" ).
access_to_sea( 2,  "North Sea").
access_to_sea( 2, "English channel").

access_to_sea( 3,  "North Sea").
access_to_sea( 3, "English channel").
access_to_sea( 3, "Mediterranean Sea").

access_to_sea(4 ,  "Mediterranean Sea").
access_to_sea(4 , "Cantabrian Sea").

access_to_sea( 5 , "Mediterranean Sea").

access_to_sea( 6 , "Mediterranean Sea").
access_to_sea(6 , "the Aegean").
access_to_sea(6 , "Black Sea").
 
%economy level on international level(Gross domestic product 2022)
economy( 1, 1 ).
economy( 2, 6).
economy( 3, 7 ).
economy( 3, 15).
economy( 5, 153 ).
economy( 6, 19).

%populare among travellers( World Economic forume 2021, Travel & Tourism Development Index 2021 overall rankings1)
tourism( 1, 2).
tourism( 2, 8).
tourism( 3, 4).
tourism( 3, 3 ).
tourism( 5, 6).
tourism( 6, 45).


%domen ex 'language( 1, english ).' english == domen 


%generation of capital of government
capital_of_country(Country, Capital) :- state(Id_st, Country, _,_),  capital(Id_st, Capital, _).

%part of the world according to capital id
part_of_the_world(Capital, PartofWorld) :- state(Id_st, _, PartofWorld, _) , capital(Id_cap, Capital, _), Id_st =Id_cap.

%country in World University Ranking 2023
education_top(Name, Ranking, Names_of_uni) :- state(Id_st, Name,_,_) , education(Id_con, Ranking, Names_of_uni), Id_st=Id_con.

%maritime border (sea)
maritime_border(State1, State2, Sea) :- state(Id_st1, State1, _,_) , state(Id_st2, State2, _,_), access_to_sea(Id_st1, Sea) ,  access_to_sea(Id_st2, Sea), Id_st1<>Id_st2.

%most populated part of the world
most_populated_part_otW(State1, State2, Population) :-state(Id_st1, State, _, Population ) , state(Id_st2, State2, _, Population).



