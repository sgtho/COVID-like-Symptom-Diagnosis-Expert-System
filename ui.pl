% ui.pl
% ---------------------------
% User Interface for Diagnosis Expert System
% Handles all I/O: prompts, reading, formatting, reporting
% ---------------------------

:- module(ui, [
    ask_patient_info/0,
    ask_duration/0,
    ask_exposure/0,
    ask_symptoms/1,
    ask_serious_symptoms/0,
    ask_common_symptoms/0,
    ask_all_less_common_symptoms/0,
    ask_runny_nose/0,
    ask_remaining_less_common_symptoms/0,
    diagnose_summary/0,
    prevention_tips/0
]).

:- use_module(wm).
:- use_module(kb).

%--- Helper: Yes/No Question Input ---
ask_yes_no(Question, true) :-
    format('~w (y/n): ', [Question]),
    read(Response),
    downcase_atom(Response, L),
    member(L, [y, yes]), !.
ask_yes_no(_, false).

%--- Gather Basic Patient Info ---
ask_patient_info :-
    write('Hello! May I have your full name, please?'), nl,
    read(Name), assertz(wm:patient_name(Name)),
    format('Thank you, ~w. How old are you?~n', [Name]),
    write('Enter your age (e.g., 45)'), nl,
    read(Age), assertz(wm:patient_age(Age)),
    write('What is your gender? (male. or female.)'), nl,
    read(Gender), assertz(wm:patient_gender(Gender)),
    write('Select any pre-existing conditions (enter list of numbers, [1]., [1,3]., or []).'), nl,
    write(' 1 hypertension'), nl,
    write(' 2 diabetes'), nl,
    write(' 3 cardiovascular_disease'), nl,
    write(' 4 chronic_respiratory_disease'), nl,
    write(' 5 cancer'), nl,
    write('E.g., [1,3] for hypertension and cardiovascular disease.'), nl,
    read(Nums), map_conditions(Nums, Conds), assertz(wm:patient_conditions(Conds)), nl.

map_conditions([], []).
map_conditions([N|T], [C|CT]) :- kb:cond_map(N,C), !, map_conditions(T,CT).
map_conditions([_|T], CT)     :- map_conditions(T,CT).

%--- Ask About Duration of Symptoms ---
ask_duration :-
    write('How many days have you felt unwell?'), nl,
    write('The incubation period can be 1–14 days. Transmission is possible even before symptoms.'), nl,
    write('If you do not feel unwell, please enter 0.'), nl,
    read(Days), assertz(wm:exposure_days_ago(Days)), nl.

%--- Ask About Exposure ---
ask_exposure :-
    write('Have you been in close contact with a sick person or touched shared surfaces in public in the last 14 days?'), nl,
    ask_yes_no('Please answer', Ans),
    assertz(wm:patient_exposure(Ans)), nl.

%--- Ask About Symptoms (Generic, By Category) ---
ask_symptoms(Type) :-
    kb:symptom_category(Type, List),
    forall(member(Sym, List), (
        format('Have you been experiencing ~w?~n', [Sym]),
        ask_yes_no('Type y. or n.>', Ans),
        ( Ans -> assertz(wm:has_symptom(Sym)) ; true )
    )).

%--- Specialized Routines for Interview Logic ---
ask_serious_symptoms        :- ask_symptoms(serious).
ask_common_symptoms         :- ask_symptoms(common).
ask_all_less_common_symptoms :- ask_symptoms(less_common).

ask_runny_nose :-
    format('Have you been experiencing runny nose?~n', []),
    ask_yes_no('Type y. or n.>', Ans),
    ( Ans -> assertz(wm:has_symptom(runny_nose)) ; true ).

ask_remaining_less_common_symptoms :-
    kb:symptom_category(less_common, List),
    exclude(=(runny_nose), List, Rest),
    forall(member(Sym, Rest), (
        format('Have you been experiencing ~w?~n', [Sym]),
        ask_yes_no('Type y. or n.>', Ans),
        ( Ans -> assertz(wm:has_symptom(Sym)) ; true )
    )).

%--- Session Summary and Prevention Tips ---
diagnose_summary :-
    write('\nSummary:'), nl,
    (wm:patient_name(Name)    -> format('Name: ~w~n',    [Name]) ; true),
    (wm:patient_age(A)        -> format('Age: ~w~n',     [A])    ; true),
    (wm:patient_gender(G)     -> format('Gender: ~w~n',  [G])    ; true),
    (wm:patient_conditions(C) -> format('Conditions: ~w~n', [C]) ; true),
    (wm:patient_exposure(E)   -> (E==true -> write('Recent exposure: Yes\n') ; write('Recent exposure: No\n')) ; true),
    write('Symptoms reported:'), nl,
    (wm:has_symptom(_) -> forall(wm:has_symptom(S), format(' - ~w~n', [S])) ; write(' - None')), nl,
    (kb:high_risk_overall -> write('Notice: You have high-risk factors.'), nl ; write('No major risk factors.'), nl),
    (kb:high_risk_gender -> write('Note: Males in high-risk groups may face slightly higher risk.'), nl ; true),
    nl.

prevention_tips :-
    write('\nPrevention & Transmission Tips:'), nl,
    write('- This virus spreads mainly via droplets when talking, sneezing, or coughing in close contact.'), nl,
    write('- Droplets may land on surfaces; the virus can survive for hours (copper/cardboard) up to days (plastic/steel).'), nl,
    write('- Always wash hands after touching public surfaces or shared objects.'), nl,
    write('- Disinfect high-touch surfaces regularly (door handles, phones, counters).'), nl,
    write('- The incubation period is 1–14 days. Infection can occur and be transmitted even before symptoms appear.'), nl,
    write('- Asymptomatic and pre-symptomatic spread is possible—stay vigilant, even if you feel well.'), nl,
    write('- Wearing a mask and maintaining physical distance helps protect both yourself and others.'), nl.
