% engine.pl
% ---------------------------
% Inference Engine for Diagnosis Expert System
% Contains core logic for question flow and diagnosis reasoning.
% Controls the interview and diagnosis process.
% ---------------------------

:- module(engine, [run_interview/0, diagnose_probability/0]).  % Export both entry and probability diagnosis

:- use_module(wm).
:- use_module(kb).
:- use_module(ui).

%--- Entry Point: Run a Diagnosis Interview ---
run_interview :-
    wm:clean_data,
    ui:ask_patient_info,
    ui:ask_duration,
    ui:ask_exposure,
    interview_logic.

%--- Central Interview Logic: Implements Decision Tree/Flowchart ---
interview_logic :-
    ui:ask_serious_symptoms,
    ( serious_symptom_present ->
        ui:ask_common_symptoms,
        write('Thank you.'), nl,
        ui:diagnose_summary,
        write('Probability assessment:'), nl,
        diagnose_probability,
        write('Recommendations:'), nl,
        diagnose, nl,
        ui:prevention_tips
    ;
        ( ui:ask_common_symptoms,
          common_symptom_present ->
            ui:ask_runny_nose,
            ( wm:has_symptom(runny_nose) ->
                write('Thank you.'), nl,
                ui:diagnose_summary,
                write('Probability assessment:'), nl,
                diagnose_probability,
                write('Recommendations:'), nl,
                diagnose, nl,
                ui:prevention_tips
            ;
                ui:ask_remaining_less_common_symptoms,
                write('Thank you.'), nl,
                ui:diagnose_summary,
                write('Probability assessment:'), nl,
                diagnose_probability,
                write('Recommendations:'), nl,
                diagnose, nl,
                ui:prevention_tips
            )
        ;
            ui:ask_all_less_common_symptoms,
            write('Thank you.'), nl,
            ui:diagnose_summary,
            write('Probability assessment:'), nl,
            diagnose_probability,
            write('Recommendations:'), nl,
            diagnose, nl,
            ui:prevention_tips
        )
    ).

%--- Internal Rules for Reasoning ---
serious_symptom_present :-
    wm:has_symptom(difficulty_breathing);
    wm:has_symptom(chest_pain);
    wm:has_symptom(loss_of_speech_or_movement).

common_symptom_present :-
    wm:has_symptom(fever);
    wm:has_symptom(persistent_dry_cough);
    wm:has_symptom(tiredness).

%--- Probability Level Reasoning (Virus-like infection) ---
% These rules combine exposure, incubation, and symptoms to produce a risk level.

% High probability: recent exposure, right incubation, and any key symptom
probability_level(high) :-
    wm:patient_exposure(true),
    wm:exposure_days_ago(Days), Days >= 1, Days =< 14,
    (wm:has_symptom(fever); wm:has_symptom(persistent_dry_cough); wm:has_symptom(difficulty_breathing)).

% Medium probability: key symptoms without known exposure, or
% recent exposure but symptom onset is a little early/late
probability_level(medium) :-
    (
        (wm:patient_exposure(false),
            (wm:has_symptom(fever); wm:has_symptom(persistent_dry_cough); wm:has_symptom(difficulty_breathing)))
        ;
        (wm:patient_exposure(true),
            wm:exposure_days_ago(Days),
            (Days < 1 ; Days > 14),
            (wm:has_symptom(fever); wm:has_symptom(persistent_dry_cough); wm:has_symptom(difficulty_breathing)))
    ).

% Low probability: less common or mild symptoms, no exposure or doesn't fit above
probability_level(low) :-
    wm:has_symptom(_),   % Has some symptom
    \+ probability_level(high),
    \+ probability_level(medium).

probability_level(low) :-
    wm:patient_exposure(true),
    \+ wm:has_symptom(_).  % No symptoms reported

% No symptoms at all: special case
probability_level(none) :-
    \+ wm:has_symptom(_).

%--- Probability Assessment Output ---
diagnose_probability :-
    probability_level(Level),
    format('Your likelihood of Virus-like infection is: ~w~n', [Level]),
    advise_based_on_level(Level).

advise_based_on_level(high) :-
    write('HIGH probability: Please self-isolate immediately and consult a healthcare provider as soon as possible.'), nl.

advise_based_on_level(medium) :-
    write('MODERATE probability: Monitor your symptoms closely, limit contact with others, and consider contacting a healthcare provider.'), nl.

advise_based_on_level(low) :-
    write('LOW probability: Your symptoms are less likely due to Virus-like illness, but stay cautious and monitor your health.'), nl.

advise_based_on_level(none) :-
    write('NO symptoms reported: Risk of infection appears low at this time. Stay vigilant if you develop symptoms later.'), nl.

%--- Diagnostic Advice Rules (classic output, for compatibility) ---
diagnose :- wm:has_symptom(difficulty_breathing), !,
    write('Serious symptom detected: Difficulty breathing. Please seek immediate medical attention.'), nl.

diagnose :- wm:has_symptom(chest_pain), !,
    write('Serious symptom detected: Chest pain. Please contact a healthcare provider right away.'), nl.

diagnose :- wm:has_symptom(loss_of_speech_or_movement), !,
    write('Serious symptom detected: Loss of speech or movement. Call emergency services immediately.'), nl.

diagnose :-
    (wm:has_symptom(fever); wm:has_symptom(persistent_dry_cough); wm:has_symptom(tiredness)),
    kb:high_risk_overall, !,
    write('You have mild symptoms but belong to a higher risk group (age, pre-existing condition, or male in risk group).'), nl,
    write('Please consult your doctor as a precaution.'), nl,
    write('Stay home, isolate from others, wear a mask, and disinfect frequently touched surfaces (virus can persist for hours to days).'), nl,
    write('Avoid close contact and follow local health authority guidance.'), nl.

diagnose :-
    (wm:has_symptom(fever); wm:has_symptom(persistent_dry_cough); wm:has_symptom(tiredness)), !,
    write('You have mild symptoms. Please manage them at home: rest, hydrate, monitor your health.'), nl,
    write('Stay home and avoid close contact with others to prevent possible spread via droplets or contaminated surfaces.'), nl.

diagnose :- wm:has_symptom(runny_nose), !,
    write('Runny nose detected. Even if mild, stay home to avoid spreading to others via droplets or surfaces.'), nl,
    write('Practice strict hand hygiene and disinfect surfaces regularly.'), nl.

diagnose :-
    wm:patient_exposure(true),
    \+ wm:has_symptom(_), !,
    write('You do not have symptoms, but your recent exposure history should be monitored.'), nl,
    write('Continue to self-monitor and follow health authority advice for people with recent contact.'), nl.

diagnose :-
    write('No key symptoms detected.'), nl.
