% kb.pl
% ---------------------------
% Knowledge Base for Diagnosis Expert System
% Contains definitions for symptom categories,
% condition mappings, and risk factor rules.
% ---------------------------

:- module(kb, [
    symptom_category/2,
    cond_map/2,
    high_risk_age/0,
    high_risk_condition/0,
    high_risk_gender/0,
    high_risk_overall/0
]).

%--- Symptom Categories (Rule Definitions) ---
symptom_category(common,      [fever, persistent_dry_cough, tiredness]).
symptom_category(serious,     [difficulty_breathing, chest_pain, loss_of_speech_or_movement]).
symptom_category(less_common, [aches_and_pains, sore_throat, diarrhoea, conjunctivitis, headache, anosmia_or_hyposmia, runny_nose]).

%--- Map Menu Number to Condition Atom (Used by UI) ---
cond_map(1, hypertension).
cond_map(2, diabetes).
cond_map(3, cardiovascular_disease).
cond_map(4, chronic_respiratory_disease).
cond_map(5, cancer).

%--- High Risk Factors (Rules) ---
% These rules refer to facts in working memory (wm module)
high_risk_age        :- wm:patient_age(A), A > 70.
high_risk_condition  :- wm:patient_conditions(Conds), Conds \= [].
high_risk_gender     :- wm:patient_gender(male), (high_risk_age ; high_risk_condition).
high_risk_overall    :- high_risk_age ; high_risk_condition.
