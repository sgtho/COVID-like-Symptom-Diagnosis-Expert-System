% wm.pl
% ---------------------------
% Working Memory for Diagnosis Expert System
% Stores all patient/session facts dynamically.
% Provides utilities for cleaning/resetting session.
% ---------------------------

:- module(wm, [
    patient_name/1,
    patient_age/1,
    patient_gender/1,
    patient_conditions/1,
    exposure_days_ago/1,
    has_symptom/1,
    patient_exposure/1,
    clean_data/0
]).

:- dynamic
    patient_name/1,
    patient_age/1,
    patient_gender/1,
    patient_conditions/1,
    exposure_days_ago/1,
    has_symptom/1,
    patient_exposure/1.

%--- Utility: Clear All Facts Before New Session ---
clean_data :-
    retractall(patient_name(_)),
    retractall(patient_age(_)),
    retractall(patient_gender(_)),
    retractall(patient_conditions(_)),
    retractall(exposure_days_ago(_)),
    retractall(has_symptom(_)),
    retractall(patient_exposure(_)).
