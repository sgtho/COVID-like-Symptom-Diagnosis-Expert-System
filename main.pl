/**************************************************************************
 * main.pl
 * ------------------------------------------------------------------------
 * Main entry point module for the COVID-like Diagnosis Expert System.
 *
 * This file acts as the top-level "controller" for the entire system.
 * - Loads all other component modules (Knowledge Base, Working Memory,
 *   User Interface, and Inference Engine).
 * - Exports a single predicate `main/0` for launching the consultation.
 *
 * Usage:
 *   1. Start SWI-Prolog and change to this project directory. >>> use_module('~YOURPATH/main.pl').
 *   2. Start the expert system with:
 *        ?- main.
 * ------------------------------------------------------------------------
 * Architecture reference:
 *   - kb.pl     : Knowledge Base (rules, categories, mappings)
 *   - wm.pl     : Working Memory (dynamic session facts)
 *   - ui.pl     : User Interface (input/output, prompts, summaries)
 *   - engine.pl : Inference Engine (interview logic, diagnosis reasoning)
 **************************************************************************/

%----------------------
% Declare this file as a module named 'main'.
% Export the predicate main/0 to be callable from outside.
%----------------------
:- module(main, [main/0]).

%----------------------
% Import all subsystem modules. This ensures their exported predicates
% are available here, but does NOT pollute the global user namespace.
%----------------------
:- use_module('kb.pl').
:- use_module('wm.pl').
:- use_module('ui.pl').
:- use_module('engine.pl').

%----------------------
% main/0
% The system's entrypoint. Launches a new diagnosis session.
% This simply calls the 'run_interview/0' predicate in the engine module,
% which orchestrates all working memory, user interface, and rule processing.
% The system now infers and displays infection probability levels (high/medium/low/none)
% based on patient exposure, incubation timing, and symptoms, for more realistic advice.
%----------------------
main :- engine:run_interview.

%----------------------
% End of main.pl
%----------------------
