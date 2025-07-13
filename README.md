# COVID-like-Symptom-Diagnosis-Expert-System (Prolog)
This is a Mid-Module Assignment of CSCK502 Reasoning and Intelligence Systems June 2025

This project is an **interactive expert system for preliminary diagnosis and guidance related to COVID-like symptoms**, implemented in SWI-Prolog.
The system simulates a clinical interview, efficiently collecting patient data, reasoning about risk and symptom combinations, and giving medical recommendations and prevention tips.

## Architecture

The system follows a classic expert system structure:

1. **Knowledge Base:**
   Contains medical rules, symptom categories, and pre-existing condition definitions.

2. **Working Memory:**
   Stores facts about the current user/session (e.g., name, age, symptoms).

3. **Inference Engine:**
   The logic that applies knowledge to facts, manages the questioning flow, and matches facts to rules for risk and diagnosis.

4. **User Interface:**
   All user interactions (input/output), guiding the interview and presenting advice.

## Directory Structure

Once extracted, the main files typically include:

```
MMA/
├── knowledge_base.pl       # Medical/domain rules and symptom definitions
├── working_memory.pl       # Dynamic predicates for session memory
├── inference_engine.pl     # Core reasoning and flow logic
├── ui.pl                   # User interaction (CLI, input/output)
├── main.pl (optional)      # Main entry point module for the COVID-like Diagnosis Expert System.
├── README.md               # This documentation
```

> **Tip:** File names may differ. Please refer to the extracted directory for exact names.

## Files

* **knowledge\_base.pl** – Medical knowledge and rule definitions.
* **working\_memory.pl** – Session facts and memory management.
* **inference\_engine.pl** – Reasoning and question flow logic.
* **ui.pl** – User interaction, input/output, and session orchestration.
* **main.pl** – Entry point to load all modules and start the system.
* **(Optionally: advice.pl)** – Output routines for diagnosis and prevention advice.

## Features

* Structured, clinically-aligned symptom questioning (serious/common/less common).
* High-risk assessment (age, pre-existing conditions, gender).
* Exposure history and incubation period considerations.
* Prevention and public health advice tailored to user inputs.
* Modular code, easily extended for additional symptoms or rules.

## Running the System

You have two ways to start the system:

### **A. Requirement, Modular Load, and Run**

1. **Install [SWI-Prolog](https://www.swi-prolog.org/)** (if not already installed).

2. **Clone or download** this repository.

3. **Load all modules** in the SWI-Prolog console:

   ```prolog
   ?- [knowledge_base], [working_memory], [inference_engine], [ui].
   ```

4. **Start the expert system:**

   ```prolog
   ?- run_expert_system.
   ```

5. **Follow the prompts** to answer questions about your health and exposure.
   The system will present a summary, medical recommendations, and prevention tips.

### **B. Quick Start Using Main Module**

1. **Open SWI-Prolog and change to your project directory.**

2. **Load the main module:**

   ```prolog
   ?- use_module('main.pl').
   ```

   *(Or provide the full path if not in the current directory.)*

3. **Start the interactive expert system:**

   ```prolog
   ?- main.
   ```

   Follow the prompts to input your information and receive expert advice.

*Note: If your entry point file has a different name, substitute `main.pl` and `main` with the correct file and predicate.*

## Customisation

* To add or change symptoms, update `knowledge_base.pl`.
* To change question flow or reasoning, edit `inference_engine.pl`.
* For custom output or integration, modify `ui.pl` and `advice.pl`.

## Limitations & Disclaimer

* **Not a substitute for professional medical advice.**
  This system is for demonstration and educational purposes only.
* The logic is based on public clinical guidelines and may not cover all scenarios.
* For any severe symptoms or emergencies, **contact a healthcare provider immediately**.

## Contributors

* System design: \ Ting-Shao HO
* Logic and code: \ Ting-Shao HO
* Medical knowledge base: \ Ting-Shao HO, CSCK502 Reasoning and Intelligence Systems June 2025

---

Feel free to personalise, extend, or add more instructions as needed!






