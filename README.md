# BDIExplainer

Explainable BDI traces and plan choices using Prolog, with a Jason-based cleaning-robot example that is manually translated into Prolog traces.

## Overview
This repository contains:
- A Prolog explainer that derives explanatory factors (desires, context conditions, non-selected alternatives, etc.) for actions/goals in a BDI trace.
- Example domains, including a coffee/credential scenario and a cleaning-robot trace manually transcribed from a Jason run.
- Jason source for the cleaning-robot environment and agents used to produce the original trace.

## Repository layout
- `explainer/explain.pl`: Prolog explainer with example queries and a small coffee domain.
- `explainer/explainerEngine.pl`: Prolog explainer for instantiated goal-plan trees.
- `explainer/cleaningrobot.pl`: Cleaning-robot example using the instantiated tree representation.
- `explainer/explainerEngine2.pl`: Alternative explainer engine variant.
- `explainer/cleaningrobot2.pl`: Cleaning-robot example for the v2 engine.
- `explainer/output2.pl`: Sample output for the cleaning-robot v2 example.
- `jason/`: Jason MAS source (agents and environment) used to generate traces.

## Requirements
- SWI-Prolog for running the Prolog examples.
- Java + Jason (if you want to run the `jason` example to regenerate traces).

## Running the Prolog examples
From the repo root:

```bash
swipl -q -t go explainer/explain.pl
```

The cleaning-robot examples require loading the explainer engine plus the scenario file:

```bash
swipl -q -g "consult('explainer/explainerEngine.pl'), consult('explainer/cleaningrobot.pl'), go, halt"
```

The `go` predicate runs the included example queries and prints explanations to stdout.

## Jason example (trace generation)
The `jason` folder contains a small multi-agent system with a grid-world cleaning robot:
- `jason/mars.mas2j`: MAS configuration.
- `jason/MarsEnv.java`: environment and grid world.
- `jason/r1.asl`, `jason/r2.asl`: agent programs.

Run the MAS with your preferred Jason setup (IDE or CLI) to reproduce or modify the raw trace. The current Prolog trace files are created by hand from the Jason output; there is no automated Jason-to-Prolog translation in this repo.

## Notes
- Example outputs are included in `explainer/output.pl` and `jason/jason-output.txt`.
