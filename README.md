# MDS Approximation Project

This project provides an approximate solution to the Minimum Dominating Set (MDS) problem.

## Requirements

- **OCaml** and **Dune** (for building the project).
- **Z3** SMT solver, installed via [smtml](https://github.com/formalsec/smtml).
- You can install the dependencies using:
  ```bash
  opam install smtml z3
  ```

## Installation and Usage

1. **Clone or download** this repository.
2. **Install** all required dependencies mentioned above (Dune, smtml, Z3).
3. From the root directory of the repository, run:
   ```bash
   make run
4. After execution:
  - Visualisations are available in src/data
  - Statistics are printed in the terminal

## Report
- The report explaining the algorithmic details, design decisions, performance results, and benchmarks is available as `Rapport_projet_geo_algo.pdf` (written in French).
