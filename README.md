# MDS Approximation Project

This project provides an approximate solution to the Minimum Dominating Set (MDS) problem.

## Requirements

- **OCaml** and **Dune** (for building the project).
- **Z3** solver, installed via [smtml](https://github.com/formalsec/smtml).
  - For example, you can install them with:
    ```bash
    opam install smtml z3
    ```

## Installation and Usage

1. **Clone or download** this repository.
2. **Install** all required dependencies mentioned above (Dune, smtml, Z3).
3. From the root directory of the repository, run:
   ```bash
   make run
4. Once executed visualisations are available in src/data (the statistics of the solution are also given via the terminal)

## Report
- A report can be found explaining the details of the project (available in french)
