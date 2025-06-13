# IG Simulation/Analysis Tool

## Description
This project appears to be a simulation or analysis tool, potentially for engineering or physics applications. It includes modules for pre-processing (defining domain, mesh, properties), solving (core computation), and post-processing (visualizing results like curves, planes, and heat distributions).

## Project Structure
The project is organized into several main directories:
*   `IG/PRE/`: Contains the pre-processing interface, likely used to define the model, geometry, mesh, and boundary conditions.
*   `IG/Solver/`: Contains the core solver logic, written in Fortran, which performs the main calculations.
*   `IG/POST/`: Contains the post-processing interface, used for visualizing and analyzing the results obtained from the solver.
*   `IG/IGS/`: Seems to be another interface or utility, possibly for specific calculations or settings.
*   `IG/Programas/`: Contains compiled executables, setup files, and supporting scripts/data files for running the application.

## Getting Started
To run the application, users would typically:
1.  Look for executable files (e.g., `Pre.exe`, `Solver.exe`, `post.exe`, `IGS.exe`) in the `IG/Programas/` directory.
2.  Alternatively, setup files are available in `IG/Programas/Setup/` and `IG/Programas/SetupWeb/` which might guide through an installation process.
*(Note: Actual execution steps may vary, this is inferred from the project structure.)*

## Language/Technology
*   The graphical user interface (GUI) for pre-processing, post-processing, and other utilities seems to be developed in **Visual Basic**.
*   The core solver component is written in **Fortran**.
