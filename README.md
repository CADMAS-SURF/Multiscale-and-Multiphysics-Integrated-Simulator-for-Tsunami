# Multiscale and Multiphysics Integrated Simulator for Tsunami
We develop a numerical fluid simulator coupled with a structural analysis. The purpose of this system is to efficiently calculate all stages of a tsunami from source to runup, including structural deformation. 
Specifically, a semi three-dimensional model (STOC-ML) assuming hydrostatic pressure is used from the source to the propagation of tsunami waves, and this is connected to the single-phase Navier-Stokes equation (CADMAS-SURF/3D), which calculates the sea surface using the VOF method, and then, in order to consider the effect of the gas phase, the single-phase VOF method is connected to the gas-liquid two-phase VOF method (CADMAS-2F) and finally to the structural and ground calculations (STR3D), which are calculated using FEM.
# Simulators
1. [**STOC-ML**](https://github.com/CADMAS-SURF/Multiscale-and-Multiphysics-Integrated-Simulator-for-Tsunami/tree/main/Simulators/STOC) (Hydrostatic model for calculating fluid motion due to tsunami)
2. [**STOC-IC**](https://github.com/CADMAS-SURF/Multiscale-and-Multiphysics-Integrated-Simulator-for-Tsunami/tree/main/Simulators/STOC) (Non-hydrostatic model for calculating fluid motion due to tsunami)
3. **CADMAS-SURF/3D** (Single-phase model for three-dimensional incompressible fluid)
4. **CADMAS-2F** (Gas-liquid two-phase model for three-dimensional incompressible fluid)
5. **STR3D** (FEM-based structural and soil calculation model)
6. **AGENT** (Evacuation simulator)
# Pre and post-processor
1. **CADMAS-MESH-MULTI** (STOC-ML and STOC-IC input data preparation support tool)
2. **CADMAS-MESH** (CADMAS-SURF/3D and CADMAS-2F input data preparation support tool)
3. **CADMAS-VR** (STOC-IC, STOC-ML, CADMAS-SURF/3D and CADMAS-2F calculation results visualisation tool)
4. **ViewKai** (CADMAS-SURF/3D and CADMAS-2F calculation results visualisation tool)
 
