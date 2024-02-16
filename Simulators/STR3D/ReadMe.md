# STR-3D
**Outline of Functions**
1. Analysis type: Direct transient response analysis (static analysis is used initially)
2. Nonlinear analysis function: Geometric nonlinear (Total Lagrange method) Material nonlinear (elastoplastic) contact (static friction, dynamic friction)
3. Element library: Tetrahedron, pentahedron, hexahedron, each first-order and second-order element, beam element (rectangular cross section)
4. Special materials: Ground (based on Biot's formula), masonry materials
5. Matrix solver: Multifrontal method, domain-divided CG method (both can be calculated in parallel)
# How to download and compile STR3D
STR3D source code is stored in the in the Sourse code folder. You can download all the files by clicking on "Code" (green button on the top-right corner) and then select "Download ZIP".
# Coupled analysis with CADMAS-2F
Supports coupling with [CADMAS-2F](https://github.com/CADMAS-SURF/Multiscale-and-Multiphysics-Integrated-Simulator-for-Tsunami/tree/main/Simulators/CADMAS-2F). You can find more details in the manual.
# Recommended pre and post-processors
+ Femap
# Libraries required to build STR
+ MUMPS
+ METIS
+ scalapack
+ BLAS
# Important information about STR3D
+ License
+ Main Features
+ Get in touch
+ Documentation
# If you want to know more about STR3D, go back to Home
