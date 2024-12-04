# Fluid Simulation Project

## Overview

This project demonstrates a **2D fluid simulation** using **Smoothed Particle Hydrodynamics (SPH)**, implemented in Haskell. The simulation visualizes fluid dynamics, providing insight into the movement, pressure, and velocity of particles in real-time. Designed for both scientific exploration and aesthetic appeal, the simulation also includes interactive features to manipulate the behavior of the particles dynamically.

## Viewing the Output  
The output video of the simulation can be viewed on [YouTube](https://youtu.be/LiqdPfCDhqo) or by opening the output.mp4 file.  
  
**Windows users**: You can download the executable file and run the simulation directly without needing to set up the Haskell environment. Download the .exe file [here](https://github.com/LucaYan0506/Haskell-Animation/releases/download/V.1.0.1/installer.exe).  

**Apple users**: You can download the executable file and run the simulation directly without needing to set up the Haskell environment. Download the file [here](https://github.com/LucaYan0506/Haskell-Animation/releases/download/V.1.0.1.Mac/haskell-animation).  

**Linux users**: You can download the executable file and run the simulation directly without needing to set up the Haskell environment. Download the file [here](https://github.com/LucaYan0506/Haskell-Animation/releases/download/v1.0.2/haskell-animation.ubuntu) (only tested on Ubuntu 24.04.1 and ,once you downloaded the file, you need to run the following command ```chmod +x haskell-animation.ubuntu```).

---

## Features

### **Core Features**
- **Smooth Particle Motion**: Real-time fluid simulation with smooth transitions.
- **Dynamic Color Gradients**: Particle colors reflect velocity and pressure changes.

### **Interactive Controls**

| Input                     | Function                                                                                      |
|---------------------------|-----------------------------------------------------------------------------------------------|
| **Mouse Left Button (Hold)**   | Pull particles **away** from the pointer.                                                  |
| **Mouse Right Button (Hold)**  | Push particles **towards** the pointer.                                                    |
| **Key `p`**               | Apply a force that **stops all movement** (particles remain static but density increases).    |
| **Space Bar (Hold)**       | **Generate new particles** at the pointer's location.                                          |
| **Key `n`**                | **Generate a new particle** at the pointer's location.                                          |
| **Shift + `g`**     | Developer Mode: Display a **2D grid** and a **circle** around the pointer (interaction range). |
| **Key `c`**               | Change particle colors to represent **velocity** (helps visualize waves and motion patterns). |
| **Key `g`**               | Toggle **gravity** on or off.                                                                 |
| **Key `w`**               | Toggle **smooth particle** on or off.                                                                 |
| **Del Key**               | **Remove** every partcle.                                                                 |

---

## Running the Simulation

1. **Build the project**:
   ```bash
   cabal build
   ```
2. **Run the simulation**:
   ```bash
   cabal run
   ```


## Technical Details

### **Particle Interaction**
- **SPH Kernels**: Used for density estimation and force calculations.
- **Pressure & Viscosity Forces**: Realistic simulation of fluid behavior.

### Kernel Function for Density Calculation

The kernel function is used to calculate the density of particles based on their distance and influence. The influence of a particle decreases as the distance between particles increases, and particles outside the smoothing radius are ignored in the density calculation.

- **Input**: Distance between particles
- **Output**: Influence (used to modify density)
- **Formula**: Influence * Time * Mass = Density

The influence of a particle is calculated only for nearby particles that fall within a certain **smoothing radius**. Particles that lie outside this radius have no effect on the density at a given point. This allows the simulation to focus on local interactions, ensuring efficiency and accuracy.

Below is a visual representation of the kernel function and its behavior:

![Kernel Function](https://github.com/LucaYan0506/Haskell-Animation/blob/master/Kernel%20Function.png)

### Optimized Particle Storage and Neighborhood Search

To optimize the performance of the fluid simulation, particles are stored in a `Data.Vector` structure, which allows constant-time access to elements. 
- **Hashing**: For each particle, a hash value is computed based on its position in space. This value is then used as the index in the vector. By hashing the position, we can efficiently store particles in discrete "boxes" that cover the simulation area. Each particle's position corresponds to an index in the vector, and nearby particles can be grouped together within the same or neighboring indices.

- **Efficient Access**: Using `Data.Vector` allows for constant-time access to particles based on their index, making it much faster to retrieve particles for density and pressure calculations compared to iterating through all particles.

- **Localized Neighbor Search**: to calculate properties like pressure and density, we only need to check particles in close proximity to the current particle. This is achieved by checking the "boxes" (or hash values) surrounding the particle. Since particles are indexed by their hashed positions, we can quickly access a small subset of nearby particles, significantly reducing computational complexity. By limiting the neighborhood check to only the surrounding boxes, we ensure that only particles within the smoothing radius—the relevant range—are considered, thus optimizing performance and avoiding unnecessary calculations.

### **Control Mechanics**
- **Push/Pull Mechanics**: Particle forces depend on pointer proximity and mouse input.
- **Force Freeze (`p` key)**: freeze particle velocity while maintaining physical properties.
- **Dynamic Particle Generation**: Create particles dynamically during simulation using the **Space bar**.

## Future Improvements
- **Fluid Interaction Algorithms**: Implement better algorithms for fluid interaction, such as **Elasticity-Plasticity** interaction with rigid objects, to improve performance and stability.
- **3D Fluid Simulation**: Expand the simulation to 3D for more advanced fluid dynamics and realistic behaviors.
- **Enhanced Particle Color Gradients**: Refine the particle color gradient to represent more complex behaviors, such as temperature or pressure, to visualize different properties of the fluid.
- **GPU Parallel Processing**: Implement parallel processing using the GPU to speed up the simulation.

## Acknowledgments
This project draws inspiration from Sebastian Lague's Fluid Simulation Tutorial:
[Fluid Simulation Tutorial](https://www.youtube.com/watch?v=rSKMYc1CQHE&t=878s)

Special thanks for their insights into SPH algorithms and fluid dynamics visualization.
