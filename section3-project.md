### SpaceX Mars Supply Mission

**Problem Definition:**

Mars has sent a distress signal indicating they are without power and have a limited number of days of oxygen remaining. Mars has not yet reached sustainability and still relies on resources from Earth to survive. Due to a recent outage, the SpaceX C++ library is unable to calculate the necessary trajectories for the supply mission. 

**Objective:**

Create an application to calculate the correct trajectories for three supply ships, each with different payloads affecting their forces differently. Each ship must successfully deliver resources to Mars within the given constraints.

**Constraints:**

1. **Time Constraint**: Mars has \( x \) days of oxygen remaining.
2. **Ship Specifications**:
   - There are three ships with different payloads.
   - The forces affecting each ship vary due to their different payloads.
3. **Trajectory Calculation**:
   - The application must calculate the trajectory for each ship to ensure successful delivery.

**Requirements:**

1. **Functional Programming**: Use functional programming principles to create the application.
2. **Pure Functions**: Ensure all calculations are performed using pure functions without side effects.
3. **State Management**: Handle state immutably to avoid issues similar to those experienced with the SpaceX C++ library.

**Deliverables:**

1. An application that can:
   - Calculate the correct trajectory for each of the three ships.
   - Ensure that each ship arrives within the \( x \) days constraint.
2. Documentation explaining the approach and functional programming principles used.




challenges:
- must divide the payloads among the ships so they have enough gas to make it
- must account for gravitational pull of nearby stars along the route
- must account for different ship weights being sent to the planet
- must account for the orbit of earth and mars, as well as where the ship is being launched from
- must connect with mars orbit so the rocket doesnt just nuke mars and kill everyone on it
- must avoid a few asteroid fields that passes through where we need to go once every day.









SpaceX Mars Supply Mission Simulation Setup
Simulation Overview:

To build this program and guide your students effectively, you'll need to create a simulation environment that provides all the necessary information about orbits, launch pad coordinates, planetary positions, and trajectories. This environment will help students apply the given formulas and implement the specified details into a robust application.

Key Variables and Information:

Planetary Orbits:
- Earth’s orbit radius (𝑟1): 1 AU (Astronomical Unit = 1.496 × 10^8km)
- Mars’ orbit radius (𝑟2): 1.524 AU

Launch Pad Coordinates:
- Earth Launch Pad: Latitude: 28.5623° N, Longitude: 80.5774° W (Kennedy Space Center)
Assume the launch happens from the surface, with initial velocity calculated based on Earth's rotation.

Planetary Positions and Trajectories:
Initial position of Earth and Mars at the time of launch.
Use Kepler's laws to simulate planetary positions over time.
Provide ephemeris data for the positions of Earth and Mars at regular intervals (e.g., daily).

Gravitational Constants:
- Gravitational constant (𝐺): 6.67430 × 10^(-11)m^3 kg^(-1) s^(-2)
- Mass of the Sun (𝑀): 1.989 × 10^30 kg

Ship Specifications:
- Ship A: Mass = 50,000 kg, Payload = 20,000 kg
- Ship B: Mass = 55,000 kg, Payload = 22,000 kg
- Ship C: Mass = 60,000 kg, Payload = 25,000 kg
Fuel capacity and efficiency to be provided as variables to each student.

Asteroid Field Coordinates and Timing:
TODO: Define coordinates and paths of asteroid fields.
TODO: Provide time intervals when asteroid fields pass through the mission route.



TODO: Simulation Setup:


Ephemeris Data Generation:
Use NASA’s SPICE toolkit or similar tools to generate ephemeris data for Earth and Mars.
Provide the data in a format accessible to the students (e.g., CSV files) ** use an API if possible
- https://www.astro.com/swisseph/swephinfo_e.htm
- https://github.com/lfborjas/swiss-ephemeris?tab=readme-ov-file#readme

Trajectory Calculation Functions:
Implement functions for Hohmann transfer, gravitational pull calculations, and orbital mechanics.
Provide students with skeleton code and helper functions to focus on implementing core logic.

State Management:
Use immutable data structures to handle state changes.
Provide examples of how to manage state immutably in a functional programming context.

Time Series Simulation:
Implement a simulation loop that progresses in time steps (e.g., daily).
Update positions, velocities, and other state variables at each time step.

Sample Code Structure:
```haskell
-- Define constants
gravitationalConstant :: Double
gravitationalConstant = 6.67430e-11

massOfSun :: Double
massOfSun = 1.989e30

-- Define data structures
data Planet = Planet {
    name :: String,
    mass :: Double,
    orbitRadius :: Double,
    position :: (Double, Double),  -- (x, y) coordinates
    velocity :: (Double, Double)   -- (vx, vy) velocity components
}

data Ship = Ship {
    shipId :: String,
    mass :: Double,
    payload :: Double,
    fuelCapacity :: Double,
    position :: (Double, Double),
    velocity :: (Double, Double)
}

-- Example of a pure function to calculate gravitational force
gravitationalForce :: Double -> Double -> Double -> Double
gravitationalForce m1 m2 distance = gravitationalConstant * m1 * m2 / distance^2

-- Example of a Hohmann transfer function
hohmannTransfer :: Planet -> Planet -> Double
hohmannTransfer earth mars = deltaV1 + deltaV2
  where
    r1 = orbitRadius earth
    r2 = orbitRadius mars
    mu = gravitationalConstant * massOfSun
    deltaV1 = sqrt(mu / r1) * (sqrt(2 * r2 / (r1 + r2)) - 1)
    deltaV2 = sqrt(mu / r2) * (1 - sqrt(2 * r1 / (r1 + r2)))

-- Main simulation loop
simulateMission :: [Planet] -> [Ship] -> Int -> [(Planet, Ship)]
simulateMission planets ships days = -- implement simulation logic

-- Example of running the simulation
main :: IO ()
main = do
    let earth = Planet "Earth" 5.972e24 1.496e8 (0, 0) (0, 29.78)
        mars = Planet "Mars" 0.64171e24 2.279e8 (0, 0) (0, 24.07)
        shipA = Ship "ShipA" 50000 20000 100000 (0, 0) (0, 0)
        shipB = Ship "ShipB" 55000 22000 110000 (0, 0) (0, 0)
        shipC = Ship "ShipC" 60000 25000 120000 (0, 0) (0, 0)
        planets = [earth, mars]
        ships = [shipA, shipB, shipC]
        days = 100

    let result = simulateMission planets ships days
    print result
```













