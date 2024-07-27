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
