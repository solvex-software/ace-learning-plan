# Solar System Plotter

## Objective
Develop an interactive web application using Haskell that displays a plot of our solar system. The application will retrieve planetary data from the Swiss Ephemeris, store it in a PostgreSQL database, and display it using a chart library on a Reflex-FRP client managed by Obelisk. The server-backend will be implemented using a Servant API to communicate data between the database and the frontend.

## Tools and Technologies
- **Frontend**: Reflex-FRP, Obelisk
- **Backend**: Haskell, Servant API
- **Database**: PostgreSQL with Persistent library
- **Data Source**: Swiss Ephemeris package
- **Visualization**: Haskell Chart library or any suitable Reflex-compatible charting library

## Project Components

### 1. Database Setup
- **Task**: Design and implement a PostgreSQL database schema using the Persistent library.
- **Objective**: Store planetary positions and related metadata fetched from the Swiss Ephemeris.

### 2. Data Retrieval and Storage
- **Task**: Integrate the Swiss Ephemeris package to fetch planetary data.
- **Objective**: Write Haskell scripts to periodically update the database with current planetary positions.

### 3. API Development
- **Task**: Develop a Servant API to handle requests between the frontend and the database.
- **Objective**: Create endpoints for fetching planetary data from the database and any other required interactions.

### 4. Frontend Development
- **Task**: Build a Reflex-FRP client using Obelisk to display the solar system.
- **Objective**: Utilize a chart library to plot the planets according to the data received from the API.

### 5. Integration and Testing
- **Task**: Integrate all components and test the application for functionality and performance.
- **Objective**: Ensure the system is robust, the data is accurate, and the user interface is responsive and intuitive.

## Expected Outcomes
- A fully functional solar system plotter that dynamically updates based on the real-time positions of planets.
- Understanding of how to build full-stack applications using Haskell, covering database management, server-side API construction, and reactive client-side programming.

## Learning Objectives
- Gain practical experience in functional programming with Haskell.
- Understand how to use Reflex-FRP for reactive web applications.
- Learn to integrate various components of a web application, from backend to frontend.
- Apply theoretical knowledge in databases and API development in a real-world project.
