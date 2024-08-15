# Project 2: Tic Tac Toe CLI

## Software Requirements

### Basic Functionality:
- Users can play a game of Tic Tac Toe against another player (two-player mode).
- The game board should be displayed in the command line.
- Players should be able to place their mark (X or O) on the board by specifying the row and column.
- The game should detect and announce when a player has won or when the game ends in a draw.
- The game should allow players to restart the game after it ends.

### Advanced Features (Optional):
- Implement an AI opponent that players can play against.
- Allow players to choose their mark (X or O) before the game starts.
- Implement a scoring system that tracks wins, losses, and draws.
- Provide a command-line option to view the help menu during the game.

### User Interface:
- The game board should be clearly displayed after each move, with current player information.
- Display a help menu that explains how to make moves and restart the game.

### Error Handling:
- The game should prevent invalid moves (e.g., placing a mark in an already occupied cell or outside the grid).
- The application should handle input errors gracefully and prompt the user to try again.

### Code Structure:
- The code should be modular, separating concerns (e.g., game logic, user input, display).
- Follow best practices for Haskell, ensuring functions are pure and data types are well-defined.

## Acceptance Criteria:
- **Game Mechanics:**
  - The game must accurately detect win conditions and draws.
  - Players should be able to make moves without errors, and the board should update correctly.

- **Advanced Features (if implemented):**
  - The AI should make reasonable moves and provide a challenging opponent.
  - The scoring system should correctly track results and reset when requested.

- **User Interface:**
  - The game should display the board and instructions clearly.
  - The help menu should be accessible and informative.

- **Error Handling:**
  - Invalid moves should be prevented, and the user should be prompted to enter a valid move.
  - The game should not crash due to invalid input.

- **Code Quality:**
  - The code should be clean, modular, and well-documented.
  - Haskell best practices should be evident throughout the project.

## Rubric:

### Basic Functionality (40 points):
- Two-player mode (15 points)
- Board display and updating (10 points)
- Win/draw detection (10 points)
- Game restart option (5 points)

### Advanced Features (Optional - 20 points):
- AI opponent (10 points)
- Customizable player marks (5 points)
- Scoring system (5 points)

### User Interface (20 points):
- Board clarity and player information (10 points)
- Help menu (10 points)

### Error Handling (10 points):
- Prevention of invalid moves (5 points)
- Graceful handling of input errors (5 points)

### Code Quality (30 points):
- Modular design (10 points)
- Code cleanliness and readability (10 points)
- Use of Haskell best practices (10 points)

**Total: 120 points (100 if advanced features are not implemented)**
