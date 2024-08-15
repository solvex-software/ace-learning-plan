# Project 3: Simple Twitter Clone (Frontend Focus)

## Software Requirements

### Basic Functionality:
- Users can sign up and log in to their accounts.
- Users can post tweets (short text posts).
- Users can view a timeline of tweets from all users.
- Users can view individual user profiles, including their tweets.
- Users can log out of their accounts.

### Advanced Features (Optional):
- Implement a "like" feature for tweets.
- Allow users to follow other users, and display a personalized timeline based on who they follow.
- Implement basic error messages (e.g., when a user tries to tweet without being logged in).

### User Interface:
- The frontend should be minimal but clear, providing easy navigation between the timeline, profiles, and tweet posting.
- Display relevant buttons for actions like tweeting, viewing profiles, and logging out.

### Error Handling:
- Ensure users cannot perform actions like posting a tweet without being logged in.
- Provide meaningful error messages for common issues (e.g., invalid login credentials).

### Code Structure:
- The code should be organized into modules separating concerns (e.g., authentication, tweet management, UI).
- Use a framework like Reflex-FRP for reactive front-end programming, if applicable.

## Acceptance Criteria:
- **Core Features:**
  - Users should be able to create accounts, log in, post tweets, and view timelines.
  - The timeline should update correctly with new tweets.

- **Advanced Features (if implemented):**
  - The "like" feature should work consistently.
  - The following system should correctly filter the timeline based on followed users.

- **User Interface:**
  - The UI should be clear, with logical navigation and correctly displayed tweets.
  - User profiles should show accurate information and tweets.

- **Error Handling:**
  - The app should prevent unauthorized actions and provide clear error messages.

- **Code Quality:**
  - The code should be modular and maintainable.
  - Best practices in both Haskell and frontend development should be followed.

## Rubric:

### Basic Functionality (40 points):
- Account creation and login (10 points)
- Tweet posting (10 points)
- Timeline display (10 points)
- User profiles (10 points)

### Advanced Features (Optional - 20 points):
- "Like" feature (10 points)
- Following system (10 points)

### User Interface (20 points):
- UI clarity and navigation (10 points)
- Profile and tweet display (10 points)

### Error Handling (10 points):
- Prevention of unauthorized actions (5 points)
- User-friendly error messages (5 points)

### Code Quality (30 points):
- Modular design (10 points)
- Code cleanliness and readability (10 points)
- Use of Haskell and frontend best practices (10 points)

**Total: 120 points (100 if advanced features are not implemented)**
