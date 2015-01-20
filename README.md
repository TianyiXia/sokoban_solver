# sokoban_solver
This is a course assignment in CS 161 Artificial Inteligence. 
It is a sokoban solver. 
Main idea:
1. transform the game solving problem to a search probelm, by defining 
sucessor function and goal test. Then solve it by performing A* search.

2. Implement a trivial admissible herustic function that compute the number of misplaced boxes
Then solve it by performing A* search(given). 

3. Designed a better adimissible herustic function: 
My approach:
1. Pre-pruning: 
- If a box is stuck in a corner, it's dead, the real cost of moving it to target is infinity.
- 
2. calculate the sum of manhattan distance from each box to its nearest goal.
then calculate the sum of moves from player to each box, add them togather.

