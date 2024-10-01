# Moneyball Capstone Project

## Overview
This project was created for the "Visualizing & Analyzing Data with R" course. It focuses on the Oakland Athletics' (OA) strategy to build a competitive baseball team despite a limited budget after losing key players in 2001.

## Introduction and Business Problem

### Situation:
In 2001, the Oakland Athletics lost key players Jason Giambi, Johnny Damon, and Jason Isringhausen. These players were critical to the team’s success, and their departure presented a significant challenge.

### Complication:
The OA team had a much lower budget ($33.8 million) than other Major League Baseball (MLB) teams, some of which had over $100 million. Replacing the lost players with high-profile, expensive alternatives made it difficult.

### Key Question:
How could the Oakland Athletics replace their key players while staying within their limited budget?

### Solution:
The team's strategy changed from using traditional player stats like Batting Average (BA) to a new metric called On Base Plus Slugging (OPS), which combines on-Base Percentage (OBP) and Slugging Average (SA). This provided a more accurate measure of a player’s overall impact on the game by examining their ability to get on base and hit with power.

## Methodology
We used an integer linear programming model to maximize the OPS metric while staying within the team’s budget. The goal was to find the best players with the highest OPS values that could be acquired without exceeding the available budget. Based on the model, the recommended players were:
- **Chris Michalak** from the Toronto Blue Jays
- **Barry Bonds** from the San Francisco Giants
- **Mike Stanton** from the New York Yankees

## Analysis
### Player Selection & Budget:
The selected players' combined salaries totalled $12.95 million, well within the team’s budget of $33.8 million. This showed that it was possible to stay under budget while still acquiring highly competitive players.

### OPS and Performance:
The chosen players had high OPS values, meaning they would likely strongly impact the team’s performance. This metric allowed the Oakland Athletics to evaluate players beyond traditional statistics, helping them identify undervalued talent.

### Replacement Strategy:
Using this strategy, the team could replace high-cost players with affordable ones with higher OPS values but were overlooked by other teams. This approach mirrored the famous Moneyball strategy developed by Billy Beane, the General Manager of the Oakland Athletics.

## Recommendations and Insights
- **Continue Using Advanced Metrics**: Metrics like OPS should continue to be used for player evaluation, as they give a more complete picture of a player’s value to the team.
- **Focus on Scouting and Development**: The team should invest in scouting to find undervalued players from lower leagues or other regions.
- **Adaptable Budgeting**: The team can stay competitive over time without overspending by adjusting the budget based on available talent.

## Conclusion
This project demonstrates how data analytics can be used to make smarter decisions in sports. By focusing on advanced metrics and using data-driven strategies, teams like the Oakland Athletics can remain competitive despite financial constraints. This approach can be a model for other teams and industries looking to optimize talent acquisition and resource allocation.

