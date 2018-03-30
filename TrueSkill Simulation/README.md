# Adversarial Search with Connect 4

The report on this project was created as a [PDF](https://github.com/SeanMeyer/UCD/blob/master/TrueSkill%20Simulation/Effect%20of%20Team%20Size%20on%20Skill%20Rating%20Convergence.pdf). Below is the introduction and setup, along with some example graphs of the results. View the PDF to see the entire project report, which includes results for a player joining an already established population.

## Introduction
The purpose of this simulation is to test how varying team sizes affects the number of games
needed for a player to be correctly rated by skill based, competitive, rating systems (e.g. Elo, Glicko,
TrueSkill). These systems attempt to quantify the skill level of a player in a multi-player competitive
environment, so that players can be matched against opponents of similar skill (to create the most
balanced games possible). While it's generally accepted that such systems work quite well in one
versus one scenarios (such as Chess or Go), it's less clear how larger team sizes affect these
systems. This simulation is an attempt to clarify that question.

## Simulation Setup
The simulation was performed using the Microsoft TrueSkilli algorithm, specifically an open source
library implementation by Jeff Moserii in the C# programming language. Because of this library,
which seemed to be well made and easy to use, I chose to use the C# language to do my simulation.
Using the library's implementation of TrueSkill, I generated player objects with two different skill
ratings: private and public. The public skill rating is the one that is the players TrueSkill rating
within the population, which moves after each game played, and the private skill rating is their
TrueSkill rating that represents their "actual" skill. These actual skills were used to determine
which team would win any given matchup, to simulate real players of differing skills playing each
other.

I generated several simulated players, assigning each a private skill rating by randomly
sampling a normal distribution (with a mean of 1700). This gave a population of players whose skill
was normally distributed, with many being average and a small number being very good or very
poor players.

The players were then matched up against other players, using the TrueSkill algorithm to try and
give the most even matches possible (closest to 50 percent win chance). This used the public skill
rating so the matches were not actually even at first, but they would become more and more even
as time went on (as the skill rating converged). This is how matching is actually done in modern
games, so this simulation was done the same way. If the team size being tested was not one versus
one, I matched each player with random teammates before finding opponents (once again creating the most even matches possible).

## Results (Full Reset Simulation)
A simulation on a population of 600 people, set to the same base starting MMR value. This particular simulation setup was run in response to [this video](https://www.youtube.com/watch?v=kB2UADNoRUA) which claims that 5v5 rating systems don't work. My results give a significantly different outlook than those presented in the video.

![Start of Simulation](https://i.imgur.com/lCrOzZM.png)
This is the graph of real skill versus the current MMR in the simulation. In the beginning, there are a variety of real skill levels, with most in the middle range, and all MMRs in the simulation are set to 1700. The coefficient of determination, or R^2, is 0 because there is no correlation between real skill and the MMR of the simulation.

![1v1 Simulation](https://i.imgur.com/P8Puw7R.png)
This is the result of running the simulation through 1v1 games, having the entire population play 100 games. I'd say the results are really good, with few outliers and the simulated MMR being determined very well by "real" MMR.

![5v5 Simulation 100 Games](https://i.imgur.com/ijoDHF5.png)
This is the same simulation as the 1v1 version above, but run on 5v5 games. The results are noticeably worse, but overall still relatively good.

![5v5 Simulation 1000 Games](https://i.imgur.com/2mcjjvd.png)
The 5v5 simulation run out to 1000 games, as in the original video. After the entire population has played 1k games, the results are even better (arguably) than the 1v1 simulation.