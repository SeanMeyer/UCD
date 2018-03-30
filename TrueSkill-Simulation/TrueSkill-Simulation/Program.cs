using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Moserware.Skills;
using Moserware.Skills.Numerics;
using System;
using System.Diagnostics;

namespace ConsoleApplication2
{
    class Program
    {
        static Random R = new Random();
        static double mean = 1700;
        static double stddev = mean / 3.0;
        static double beta = mean / 6.0;
        static double dynamicFactor = mean / 300.0;
        static double drawProbability = 0.0;
        static int population = 1000;
        static List<CustomPlayer> players;
        static GameInfo gameInfo;

        static void Main(string[] args)
        {
            gameInfo = new GameInfo(mean, stddev, beta, dynamicFactor, drawProbability);
            players = GeneratePlayers(mean, stddev, population, false);
            double convergeThreshold = 0.85;
            int convergedGames = 5;

            // ##### Simulations for how team size effects time to reach real rating #####

            // Simulations for very highly rated player (3 std deviations away, top 0.3%)
            //NewPlayerRunAndOutput(3400, 1, convergeThreshold, convergedGames);
            NewPlayerRunAndOutput(2150, 5, convergeThreshold, convergedGames);
            NewPlayerRunAndOutput(3400, 10, convergeThreshold, convergedGames);

            // Simulations for an average rated player (at the mean)
            CustomPlayer avgRated1v1 = NewPlayerSimulation(1700, 1);
            var test4 = GamesTillConvergence(avgRated1v1, .90, 10);
            CustomPlayer avgRated5v5 = NewPlayerSimulation(1700, 5);
            var test5 = GamesTillConvergence(avgRated5v5, .90, 10);
            CustomPlayer avgRated10v10 = NewPlayerSimulation(1700, 10);
            var test6 = GamesTillConvergence(avgRated10v10, .90, 10);

            // ##### Full reset simulations #####
            List<CustomPlayer> resetPlayers1v1 = FullResetSimulation(1);
            List<CustomPlayer> resetPlayers5v5 = FullResetSimulation(5);


            var team1Cur = new Team(players[0].PlayerObj, players[0].CurrentRating);
            var team2Cur = new Team(players[1].PlayerObj, players[1].CurrentRating);
            var teamsCur = Teams.Concat(team1Cur, team2Cur);

            var team1Real = new Team(players[0].PlayerObj, players[0].RealRating);
            var team2Real = new Team(players[1].PlayerObj, players[1].RealRating);
            var teamsReal = Teams.Concat(team1Real, team2Real);
            var newRatings = TrueSkillCalculator.CalculateNewRatings(gameInfo, teamsCur, 1, 2);
            var team1WinChance = Team1WinChance(teamsReal, gameInfo);

            for (int i = 0; i < 500; i++)
            {
                if (R.NextDouble() < team1WinChance)
                    // team 1 wins
                    newRatings = TrueSkillCalculator.CalculateNewRatings(gameInfo, teamsCur, 1, 2);
                else
                    // team 2 wins
                    newRatings = TrueSkillCalculator.CalculateNewRatings(gameInfo, teamsCur, 2, 1);

                players[0].CurrentRating = newRatings[players[0].PlayerObj];
                players[1].CurrentRating = newRatings[players[1].PlayerObj];
                team1Cur = new Team(players[0].PlayerObj, players[0].CurrentRating);
                team2Cur = new Team(players[1].PlayerObj, players[1].CurrentRating);
                teamsCur = Teams.Concat(team1Cur, team2Cur);
            }

        }

        
        static List<CustomPlayer> GeneratePlayers(double mean, double stddev, int numberOfPlayers, bool reset)
        {
            int randomGauss; 
            List<CustomPlayer> players = new List<CustomPlayer>();
            MathNet.Numerics.Distributions.Normal normalDist = new MathNet.Numerics.Distributions.Normal(mean, stddev);

            for (int i = 0; i < numberOfPlayers; i++)
            {
                randomGauss = (int)normalDist.Sample();
                CustomPlayer aPlayer = new CustomPlayer(randomGauss, (int)mean, stddev, i, reset);
                players.Add(aPlayer);
            }

            return players;
        }

        static List<CustomPlayer> CopyPlayerPool(List<CustomPlayer> players, bool reset)
        {
            List<CustomPlayer> newPlayers = new List<CustomPlayer>();

            foreach (var player in players)
            {
                CustomPlayer aPlayer = new CustomPlayer(player.realSkill, player.mean, player.stddev, player.id, reset);
                newPlayers.Add(aPlayer);
            }

            return newPlayers;
        }

        static double Team1WinChance(IEnumerable<IDictionary<Player, Rating>> teams, GameInfo gameInfo)
        {
            // Math copied from a comment by Jeff Moser on his blog: http://disq.us/p/vreqf0
            double drawMargin = GetDrawMarginFromDrawProbability(gameInfo.DrawProbability, gameInfo.Beta);

            IDictionary<Player, Rating> team1 = teams.First();
            IDictionary<Player, Rating> team2 = teams.Last();

            int totalPlayers = team1.Count() + team2.Count();

            double team1MeanSum = team1.Values.Sum(r => r.Mean);
            double team2MeanSum = team2.Values.Sum(r => r.Mean);

            double team1WinNumerator = team1MeanSum - team2MeanSum - drawMargin;

            double team1SigmaSquaredSum = team1.Values.Sum(r => (r.StandardDeviation * r.StandardDeviation));
            double team2SigmaSquaredSum = team2.Values.Sum(r => (r.StandardDeviation * r.StandardDeviation));

            double denominator = Math.Sqrt(totalPlayers * (gameInfo.Beta * gameInfo.Beta) +
                team1SigmaSquaredSum + team2SigmaSquaredSum);

            return Moserware.Numerics.GaussianDistribution.CumulativeTo(team1WinNumerator / denominator);
        }

        public static double GetDrawMarginFromDrawProbability(double drawProbability, double beta)
        {
            // Derived from TrueSkill technical report (MSR-TR-2006-80), page 6

            // draw probability = 2 * CDF(margin/(sqrt(n1+n2)*beta)) -1

            // implies
            //
            // margin = inversecdf((draw probability + 1)/2) * sqrt(n1+n2) * beta
            // n1 and n2 are the number of players on each team
           
            double margin = Moserware.Numerics.GaussianDistribution.InverseCumulativeTo(
                .5 * (drawProbability + 1), 0, 1) * Math.Sqrt(1 + 1) * beta;
            return margin;
        }

        public static void PlayGame(List<CustomPlayer> team1Players, List<CustomPlayer> team2Players, GameInfo gameInfo)
        {
            // Create teams based on current rating
            var team1Cur = new Team();
            var team2Cur = new Team();
            foreach (var player in team1Players)
                team1Cur.AddPlayer(player.PlayerObj, player.CurrentRating);
            foreach (var player in team2Players)
                team2Cur.AddPlayer(player.PlayerObj, player.CurrentRating);
            var teamsCur = Teams.Concat(team1Cur, team2Cur);

            // Create teams based on real rating
            var team1Real = new Team();
            var team2Real = new Team();
            foreach (var player in team1Players)
                team1Real.AddPlayer(player.PlayerObj, player.RealRating);
            foreach (var player in team2Players)
                team2Real.AddPlayer(player.PlayerObj, player.RealRating);
            var teamsReal = Teams.Concat(team1Real, team2Real);

            // Calculate win chance using real rating
            var team1WinChance = Team1WinChance(teamsReal, gameInfo);

            // Get updated ratings based on who won (using random number and win chance)
            IDictionary<Player, Rating> newRatings;
            if (R.NextDouble() < team1WinChance)
                // team 1 wins
                newRatings = TrueSkillCalculator.CalculateNewRatings(gameInfo, teamsCur, 1, 2);
            else
                // team 2 wins
                newRatings = TrueSkillCalculator.CalculateNewRatings(gameInfo, teamsCur, 2, 1);

            // Update the player objects with new ratings
            foreach (var player in team1Players)
                player.CurrentRating = newRatings[player.PlayerObj];
            foreach (var player in team2Players)
                player.CurrentRating = newRatings[player.PlayerObj];
        }

        public static List<CustomPlayer> GetTeam1(List<CustomPlayer> players, int teamSize)
        {
            List<CustomPlayer> team1 = new List<CustomPlayer>();

            /*
            for (int i = 0; i < teamSize; i++)
            {
                team1.Add(players[0]);
                players.RemoveAt(0);
            }*/

            team1.Add(players[0]);
            players.RemoveAt(0);
            for (int i = 0; i < teamSize - 1; i++)
            {
                team1.Add(players[R.Next(0, players.Count())]);
            }
            return team1;
        }

        public static List<CustomPlayer> GetTeam2(List<CustomPlayer> players, List<CustomPlayer> team1Players, GameInfo gameInfo)
        {
            List<CustomPlayer> team2Players = new List<CustomPlayer>();

            var team1 = new Team();
            var team2 = new Team();
            IEnumerable<IDictionary<Player, Rating>> teams;
            CustomPlayer bestMatchedPlayer = players[0];
            double bestMatchQuality, matchQuality;

            // We're going to match player by player
            foreach (CustomPlayer team1Player in team1Players)
            {
                // Set up this player as a team
                team1 = new Team(team1Player.PlayerObj, team1Player.CurrentRating);
                bestMatchQuality = 0.0;

                // Search all other players
                foreach (CustomPlayer opponent in players)
                {
                    team2 = new Team(opponent.PlayerObj, opponent.CurrentRating);
                    teams = Teams.Concat(team1, team2);
                    matchQuality = TrueSkillCalculator.CalculateMatchQuality(gameInfo, teams);
                    if (matchQuality > bestMatchQuality)
                    {
                        bestMatchQuality = matchQuality;
                        bestMatchedPlayer = opponent;
                    }
                    if (bestMatchQuality >= .90)
                        break;
                }
                team2Players.Add(bestMatchedPlayer);
                players.Remove(bestMatchedPlayer);
            }

            return team2Players;
        }

        public static CustomPlayer NewPlayerSimulation(int realRating, int teamSize)
        {
            List<CustomPlayer> myPlayers = CopyPlayerPool(players, false);

            myPlayers.RemoveAt(0);
            myPlayers.Insert(0, new CustomPlayer(1900, (int)mean, stddev, 0, true));

            List<CustomPlayer> availablePlayers = myPlayers.ToList();

            List<CustomPlayer> team1 = new List<CustomPlayer>();
            List<CustomPlayer> team2 = new List<CustomPlayer>();

            for (int i = 0; i < 500; i++)
            {
                team1 = GetTeam1(availablePlayers, teamSize);
                team2 = GetTeam2(availablePlayers, team1, gameInfo);
                PlayGame(team1, team2, gameInfo);
                availablePlayers = myPlayers.ToList();
            }

            myPlayers[0].realSkill = realRating;
            myPlayers[0].RealRating = new Moserware.Skills.Rating(realRating, 50);
            //myPlayers[0].CurrentRating = new Moserware.Skills.Rating(myPlayers[0].currentSkill, 566);
            availablePlayers = myPlayers.ToList();
            team1 = new List<CustomPlayer>();
            team2 = new List<CustomPlayer>();

            for (int i = 0; i < 2000; i++)
            {
                team1 = GetTeam1(availablePlayers, teamSize);
                team2 = GetTeam2(availablePlayers, team1, gameInfo);
                PlayGame(team1, team2, gameInfo);
                availablePlayers = myPlayers.ToList();
            }

            return myPlayers[0];
        }

        public static CustomPlayer CurPlayerSimulation(CustomPlayer curPlayer, int realRating, int teamSize)
        {
            List<CustomPlayer> myPlayers = CopyPlayerPool(players, false);

            myPlayers.RemoveAt(0);
            myPlayers.Insert(0, new CustomPlayer(realRating, (int)mean, stddev, 0, true));

            List<CustomPlayer> availablePlayers = myPlayers.ToList();

            List<CustomPlayer> team1 = new List<CustomPlayer>();
            List<CustomPlayer> team2 = new List<CustomPlayer>();

            for (int i = 0; i < 300; i++)
            {
                team1 = GetTeam1(availablePlayers, teamSize);
                team2 = GetTeam2(availablePlayers, team1, gameInfo);
                PlayGame(team1, team2, gameInfo);
                availablePlayers = myPlayers.ToList();
            }

            return myPlayers[0];
        }

        public static List<CustomPlayer> FullResetSimulation(int teamSize)
        {
            List<CustomPlayer> myPlayers = CopyPlayerPool(players, true);
            List<CustomPlayer> availablePlayers = myPlayers.ToList();

            List<CustomPlayer> team1 = new List<CustomPlayer>();
            List<CustomPlayer> team2 = new List<CustomPlayer>();

            Debug.Assert(availablePlayers.Count() % teamSize == 0);

            for (int i = 0; i < 100; i++)
            {
                while (availablePlayers.Count() > 0)
                {
                    team1 = GetTeam1(availablePlayers, teamSize);
                    team2 = GetTeam2(availablePlayers, team1, gameInfo);
                    PlayGame(team1, team2, gameInfo);
                }
                availablePlayers = myPlayers.ToList();
            }

            return myPlayers;
        }

        public static int GamesTillConvergence(CustomPlayer player, double qualityRequirement, int consecutive)
        {
            int consecutiveWithinRequirement = 0, count = 0;

            foreach (Rating rating in player.ratingHistory)
            {
                count++;
                if (count > 500)
                {
                    var player1History = new Team(player.PlayerObj, rating);
                    var player1Real = new Team(player.PlayerObj, player.RealRating);
                    var teams = Teams.Concat(player1History, player1Real);
                    var quality = TrueSkillCalculator.CalculateMatchQuality(gameInfo, teams);

                    if (quality >= qualityRequirement)
                        consecutiveWithinRequirement++;
                    if (consecutiveWithinRequirement == consecutive)
                        break;
                }
            }

            if (consecutiveWithinRequirement == consecutive)
                return count - consecutive - 500;
            else
                return count - 500;
        }

        public static void NewPlayerRunAndOutput(int mmr, int teamsize, double convergeThreshold, int convergedGames)
        {
            CustomPlayer simulatedPlayer = NewPlayerSimulation(mmr, teamsize);
            string[] lines = simulatedPlayer.skillHistory.Select(i => i.ToString()).ToArray();
            string path = @"F:\Dropbox\UCD\Simulation\Project Data\Simulated MMR " + mmr.ToString() + " - Team Size " + teamsize.ToString() + ".txt";
            System.IO.File.WriteAllLines(path, lines);
            List<int> gamesToConverge = new List<int>();

            Parallel.For(0, 500,
                i =>
                {
                    CustomPlayer simPlayer = NewPlayerSimulation(mmr, teamsize);
                    gamesToConverge.Add(GamesTillConvergence(simPlayer, convergeThreshold, convergedGames));
                });

            /*
            for (int i = 0; i < 440; i++)
            {
                simulatedPlayer = NewPlayerSimulation(mmr, teamsize);
                gamesToConverge.Add(GamesTillConvergence(simulatedPlayer, convergeThreshold, convergedGames));
            } */
            lines = gamesToConverge.Select(i => i.ToString()).ToArray();
            path = @"F:\Dropbox\UCD\Simulation\Project Data\Monte Carlo " + mmr.ToString() + " - Team Size " + teamsize.ToString() + ".txt";
            System.IO.File.WriteAllLines(path, lines);
        }
    }
}
