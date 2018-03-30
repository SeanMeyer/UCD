using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApplication2
{
    class CustomPlayer
    {
        public int realSkill;
        private int pCurrentSkill;
        private double pCurrentDev;
        private Moserware.Skills.Rating pRating;
        public Moserware.Skills.Rating RealRating;
        public Moserware.Skills.Player PlayerObj;
        public List<int> skillHistory;
        public List<Moserware.Skills.Rating> ratingHistory;
        public int id;
        public int mean;
        public double stddev;

        public CustomPlayer(int realSkill, int mean, double stddev, int id, bool reset)
        {
            this.mean = mean;
            this.stddev = stddev;
            skillHistory = new List<int>();
            ratingHistory = new List<Moserware.Skills.Rating>();
            this.realSkill = realSkill;
            if (reset)
                pCurrentSkill = mean;
            else
                pCurrentSkill = realSkill;
            skillHistory.Add(pCurrentSkill);
            if (reset)
                pCurrentDev = stddev;
            else
                pCurrentDev = 50;
            PlayerObj = new Moserware.Skills.Player(0);
            pRating = new Moserware.Skills.Rating(pCurrentSkill, pCurrentDev);
            ratingHistory.Add(pRating);
            RealRating = new Moserware.Skills.Rating(realSkill, 50);
            this.id = id;
        }

        public int currentSkill
        {
            get { return pCurrentSkill; }
            set
            {
                skillHistory.Add(value);
                pCurrentSkill = value;
            }
        }

        public Moserware.Skills.Rating CurrentRating
        {
            get { return pRating; }
            set
            {
                ratingHistory.Add(value);
                this.pRating = value;
                currentSkill = (int)value.Mean;
            }
        }
    }
}
