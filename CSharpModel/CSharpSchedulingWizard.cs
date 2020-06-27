using System;
using System.Collections.Generic;
using System.Linq;

namespace QUT
{
    using StudyPlan = IEnumerable<UnitInPlan>;

    public class CSharpSchedulingWizard
    {
        public static Semester currentSemester
        {
            // Do not change
            get { return new Semester(2020, Offering.Semester1); }
        }

        private static StudyPlan scheduleRemaning(IEnumerable<PlannedUnit> remainingUnits, StudyPlan plannedUnits)
        {

            try
            {
                if (!remainingUnits.Any()) return plannedUnits;

                PlannedUnit schedulableUnit = 
                    remainingUnits.First(
                        unit => unit.possibleSemesters.Where(
                            sem => StudyPlannerModel.isEnrollableIn(unit.code, sem, plannedUnits)
                            ).Any()
                    );

                foreach (var sem in schedulableUnit.possibleSemesters)
                {
                    if (StudyPlannerModel.isEnrollableIn(schedulableUnit.code, sem, plannedUnits))
                    {
                        UnitInPlan newUnitPlan = new UnitInPlan(schedulableUnit.code, schedulableUnit.studyArea, sem);
                        StudyPlan scheduleEachSem = scheduleRemaning(remainingUnits.Where(unit => unit.code != schedulableUnit.code), plannedUnits.Concat(new UnitInPlan[] { newUnitPlan }));
                        if (scheduleEachSem != null)
                        {
                            return scheduleEachSem;
                        }
                    }
                }

            }catch (Exception ex) //if can't find any schedulable unit while there are still units needed to add to the plan
            {
                return null;
            }

            return null; //if can't add all the remaining units
        }

        private static Semester bestAchievable(StudyPlan plan)
        {
            int totalUnits = plan.Count();
            Offering nextOffering = getNextOffering(totalUnits, currentSemester.offering);
            return new Semester(currentSemester.year + (totalUnits-1)/8, nextOffering);
        }

        public static Offering getNextOffering(int totalUnits, Offering current)
        {
            int semesters = totalUnits % 4 == 0 ? totalUnits / 4 : (totalUnits / 4) + 1;
            if (semesters % 2 == 0)
            {
                if (current == Offering.Semester1)
                {
                    return Offering.Semester2;
                }
                else
                {
                    return Offering.Semester1;
                }
            }
            else
            {
                return current;
            }
        }

        public static IEnumerable<StudyPlan> TryToImproveSchedule(StudyPlan plan)
        {
            // TODO: Fixme
            Semester first = currentSemester;
            Semester last = FSharpSchedulingWizard.lastSemester(plan);
            Semester bestPossible = bestAchievable(plan);

            //variables for mutating to get the results using while loop
            IEnumerable<StudyPlan> bestPlans = new StudyPlan[] { };
            Semester targetGraduation = StudyPlannerModel.previousSemester(last);

            while (targetGraduation.CompareTo(bestPossible) >= 0)
            {
                IEnumerable<PlannedUnit> bounds = BoundsOptimizer.boundUnitsInPlan(plan, first, targetGraduation);
                StudyPlan newPlan = scheduleRemaning(bounds, new UnitInPlan[] { });

                if (newPlan == null) break; 

                //append the newPlan and try to get better plan with tighter target graduation semester
                bestPlans = bestPlans.Concat(new[] { newPlan });
                targetGraduation = StudyPlannerModel.previousSemester(FSharpSchedulingWizard.lastSemester(newPlan));
            }
            
            return bestPlans;
        }
    }
}