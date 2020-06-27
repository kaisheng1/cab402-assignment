module FSharpSchedulingWizard

open QUT
open StudyPlannerModel
open BoundsOptimizer

// Functions used for optimizing study plan ...

// The semester that we are currently in
let currentSemester : Semester = 
    // Do not change
    { year = 2020; offering = Semester1 }


// Given a partial plan, try to schedule the remaining units such that all remaining units are legally scheduled 
// (with no more than 4 units per semester) .
// Should return None if it is not possible to schedule the remaining units
// We start by selecting one of the remaining units that can be scheduled in at least one of its possible semesters.
// If none of the remaining units can be scheduled then we fail.
// Otherwise we try scheduling that unit in each of the possible semesters in which it can be legally scheduled. 
// If any of those schedules can be extended into a complete plan then we succeed, otherwise we fail.
let rec private scheduleRemaining (remainingUnits:BoundPlan) (plannedUnits:StudyPlan): StudyPlan option =
    // TODO: Fixme (difficulty: 10/10)
    let addToPlan (unitInPlan:UnitInPlan) plan = Seq.append plan (Seq.singleton unitInPlan)
    let schedulable unit = unit.possibleSemesters |> Seq.exists (fun sem -> isEnrollableIn unit.code sem plannedUnits)
    let getPlan (unit: PlannedUnit) enrollableSem =
        let unitInPlan = {code=unit.code;studyArea=unit.studyArea;semester=enrollableSem}
        let planWithUnit = addToPlan unitInPlan plannedUnits
        scheduleRemaining (List.filter (fun x -> x <> unit) remainingUnits) planWithUnit
    let getCompletePlan unit bestPlan sem : StudyPlan option = 
        if (Option.isNone bestPlan) then 
            if (isEnrollableIn unit.code sem plannedUnits) then getPlan unit sem
            else None 
        else bestPlan
    
    if (List.isEmpty remainingUnits) then Some(plannedUnits) //if all units are added to the plan
    else 
        match List.tryFind schedulable remainingUnits with 
        | Some(unit) -> 
            unit.possibleSemesters
            |> Seq.fold (getCompletePlan unit) None
        | None -> None




// Assuming that study commences in the given first semester and that units are only studied 
// in semester 1 or semester 2, returns the earliest possible semester by which all units in
// the study plan could be completed, assuming at most 4 units per semester.
let private bestAchievable (firstSemester:Semester) (plan:StudyPlan) : Semester =
     // TODO: Fixme (difficulty: 5/10)
     let totalUnits = Seq.length plan

     let nextOffering = 
        let semesters = if totalUnits % 4 = 0 then totalUnits / 4 else (totalUnits / 4) + 1 //self implementation of Math.floor

        if (semesters % 2 = 0) then 
            match firstSemester.offering with
            | Semester1 -> Semester2
            | _ -> Semester1 
        else 
            firstSemester.offering
            

     {year = firstSemester.year + (totalUnits-1)/8; offering = nextOffering}
        

// Returns the last semester in which units will be studied in the study plan
let lastSemester (plan: StudyPlan): Semester =
     // TODO: Fixme (difficulty: 3/10)
    let getLatest (acc: UnitInPlan) (elem: UnitInPlan) = 
        if (elem.semester > acc.semester) then elem
        else acc

    plan
    |> Seq.reduce getLatest
    |> (fun unitPlan -> unitPlan.semester)



// Returns true if and only if every unit in the plan has at least one possible semester for it to be scheduled
let allBoundsFeasible (bounds:BoundPlan) =
    // do not change  (difficulty: 3/10)
    bounds |> Seq.forall (fun unit -> not (Seq.isEmpty unit.possibleSemesters)) 

// Returns a sequence of progressively better study plans.
// Each successive plan returned finishes in an earlier semester than the previous plan.
// Should return the empty sequence if the plan cannot be improved.
// The earliest semester that we can schedule units in is the current semester.
// Successively better plans are created by specifying progressively tighter target graduation semesters.
// We determine the final semester of the current plan and set our target semester as the semester before that.
// If we succeed in finding a plan that completes by that target semester then we try to improve that plan further, 
// semester by semester until it becomes impossible to improve further.
let TryToImproveSchedule (plan:StudyPlan) : seq<StudyPlan> =
    let first = currentSemester
    let last = lastSemester plan
    let bestPossible = bestAchievable first plan
    let rec TryToCompleteBy (targetGraduation:Semester) =
        // TODO: Fixme (hint: use scheduleRemaining function) (difficulty: 8/10)
        if (targetGraduation < bestPossible) then 
            Seq.empty
        else 
            let bounds = boundUnitsInPlan plan first targetGraduation
            if (allBoundsFeasible bounds) then 
                let newPlan = scheduleRemaining bounds Seq.empty //try to get a new plan with all the units
                match newPlan with 
                    | Some(studyPlan) -> 
                        seq {
                            yield studyPlan
                            yield! TryToCompleteBy (previousSemester (lastSemester studyPlan))
                        }
                    | None -> Seq.empty
            else Seq.empty
    TryToCompleteBy (previousSemester last)
