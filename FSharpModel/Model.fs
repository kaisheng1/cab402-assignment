module StudyPlannerModel

open QUT

// Functions dealing with unit lists ...

// Loads unit information about all QUT units from a resource file
let private unitList : Map<UnitCode,UnitInfo> = 
    Parser.parseUnitData CourseData.Properties.Resources.units

// Lookup the given unit code in the unitList
let lookup (code:UnitCode) : UnitInfo = 
    // TODO: Fixme (difficulty: 1/10)
    unitList.[code]

// Functions dealing with semester sequences ...

// The semester prior to the given semester
// e.g previousSemester 2020/2 = 2020/1
//     previousSemester 2020/1 = 2019/S
//     previousSemester 2020/S = 2020/2

let previousSemester (semester:Semester) =
    // TODO: Fixme (difficulty: 2/10)
    match semester.offering with
    | Summer -> {year = semester.year; offering = Semester2}
    | Semester2 -> {year = semester.year; offering = Semester1}
    | Semester1 -> {year = semester.year-1; offering = Summer}
    

// The semester after to the given semester
// e.g nextSemester 2020/1 = 2020/2
//     nextSemester 2020/2 = 2020/S
//     nextSemester 2020/S = 2021/1
let nextSemester (semester:Semester) =
    // TODO: Fixme  (difficulty: 2/10)
    match semester.offering with
    | Summer -> {year = semester.year+1; offering = Semester1}
    | Semester2 -> {year = semester.year; offering = Summer}
    | Semester1 -> {year = semester.year; offering = Semester2}


// Returns a sequence of consecutive semesters starting from the first semester and ending at the last semester.
// E.g. SemesterSequence 2019/2 2021/1 would return the sequence 2019/2, 2019/S, 2020/1, 2020/2, 2020/S, 2021/1.
let rec SemesterSequence (firstSemester: Semester) (lastSemester: Semester): seq<Semester> =
    // TODO: Fixme (difficulty: 4/10)
    if (firstSemester = lastSemester) then Seq.singleton lastSemester
    else if (firstSemester > lastSemester) then Seq.empty //stops here
    else 
        seq{
            yield firstSemester
            yield! SemesterSequence (nextSemester firstSemester) lastSemester //get the next semester
        }



// Functions dealing with prerequisites ...

// True if and only if the prerequisites have been met based on units in the study 
// plan taken in an earlier semester (based on the before function)
let rec private satisfied (prereq:Prereq) (plannedUnits:StudyPlan) (before: Semester->bool) : bool = 
    // TODO: Fixme (difficulty: 8/10)
    match prereq with 
    | And s -> Seq.forall (fun x -> satisfied x plannedUnits before) s
    | Or s -> Seq.exists (fun x -> satisfied x plannedUnits before) s
    | Unit u -> plannedUnits |> Seq.exists (fun x -> before x.semester && x.code = u)
    | CreditPoints p -> 
        let getPoints (unitInPlan: UnitInPlan) = 
            unitInPlan 
            |> (fun x -> lookup x.code)
            |> (fun unitInfo -> unitInfo.creditpoints)
        let getTotalPoints plannedUnits = 
            plannedUnits 
            |> Seq.fold (fun acc elem -> if (before elem.semester) then acc + getPoints elem else acc) 0

        p <= getTotalPoints plannedUnits 
    | Nil -> true
        





 // Functions used for determining when units can be studied ...

 // True if and only if the unit with the specified unit code is offered in the specified semester
let isOffered (unitCode:UnitCode) (semester:Semester) : bool = 
    // TODO: Fixme (difficulty: 4/10)
    unitCode 
    |> lookup 
    |> (fun unit -> unit.offered)
    |> Set.contains semester.offering



// True if and only if the specified unit can be studied in the specified semester based on the specified study plan.
// Requires that the unit is offered in that semester and that prerequistes are meet by units studied before that semester 
let isLegalIn (unitCode:UnitCode) (semester:Semester) (plannedUnits:StudyPlan) : bool =
    let prereq = 
        unitCode
        |> lookup 
        |> (fun x -> x.prereq)

    (isOffered unitCode semester) && (satisfied prereq plannedUnits (fun sem -> sem < semester))

// True if and only if the specified unit can be added to the study plan in that semester.
// Requires that the number of units currently studied in that semester is less than four and that it is legal in that semester
let isEnrollableIn (unitCode:UnitCode) (semester:Semester) (plannedUnits:StudyPlan) : bool =
    // TODO: Fixme (difficulty: 2/10)
    let numberOfUnits semester = Seq.fold (fun acc elem -> if (elem.semester = semester) then acc + 1 else acc) 0 plannedUnits

    (isLegalIn unitCode semester plannedUnits) && (numberOfUnits semester) < 4

// True if and only if the unit can be legally added to the study plan (in some semester) 
let isEnrollable (unitCode:UnitCode) (plannedUnits:StudyPlan) : bool =
    // TODO: Fixme (difficulty: 4/10)
    let prereq = 
        unitCode
        |> lookup   
        |> (fun x -> x.prereq)

    satisfied prereq plannedUnits (fun sem -> true)

// True if and only if the all of the units in the study plan are legally scheduled
let isLegalPlan (plan: StudyPlan): bool =
    // TODO: Fixme (difficulty: 4/10)
    let allLegalSemester plan = 
        plan 
        |> Seq.groupBy (fun x -> x.semester)
        |> Seq.forall (fun (sem, allunits) -> Seq.length allunits <= 4)
    let allLegalUnit (plan: StudyPlan) = 
        plan 
        |> Seq.forall (fun unit -> isLegalIn unit.code unit.semester plan)

    allLegalUnit plan && allLegalSemester plan



// Functions returning various information about units ...

// Returns all of the unit codes that are mentioned anywhere in the prerequisites of the specified unit
let UnitPrereqs (unitCode:UnitCode) : seq<UnitCode> = 
    // TODO: Fixme (difficulty: 6/10)
    let prereq = 
        unitCode
        |> lookup 
        |> (fun x -> x.prereq)

    let appendUnitsAfter func acc elem = Seq.append acc (func elem)

    let rec prereqSeq prereq = 
        match prereq with 
        | And s -> Seq.fold (appendUnitsAfter prereqSeq) Seq.empty s
        | Or s -> Seq.fold (appendUnitsAfter prereqSeq) Seq.empty s
        | Unit u -> Seq.singleton u
        | _ -> Seq.empty

    prereqSeq prereq

// The title of the specified unit
// e.g. getUnitTitle("CAB402") = "Programming Paradigms"
let getUnitTitle (unitCode:UnitCode) : string = 
    // TODO: Fixme (difficulty: 2/10)
    unitCode
    |> lookup 
    |> (fun unitInfo -> unitCode + " " + unitInfo.title)


// The prerequisites of the specified unit as a string
// e.g. getPrereq("CAB402") = "Prereqs: (CAB201 or ITD121) and CAB203"
// e.g. getPrereq("IFB104") = "Prereqs: Nil"
let getPrereq (unitCode:UnitCode) : string = 
    // TODO: Fixme (difficulty: 3/10)
    let convertPrereqString prereqString = 
        match prereqString with
        | "" -> "Prereqs: Nil"
        | _ -> "Prereqs: " + prereqString
    unitCode 
    |> lookup 
    |> (fun unitInfo -> unitInfo.prereqString)
    |> convertPrereqString 

// The semesters that the specified unit is offered in as a string
// e.g. displayOffered("CAB201") = "semester 1 or 2"
// e.g. displayOffered("CAB402") = "semester 1"
let displayOffered (unitCode:UnitCode) : string =
    // TODO: Fixme (difficulty: 5/10)
    let offered = 
        unitCode
        |> lookup 
        |> (fun unitInfo -> unitInfo.offered)

    if (Set.contains Semester1 offered) then 
        if (Set.contains Semester2 offered) then "semester 1 or 2"
        else "semester 1"
    else "semester 2"
    

// The specified semester as a string (format: year/semester)
// e.g. display(currentSemester) = "2020/1"
let display (sem:Semester) : string = 
    // TODO: Fixme (difficulty: 2/10)
    match sem.offering with 
    | Summer -> (string sem.year) + "/S"
    | Semester1 -> (string sem.year) + "/1"
    | Semester2 -> (string sem.year) + "/2"