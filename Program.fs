open System.IO

let input = File.ReadAllLines "/tmp/aoc/input.t" |> Array.toList


type Valve(name: string, flowRate: int, leadsTo: string list) =
    member this.Name = name
    member this.LeadsTo = leadsTo
    member this.FlowRate = flowRate
    member this.DeadEnd = leadsTo.Length = 1
    override this.ToString() = $"Valve({name}, {flowRate}, {leadsTo})"

let parse (s: string) =
    let s = s.Split [| ' '; ';'; ','; '=' |]
    Valve(s[1], s[5] |> int, s |> Array.skip 11 |> Array.toList |> List.filter (fun s -> s.Length = 2))

let valves = input |> List.map parse

valves |> List.map (printfn "%A")

let valveMap = valves |> List.map (fun tunnel -> tunnel.Name, tunnel) |> Map
let current = "AA"
let minutesLeft = 30

type Visited
    (
        current: string,
        time: int,
        visitedValves: Map<string, int>,
        opened: Set<string>,
        released: int,
        openedValves: int,
        best: int,
        valvesLeft: int
    ) =
    member this.Valve = valveMap[current]
    member this.OutOfTime = time < 1
    member this.IsOpen = opened.Contains current
    member this.Released = released

    member this.Value =
        if opened.Contains current then
            0
        else
            this.Valve.FlowRate * (time - 1)

    member this.Next = valveMap[current].LeadsTo

    member this.Best = best

    member this.loopedBackWithNoResult =
        visitedValves |> Map.tryFind current = Some(openedValves)

    member this.SecondDeadEnd = valveMap[current].DeadEnd && this.Value = 0
    member this.TooLittleLeft = released + (valvesLeft * time) < best

    member this.Open() =
        let opened = opened.Add current
        let openedValves = openedValves + 1
        let valvesLeft = valvesLeft - valveMap[current].FlowRate
        Visited(current, time - 1, visitedValves, opened, released + this.Value, openedValves, best, valvesLeft)

    member this.registerTime(newBest) =
        let best = if newBest > best then newBest else best
        Visited(current, time, visitedValves, opened, released, openedValves, best, valvesLeft)

    member this.MoveTo(next: string) =
        Visited(
            next,
            time - 1,
            visitedValves.Add(current, openedValves),
            opened,
            released,
            openedValves,
            best,
            valvesLeft
        )

    override this.ToString() =
        $"Visited({current},{time},{visitedValves},{opened},{released},{openedValves},B:{best},L:{valvesLeft}"

let rec findMax (visited: Visited) =
    // printfn $"findMax {visited}"
    // if visited.SecondDeadEnd then printfn "|terminate(deadend)"
    // if visited.OutOfTime then printfn "|terminate(outoftime)"
    // if visited.loopedBackWithNoResult then printfn "|terminate(noresult)"
    // if visited.TooLittleLeft then printfn "|terminate(tooLittleLeft)"
    let shouldStop =
        visited.SecondDeadEnd
        || visited.OutOfTime
        || visited.loopedBackWithNoResult
        || visited.TooLittleLeft

    if shouldStop then
        visited.Released
    else
        let valueIfOpen =
            if visited.Value > 1 then
                let visited = visited.Open()

                let rec visit (best: int) (nexts: string list) =
                    // printfn $"visit (best={best} -> {nexts}"
                    let visited = visited.registerTime best

                    match nexts with
                    | [] -> best
                    | next :: rest ->
                        let result = findMax (visited.MoveTo(next))
                        let best = max result best
                        visit best rest

                visited.Next |> visit visited.Best
            else
                0

        let valueIfSkip =
            let rec visit (best: int) (nexts: string list) =
                // printfn $"visit (best={best} -> {nexts}"
                let visited = visited.registerTime best

                match nexts with
                | [] -> best
                | next :: rest ->
                    let result = findMax (visited.MoveTo(next))
                    let best = max result best
                    visit best rest

            visited.Next |> visit visited.Best

        max valueIfOpen valueIfSkip

let valvesTotal = valves |> List.map (fun v -> v.FlowRate) |> List.sum
let start = Visited("AA", 30, Map.empty, Set.empty, 0, 0, 0, valvesTotal)

let res = findMax start


type OpType =
    | O
    | M

type LastOper = OpType * OpType

type Visited2
    (
        current: string,
        ecurr: string,
        time: int,
        visitedValves: Map<string, int>,
        opened: Set<string>,
        released: int,
        openedValves: int,
        best: int,
        valvesLeft: int,
        lastOper: LastOper
    ) =
    member this.Valve = valveMap[current]
    member this.EValve = valveMap[ecurr]
    member this.OutOfTime = time < 1
    member this.IsOpen = opened.Contains current
    member this.eIsOpen = opened.Contains ecurr
    member this.Released = released

    member this.Value =
        if opened.Contains current then
            0
        else
            this.Valve.FlowRate * (time - 1)

    member this.EValue =
        if opened.Contains ecurr then
            0
        else
            this.EValve.FlowRate * (time - 1)


    member this.Next = valveMap[current].LeadsTo
    member this.ENext = valveMap[ecurr].LeadsTo
    member this.Best = best

    member this.loopedBackWithNoResult =
        let moved = fst lastOper = M
        let emoved = snd lastOper = M
        let loop1 = moved && visitedValves |> Map.tryFind current = Some(openedValves)
        let loop2 = emoved && visitedValves |> Map.tryFind ecurr = Some(openedValves)
        loop1 // && loop2

    member this.Together = current = ecurr

    member this.SecondDeadEnd =
        valveMap[current].DeadEnd && this.Value = 0
        || valveMap[ecurr].DeadEnd && this.Value = 0

    member this.TooLittleLeft = released + (valvesLeft * (time + 2)) < best

    member this.Open() =
        let opened = opened.Add current
        let openedValves = openedValves + 1
        let valvesLeft = valvesLeft - valveMap[current].FlowRate
        let lastOper = (O, snd lastOper)
        let time = time - 1
        let released = released + this.Value

        Visited2(current, ecurr, time, visitedValves, opened, released, openedValves, best, valvesLeft, lastOper)

    member this.EOpen() =
        let opened = opened.Add ecurr
        let openedValves = openedValves + 1
        let valvesLeft = valvesLeft - valveMap[ecurr].FlowRate
        let released = released + this.EValue
        let lastOper = (fst lastOper, O)
        Visited2(current, ecurr, time, visitedValves, opened, released, openedValves, best, valvesLeft, lastOper)

    member this.registerTime(newBest) =
        let best = if newBest > best then newBest else best
        Visited2(current, ecurr, time, visitedValves, opened, released, openedValves, best, valvesLeft, lastOper)

    member this.MoveTo(next: string) =
        let lastOper = (M, snd lastOper)
        let time = time - 1
        let visitedValves = visitedValves.Add(current, openedValves)
        Visited2(next, ecurr, time, visitedValves, opened, released, openedValves, best, valvesLeft, lastOper)

    override this.ToString() =
        $"Visited2(({current},{ecurr}),{time},{visitedValves},{opened},{released},{openedValves},B:{best},L:{valvesLeft},{lastOper})"

    member this.EMoveTo(next: string) =
        let lastOper = (fst lastOper, M)
        let visitedValves = visitedValves.Add(ecurr, openedValves)
        Visited2(current, next, time, visitedValves, opened, released, openedValves, best, valvesLeft, lastOper)

let rec findMax2 (visited: Visited2) =
    // printfn $"findMax2 {visited}"

    // if visited.SecondDeadEnd then printfn "|terminate(deadend)"

    // if visited.OutOfTime then printfn "|terminate(outoftime)"

    // if visited.loopedBackWithNoResult then printfn "|terminate(noresult)"

    // if visited.TooLittleLeft then printfn "|terminate(tooLittleLeft)"

    let shouldStop =
        visited.SecondDeadEnd
        || visited.OutOfTime
        || visited.loopedBackWithNoResult
        || visited.TooLittleLeft

    if shouldStop then
        visited.Released
    else
        let best = visited.Best
        let youCanOpen = visited.Value > 0
        let eleCanOpen = visited.EValue > 0 && (not visited.Together) // if you're together, only you open
        let bothCanOpen = youCanOpen && eleCanOpen

        let s () =
            let y = if youCanOpen then "Y" else "_"
            let e = if eleCanOpen then "E" else "_"
            printfn $"{y}{e} -> {visited}"
        // s ()

        let bothOpens =
            if bothCanOpen then
                // printfn "Both opens"
                let visited = visited.Open() // ticks
                let visited = visited.EOpen()
                findMax2 visited
            else
                0

        let best = max best bothOpens

        let eOpens =
            if eleCanOpen then
                // printfn "Ele opens"
                let visited = visited.EOpen()

                let rec move (best: int) (steps: string list) =
                    match steps with
                    | [] -> best
                    | next :: rest ->
                        let visited = visited.registerTime best
                        let visited = visited.MoveTo next
                        let value = findMax2 visited
                        let best = max best value
                        move best rest

                visited.Next |> move visited.Best
            else
                0

        let best = max best eOpens

        let youOpens =
            if youCanOpen then
                // printfn "You open"
                let visited = visited.Open()

                let rec move (best: int) (steps: string list) =
                    match steps with
                    | [] -> best
                    | next :: rest ->
                        let visited = visited.registerTime best
                        let visited = visited.EMoveTo next
                        let value = findMax2 visited
                        let best = max best value
                        move best rest

                visited.ENext |> move best
            else
                0

        let best = max best youOpens

        let noneOpens =
            // printfn "None opens"
            let nextPairs: (string * string) list =
                let toP (e: string list) (you: string) : (string * string) list = e |> List.map (fun e -> you, e)
                visited.Next |> List.collect (toP visited.ENext)

            let rec move (best: int) (steps: (string * string) list) =
                match steps with
                | [] -> best
                | (next, enext) :: rest ->
                    let visited = visited.registerTime best
                    let visited = visited.MoveTo next
                    let visited = visited.EMoveTo enext
                    // printfn $"Moved both: {(next, enext)} -> {visited}"
                    let value = findMax2 visited
                    let best = max best value
                    move best rest

            nextPairs |> move best

        let best = max best noneOpens
        best

let start2 =
    Visited2("AA", "AA", 26, Map.empty, Set.empty, 0, 0, 0, valvesTotal, (M, M))

// let res2 = findMax2 start2

// printfn $"2={res2}"

type Cave(id:string) =
    member this.Id = id 
    override this.ToString() = "Cave"

    static member ofValve(valve:Valve) =
        Cave(valve.Name)
        
let caves = valves |> List.map Cave.ofValve |> List.map (fun c -> c.Id,c) |> Map.ofList 

type Caves (caves:Map<string,Cave>) =
    member this.Caves = caves
    override this.ToString() = "Caves"
type Trip(time:int,you:string,ele:string,caves:Caves) =
    override this.ToString() = "Trip"
    
let trip = Trip(26,"AA","AA",Caves(caves))

printfn $"{trip}"

