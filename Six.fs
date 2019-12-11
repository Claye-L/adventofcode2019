module Six

open System.IO
open System.Reflection
open System

type OrbitNode =
|COM of string
|Child of string 

let testdata ="COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L"
let data =
    let binpath = Path.GetDirectoryName (Assembly.GetExecutingAssembly().Location)
    File.ReadAllText(Path.Combine(binpath, "sixinput.txt")).Replace(Environment.NewLine," ");
let stringToMap (m: Map<string,OrbitNode>) (a,b) = 
    if a = "COM" then 
        (m.Add (a, COM a)).Add(b, Child a) 
    else  
        m.Add (b, Child a)

let parseOrbitTree (serialized:string) = 
    let split = serialized.Split(' ')
    let pairs = Seq.map (fun (s:string) -> Array.toList (s.Split(')')) ) split
                |> Seq.filter (fun l -> l.Length =2)
                |> Seq.map (fun l -> l.[0],l.[1]) |> Seq.toList
    let (map: Map<string,OrbitNode>) = Map.empty
    pairs|> List.fold stringToMap map

let rec distanceToParent node (tree: Map<string,OrbitNode>) pa =
    match node with 
    |COM a-> 1
    |Child (parent) -> if parent = pa then 1 else 1 + distanceToParent((tree.TryFind parent).Value) tree pa

let rec distanceToCom node (tree: Map<string,OrbitNode>) =
    match node with 
    |COM _-> 0
    |Child (parent) -> 1 + distanceToCom ((tree.TryFind parent).Value) tree

let totalOrbits (tree: Map<string,OrbitNode>) = 
    Map.toSeq tree |> Seq.map (fun (_,x) -> distanceToCom x tree) |> Seq.sum

let Six =
    parseOrbitTree data 
    |> totalOrbits

let getParents (node:OrbitNode) (tree: Map<string,OrbitNode>) =
    let getP key = (tree.TryFind key).Value
    let rec getParent node =
        match node with
        |COM (a) -> []
        |Child (parent) -> parent :: getParent (getP parent)
    getParent node

let getFirstCommonParent nodea nodeb tree =
    let parentsa = getParents nodea tree
    let parentsb = getParents nodeb tree
    let rec findCommonParent pa =
        match pa with
        |n :: tail -> if List.contains n parentsb then n else findCommonParent tail
        |[] -> failwith "no common parent uh oh"
    findCommonParent parentsa

let SixTwo = 
    let t = parseOrbitTree data 
    let (_,younode) = t.TryGetValue "YOU"
    let (_,santanode) = t.TryGetValue "SAN"
    let common = getFirstCommonParent younode santanode t
    (distanceToParent younode t common) + (distanceToParent santanode t common)