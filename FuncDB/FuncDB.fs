﻿module FuncDB

(*

Note, this is my scratchpad.  See "FuncDB" for the cleaner version

I'm keeping it around to have my prior
notes and thoughts handy.  Note, the history is kept per attribute
as a list of facts, where a fact is a value at a time.

Think I'll replace this with a FuncDB module.  Definitely will clean
things up there.  History per entity (i.e. the timestamp is at the 
entity level) is probably a better way to go.

*)

// A value at a particular time is a Fact
//type FactClass<'a>(value : 'a, time : int) = 
//    member this.value = value
//    member this.time = time

// A set of Attributes about a single subject is a Record
//type RecordClass<'a>(attributeName : string, fact : Fact<'a>, prior : RecordClass<'a>) =
//    member this.attributes = Map.add attributeName fact prior.attributes


// Goals:
// #1 Focal point for initial survey of new languages
// #2 Explore functional DB concepts
// #3 Don't let practical considerations (e.g. performance) interfere with 1 and 2
// #4 Be extreme in the idea of "many simple unentangled building blocks"
// #5 Don't dive too deeply in 1 language before implementing in other languages.  Do several iterations, adding new features/concepts each time.


// Records and unions are automatically immutable types


// The value that an attribute may have at a particular time
// Essentially want Entity to be a heterogenous Map of attributes.  Using the union as a workaround since F# is strongly typed
type Value =
    | String of string
    | Int of int
    | Boolean of bool
    | Float of float
    | Object of obj
    | Key of int * int // the id of a entity and a time
    | Undef // May use this to indicate the attribute didnt exist at that time - also testing Git in Visual Studio in this commit

// A value at a particular time is a Fact
type Fact = {value : Value; time : int}  //??? Probably more efficient way by tieing things to a single transaction


// A sequence of Facts about a single characteristic is a Attribute
// where no two facts shall have the same time (i.e. an attribute may
// only have one value at a point in time).
type Attribute = {name : string; history : List<Fact>}          // ??? Exposing the list seems to break encapsulation.  What if later
                                                                // down the road I want to make it a tree for faster search by time
                                                                // or some monad that keeps recent in memory and old on slow storage?
                                                                // Will deal with best practices for encapsultion during second pass.

// This can wait.  Get create and queries done before we support schema changes
type Operation =
    | Deleted of string // To remove an attribute from an entity we set its value to deleted at a time
    | Renamed of string * string // value is the attribute's old name and the new name
    

// The history of an entity as defined by its various characteristics
type Entity = {
    id : int; 
//    attributes : Map<string, Attribute> // The current attributes
    attributes : Map<string, List<Fact>>
//    ;ops : List<int * Operation>
}   

// All the entities
type Database = {
    name : string;
    entities : Map<int, Entity>;
    entitySeq : int;
    timeSeq : int}                             
    
// Note, no schema explicitly.  The database has no idea if entities are the same type.
// However, something like a table definition could be an attribute of all entities.
// Since schema is an attribute it has a history of its own.  Imagine, 100 entities
// are the same type and share the same schema.  Schema changes and the schema attribute
// of all the entities are adds a fact as well as the migrated attributes to fit the schema.
// Definitely need to support sharing references to values (which are immutable of course)
// instead of copying values.  E.g. 100 copies of a reference to the schema rather than 100
// copies of the schema.
//
// Also an example for the necessity of transactions: changing the schema and migrating 
// the attributes needs to be an atomic step.  A transaction would be a set of facts in
// an entity with the same time.

// Goals with the functions:
// #1: support composition
// #2: no side effects
// #3: separation of concerns

// create a DB
let createDatabase name = {name = name; entities = Map.empty; entitySeq = 0; timeSeq = 0}

//let createEntity id time attribMap =
//    let attribs = Map.map (fun k v -> [{value = v; time = time}]) attribMap
//    {id = id; attributes = attribs}
    
let transact db ops =
    let time = db.timeSeq + 1
    {name = db.name; entities = db.entities; entitySeq = db.entitySeq; timeSeq = time};

// create transaction, add entity, create attribute, change attribute value, rename attribute, delete attribute, delete entity (perhaps special attribute?)

// create an entity.  return the entity and the new version of the DB
//let createEntity entityId time attribMap  =
//    let createAttribute (name:string, value:Value) =
//        {name; [{value; time}]}
//    let attributes = Map.map (fun (k, v) -> createAttribute k v) attribMap
//    {entityId; attributes}




// record facts about an entity
//let recordFact entity attributeName attributeValue

//let recordFact name value entity =
    
//let maptest = Map.ofList [(5, "foo"); (6, true); (7, 6.7)]


//let createAttribute name fact = {name = name; history = [fact];}

//let readNow predicate transform = // apply transform to all entitie where predicate is true
//
//let readWhen predicate transform time = // list of entities
//
//let 

// Example:
// Create db
// Create radio type with default settings
// Change radio type name
// Create platform
// Assign type
// Update state every frame


// what if an entity was an attribute map (AVL tree), a timestamp, and a link back                                     
// to its predecessor. 
let createEntity time id attribs =
    let attribMap = 
        List.map (fun (a, v) -> (a, [{value = v; time = time}])) attribs
        |> Map.ofList
    {id = id; attributes = attribMap}



let addAll entities db =
    let time = db.timeSeq + 1
    let idAttribSeq = Seq.zip (seq {db.entitySeq .. System.Int32.MaxValue}) (Seq.ofList entities)
    let create (id, attribs) = (id, createEntity time id attribs)
    let add entMap (id, entity) = Map.add id entity entMap
    let idEntSeq = Seq.map create idAttribSeq
    let newEntities = Seq.fold add db.entities idEntSeq
    {name = db.name; entities = newEntities; entitySeq = db.entitySeq + entities.Length; timeSeq = time}


let updateEntity time attribs entity = 
    let updateAttrib attribMap (attribName, attribValue) =
        let newFact = {value = attribValue; time = time}
        let newFactList = 
            match Map.tryFind attribName attribMap with
            | Some (list) -> newFact :: list
            | None -> [newFact]
        Map.add attribName newFactList attribMap
    let newAttribs = List.fold updateAttrib entity.attributes attribs
    {id = entity.id; attributes = newAttribs}


let update where attribs db =
    let time = db.timeSeq + 1
    let applyChange entityMap (id, entity) =
        let newEntity = updateEntity time attribs entity
        Map.add id newEntity entityMap
    let newEntityMap = 
        Seq.filter where (Map.toSeq db.entities)
        |> Seq.fold (applyChange) db.entities
    {name = db.name; entities = newEntityMap; entitySeq = db.entitySeq; timeSeq = time}


let select where db =
    Seq.filter where (Map.toSeq db.entities)

let valueWhen time attributeName entity =
    let rightBefore factList =
        match List.tryFind (fun fact -> fact.time <= time) factList with
        | Some(fact) -> fact.value
        | None -> Undef
        
    match Map.tryFind attributeName entity.attributes with
    | Some (factList) -> rightBefore factList
    | None -> Undef

let valueNow attributeName entity =
    match Map.tryFind attributeName entity.attributes with
    | Some (fact :: _) -> fact.value
    | _ -> Undef   // Handle Some([]) and None

// each e where "name" equals "alpha" and "active" equals true

// Test whether an attribute is equal to a given value
let equalsNow attribute (value : Value) =
    fun (id, entity) -> valueNow attribute entity = value

let equalsWhen time attribute (value : Value) =
    fun (id, entity) -> valueWhen time attribute entity = value

let AND pred1 pred2 = // and
    fun entity -> pred1 entity && pred2 entity

let OR pred1 pred2 = // or
    fun entity -> pred1 entity || pred2 entity

let v = String("alpha")
//let nameAlpha = EQUALS("name" String("alpha"))

//let nameAlpha = EQUALS "name" (String("alpha"))
//let activeTrue = EQUALS "active" (Boolean(true))
//let pred = AND nameAlpha activeTrue
//let pred2 = AND (EQUALS "name" (String("alpha"))) (EQUALS "active" (Boolean(true)))
//
//let gt5 x = x > 5
//let lt10 x = x < 10
//let foo = AND gt5 lt10
//let seven = foo 7
//let twelve = foo 12

//let where = ["name"; EQUALS; "alpha"; AND; "active"; EQUALS; true]
//blah







let alphaAndBravo = [
    ["name",  String("alpha"); // Maybe could have a convention later for nested entities
    "x",      Float(1.0);      // and make automatic FKs (child to parent)
    "y",      Float(8.0);
    "active", Boolean(true)
    ];
    ["name",  String("bravo"); 
    "x",      Float(4.0); 
    "y",      Float(3.0); 
    "active", Boolean(true)
    ];
]

let charlie = [
    ["name",  String("charlie"); 
    "x",      Float(4.0); 
    "y",      Float(3.0); 
    "active", Boolean(true)
    ];
]

// create "test" db
// create entities alpha and bravo in the database
// deactivate alpha
// create charlie
// move bravo
let db = 
    createDatabase "test"
    |> addAll alphaAndBravo
    |> update (equalsNow "name" (String("alpha"))) ["active", Boolean(false)]
    |> addAll charlie
    |> update (equalsNow "name" (String("bravo"))) ["x", Float(-10.0); "y", Float(-20.0)]


// select all active now
let activeNow = List.ofSeq (select (equalsNow "active" (Boolean(true))) db)

// select all active before
let activeAt1 = List.ofSeq (select (equalsWhen 1 "active" (Boolean(true))) db)


// Current Thoughts and Issues:
// 1) A set of operations can't occur atomically, e.g. create an entity and update another in a single transaction
// 2) Timestamps per attribute eat more memory than timestamping the entity
// 3) Returning tuple (id, entity) from selects feels clunky
// 4) If attributes change a lot, finding old attribute values will be O(n) since its a list
// 5) 