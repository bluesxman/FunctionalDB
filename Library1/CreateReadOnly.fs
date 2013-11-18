﻿module CreateReadOnly

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

// A value at a particular time is a Fact
type Fact = {value : Value; time : int}  //??? Probably more efficient way by tieing things to a single transaction


// A sequence of Facts about a single characteristic is a Attribute
// where no two facts shall have the same time (i.e. an attribute may
// only have one value at a point in time).
type Attribute = {name : string; history : List<Fact>}          // ??? Exposing the list seems to break encapsulation.  What if later
                                                                // down the road I want to make it a tree for faster search by time
                                                                // or some monad that keeps recent in memory and old on slow storage?
                                                                // Will deal with best practices for encapsultion during second pass.

type Operation =
    | Deleted of string // To remove an attribute from an entity we set its value to deleted at a time
    | Renamed of string * string // value is the attribute's old name and the new name
    

// The history of an entity as defined by its various characteristics
type Entity = {
    id : int; 
    attributes : Map<string, Attribute>; // The current attributes
    ops : List<int * Operation>
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

// create transaction, add entity, create attribute, change attribute value, rename attribute, delete attribute, delete entity (perhaps special attribute?)

// create an entity
let createEntity attributes database =
    {id = transactor.createEntityId ;   
    attributes = Map.ofList []}                     


// record facts about an entity
let recordFact entity attributeName attributeValue

//let recordFact name value entity =
    
//let maptest = Map.ofList [(5, "foo"); (6, true); (7, 6.7)]


let createAttribute name fact =
    {name = name; history = fact :: []}

let readNow predicate transform = // apply transform to all entitie where predicate is true

let readWhen predicate transform time = // list of entities

let 

// Example:
// Create db
// Create radio type with default settings
// Change radio type name
// Create platform
// Assign type
// Update state every frame