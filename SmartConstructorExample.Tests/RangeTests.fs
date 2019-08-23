module SmartConstructorExample.Tests.RangeTests

open FsCheck
open FsCheck.Xunit
open SmartConstructorExample

let private boundsAreValid (lowerBound : Bound<int>) (upperBound : Bound<int>) =
    Bound.getValue lowerBound <= Bound.getValue upperBound
    
let private boundsAreInValid (lowerBound : Bound<int>) (upperBound : Bound<int>) =
    Bound.getValue lowerBound > Bound.getValue upperBound
    
let private valuesBelowLowerBound (lowerBound : Bound<int>) (upperBound : Bound<int>) (value : int) =
   boundsAreValid lowerBound upperBound &&
       match lowerBound with
       | Exclusive bound -> value <= bound
       | Inclusive bound -> value < bound
   
let private valuesWithinRange (lowerBound : Bound<int>) (upperBound : Bound<int>) (value : int) =
   boundsAreValid lowerBound upperBound &&
       match (lowerBound, upperBound) with
       | (Exclusive l, Exclusive u) -> value > l && value < u
       | (Inclusive l, Inclusive u) -> value >= l && value <= u
       | (Exclusive l, Inclusive u) -> value > l && value <= u
       | (Inclusive l, Exclusive u) -> value >= l && value < u
       
let private valuesAboveUpperBound (lowerBound : Bound<int>) (upperBound : Bound<int>) (value : int) =
   boundsAreValid lowerBound upperBound &&
       match upperBound with
       | Exclusive bound -> value >= bound && Bound.getValue lowerBound <> bound
       | Inclusive bound -> value > bound && Bound.getValue lowerBound <> bound
       
let private valuesNotWithinRange (lowerBound : Bound<int>) (upperBound : Bound<int>) (value : int) =
    valuesBelowLowerBound lowerBound upperBound value || valuesAboveUpperBound lowerBound upperBound value
       
[<Property>]
let ``getValue always returns the correct value for Inclusive`` (value : int) =
    let bound = Inclusive value
    Bound.getValue bound = value
    
[<Property>]
let ``getValue always returns the correct value for Exclusive`` (value : int) =
    let bound = Exclusive value
    Bound.getValue bound = value
    
[<Property>]
let ``Only allowed to make valid ranges`` (lowerBound : Bound<int>) (upperBound : Bound<int>) =
   boundsAreValid lowerBound upperBound ==> (Range.Make lowerBound upperBound).IsSome
        
[<Property>]
let ``Does not allow invalid ranges`` (lowerBound : Bound<int>) (upperBound : Bound<int>) =
    boundsAreInValid lowerBound upperBound ==> (Range.Make lowerBound upperBound).IsNone

[<Property>]
let ``Identifies values that are below the lower bound``(lowerBound : Bound<int>) (upperBound : Bound<int>) (value : int) =
    valuesBelowLowerBound lowerBound upperBound value ==>
        match Range.Make lowerBound upperBound with
        | None -> false
        | Some range ->
            match Range.test range value with
            | BelowLowerBound -> true
            | _ -> false
            
[<Property>]
let ``Identifies values that are within range`` (lowerBound : Bound<int>) (upperBound : Bound<int>) (value : int) =
    valuesWithinRange lowerBound upperBound value ==>
        match Range.Make lowerBound upperBound with
        | None -> false
        | Some range ->
            match Range.test range value with
            | WithinBounds -> true
            | _ -> false
            
[<Property>]
let ``Identifies values that are above the upper bound`` (lowerBound : Bound<int>) (upperBound : Bound<int>) (value : int) =
    valuesAboveUpperBound lowerBound upperBound value ==>
        match Range.Make lowerBound upperBound with
        | None -> false
        | Some range ->
            match Range.test range value with
            | AboveUpperBound -> true
            | _ -> false
            
[<Property>]
let ``Returns true if value is within range`` (lowerBound : Bound<int>) (upperBound : Bound<int>) (value : int) =
    valuesWithinRange lowerBound upperBound value ==>
        match Range.Make lowerBound upperBound with
        | None -> false
        | Some range -> Range.isWithin range value
        
[<Property>]
let ``Returns false if value is not within range`` (lowerBound : Bound<int>) (upperBound : Bound<int>) (value : int) =
    valuesNotWithinRange lowerBound upperBound value ==>
        match Range.Make lowerBound upperBound with
        | None -> false
        | Some range -> not <| Range.isWithin range value 