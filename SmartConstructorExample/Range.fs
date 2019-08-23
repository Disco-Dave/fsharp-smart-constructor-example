namespace SmartConstructorExample

type Bound<'a> =
    | Inclusive of 'a
    | Exclusive of 'a

[<RequireQualifiedAccess>]
module Bound =
    let getValue bound =
        match bound with
        | Inclusive a -> a
        | Exclusive a -> a

type Range<'a when 'a : comparison> private (lowerBound : Bound<'a>, upperBound : Bound<'a>) =
    member val LowerBound = lowerBound
    member val UpperBound = upperBound
    
    static member Make lowerBound upperBound =
        let lowerValue = Bound.getValue lowerBound
        let upperValue = Bound.getValue upperBound

        if lowerValue <= upperValue then
            Some <| new Range<'a>(lowerBound, upperBound)
        else
            None

type RangeResult =
    | BelowLowerBound
    | WithinBounds
    | AboveUpperBound
    
[<RequireQualifiedAccess>]
module Range =
    let private isWithinUpper<'a when 'a : comparison> (range : Range<'a>) value =
        match range.UpperBound with
        | Inclusive u -> value <= u
        | Exclusive u -> value < u

    let private isWithinLower<'a when 'a : comparison> (range : Range<'a>) value =
        match range.LowerBound with
        | Inclusive l -> value >= l
        | Exclusive l -> value > l

    let test range value =
        match (isWithinLower range value, isWithinUpper range value) with
        | (true, true) -> WithinBounds
        | (false, _) -> BelowLowerBound
        | (_, false) -> AboveUpperBound

    let isWithin range value =
        match (test range value) with
        | WithinBounds -> true
        | _ -> false
