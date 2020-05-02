namespace SmartConstructorExample

type Bound<'a> =
    | Inclusive of 'a
    | Exclusive of 'a

[<RequireQualifiedAccess>]
module Bound =
    let value = function
        | Inclusive a -> a
        | Exclusive a -> a


type Range<'a when 'a: comparison> =
    private { LowerBound: Bound<'a>
              UpperBound: Bound<'a> }

type RangeResult =
    | BelowLowerBound
    | WithinBounds
    | AboveUpperBound

[<RequireQualifiedAccess>]
module Range =
    let make lowerBound upperBound =
        let lowerValue = Bound.value lowerBound
        let upperValue = Bound.value upperBound

        if lowerValue <= upperValue then
            Some <| { LowerBound = lowerBound
                      UpperBound = upperBound }
        else
            None

    let test range value =
        let isWithinUpperBound =
            match range.UpperBound with
            | Inclusive u -> value <= u
            | Exclusive u -> value < u

        let isWithinLowerBound =
            match range.LowerBound with
            | Inclusive l -> value >= l
            | Exclusive l -> value > l

        match (isWithinLowerBound, isWithinUpperBound) with
        | (true, true) -> WithinBounds
        | (false, _) -> BelowLowerBound
        | (_, false) -> AboveUpperBound

    let isWithin range value =
        match test range value with
        | WithinBounds -> true
        | _ -> false
