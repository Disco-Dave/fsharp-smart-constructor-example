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
    private { _lowerBound: Bound<'a>
              _upperBound: Bound<'a> }
    member this.LowerBound = this._lowerBound
    member this.UpperBound = this._upperBound

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
            Some <| { _lowerBound = lowerBound
                      _upperBound = upperBound }
        else
            None

    let test (range : Range<'a>) value =
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
        
    let lowerBound (range : Range<'a>) =
        range.LowerBound
        
    let upperBound (range : Range<'a>) =
        range.UpperBound
