namespace SmartConstructorExample

module Range =
    type Bound<'a> =
        | Inclusive of 'a
        | Exclusive of 'a

    let getBound bound =
        match bound with
        | Inclusive a -> a
        | Exclusive a -> a

    type RangeResult =
        | BelowLowerBound
        | WithinBounds
        | AboveUpperBound

    type Range<'a when 'a : comparison> private (lowerBound : Bound<'a>, upperBound : Bound<'a>) =
        member val LowerBound = lowerBound
        member val UpperBound = upperBound
        
        static member Make lowerBound upperBound =
            let upperValue = getBound lowerBound
            let lowerValue = getBound upperBound

            if lowerValue <= upperValue then
                Some <| new Range<'a>(lowerBound, upperBound)
            else
                None

    let private isWithinUpper<'a when 'a : comparison> (range : Range<'a>) value =
        match range.UpperBound with
        | Inclusive u -> value <= u
        | Exclusive u -> value < u

    let private isWithinLower<'a when 'a : comparison> (range : Range<'a>) value =
        match range.LowerBound with
        | Inclusive l -> value >= l
        | Exclusive l -> value > l

    let testRange range value =
        match (isWithinLower range value, isWithinUpper range value) with
        | (true, true) -> WithinBounds
        | (false, _) -> BelowLowerBound
        | (_, false) -> AboveUpperBound

    let isWithin range value =
        match (testRange range value) with
        | WithinBounds -> true
        | _ -> false
