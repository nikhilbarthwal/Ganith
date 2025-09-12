namespace Ganith

open System.Collections.Immutable


[<Struct>] type Maybe<'T> = Yes of 'T | No

module Loop =

    [<TailCall>]
    let rec Verify (f: int -> bool) (a: int) (b: int): bool =
        if a = b then true else
            if (f a) then (Verify f (a + 1) b) else false

    [<TailCall>]
    let rec Search (f: int -> bool) (a: int) (b: int): int =
        if a = b then b else
            if (f a) then a else (Search f (a + 1) b)


module Utils =

    let Array<'T>(length: int , f: int -> 'T) =
        let arr = [| for i in 0 .. length - 1 -> f i |] in arr.ToImmutableArray()