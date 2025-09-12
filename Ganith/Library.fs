namespace Ganith


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
