namespace Ganith

open System


type Vector<'T when 'T :> IEquatable<'T>>(length: int, gen: int -> 'T) =
    let data  = [| for i in 0 .. length - 1 do gen i|]
    member this.Get k = data[k]
    member this.Length = length
    member this.Item(k: int) = data[k]

    static member inline (+) (v1: Vector<'V>, v2: Vector<'V>): Vector<'V> =
        assert (v1.Length = v2.Length)
        let gen i = v1[i] + v2[i] in Vector(v1.Length, gen)

    static member inline (-) (v1: Vector<'V>, v2: Vector<'V>): Vector<'V> =
        assert (v1.Length = v2.Length)
        let gen i = v1[i] - v2[i] in Vector(v1.Length, gen)

    static member inline convolve<'V when 'V: (static member (*) : 'V * 'V -> 'V)
                                      and 'V: (static member (+) : 'V * 'V -> 'V)
                                      and 'V :> IEquatable<'V>>
            (v1: Vector<'V>, v2: Vector<'V>): 'V =
        assert (v1.Length = v2.Length)
        let f (sum: 'V) (k: int): 'V =  sum + v1[k] * v2[k]
        let init = v1[0] * v2[0] in (List.fold f init [1 .. v1.Length - 1])

    static member inline Convolve<'V when 'V: (static member (*) : 'V * 'V -> 'V)
                                      and 'V: (static member (+) : 'V * 'V -> 'V)>
            (l: int) (f1: int -> 'V) (f2: int -> 'V): 'V =
        let sum (s: 'V) (k: int) = s + (f1 k) * (f2 k)
        let init = (f1 0) * (f2 0) in (List.fold sum init [1 .. l - 1])

    interface IEquatable<Vector<'T>> with
        override this.Equals(x: Vector<'T>): bool =
            let eq i = this[i].Equals(x[i]) in (Loop.Verify eq 0  length)
