namespace Ganith

open System


type Vector<'T when 'T :> IEquatable<'T>
                and 'T :> Numerics.IAdditionOperators<'T, 'T, 'T>
                and 'T :> Numerics.ISubtractionOperators<'T, 'T, 'T>
                and 'T :> Numerics.IMultiplyOperators<'T, 'T, 'T>>(length: int, gen: int -> 'T) =
    let data  = [| for i in 0 .. length - 1 do gen i|]
    member this.Get k = data[k]
    member this.Length = length
    member this.Item(k: int) = data[k]
    member this.Convolve(gen: int -> 'T): 'T =
        let f (sum: 'T) (k: int): 'T =  sum + this[k] * (gen k)
        let init = this[0] * (gen 0) in (List.fold f init [1 .. length - 1])

    static member inline (+) (v1: Vector<'V>, v2: Vector<'V>): Vector<'V> =
        assert (v1.Length = v2.Length)
        let gen i = v1[i] + v2[i] in Vector(v1.Length, gen)

    static member inline (-) (v1: Vector<'V>, v2: Vector<'V>): Vector<'V> =
        assert (v1.Length = v2.Length)
        let gen i = v1[i] - v2[i] in Vector(v1.Length, gen)

    static member inline (*) (v1: Vector<'V>, v2: Vector<'V>): 'V =
        assert (v1.Length = v2.Length) ; v1.Convolve(v2.Get)

    interface IEquatable<Vector<'T>> with
        override this.Equals(x: Vector<'T>): bool =
            let eq i = this[i].Equals(x[i]) in (Loop.Verify eq 0  length)
