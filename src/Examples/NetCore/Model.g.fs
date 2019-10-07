namespace Model
open FSharp.Data.Adaptive
[<AutoOpen>]
module rec ModelAdaptor =
    /// Adaptive representation for `Model`
    type AdaptiveModel private(__initial : Model.Model) =
        let _all = cset(__initial.all)
        let _value = cval(__initial.value)
        let _test = clist(__initial.test)
        let _foo = cval(__initial.foo)
        /// The current value of all as `aset<int>`.
        member __.all = _all :> aset<_>
        /// The current value of value as `aval<int>`.
        member __.value = _value :> aval<_>
        /// The current value of test as `alist<int>`.
        member __.test = _test :> alist<_>
        /// The current value of foo as `aval<string>`.
        member __.foo = _foo :> aval<_>
        /// Updates all values in the `AdaptiveModel` to the given `Model`.
        /// Note that it expects a Transaction to be current.
        member __.update(value : Model.Model) : unit =
            let __value = value
            _all.Value <- __value.all
            _value.Value <- __value.value
            _test.Value <- __value.test
            _foo.Value <- __value.foo
        /// Creates a new `AdaptiveModel` using the given `Model`.
        static member create(value : Model.Model) : AdaptiveModel = 
            AdaptiveModel(value)
