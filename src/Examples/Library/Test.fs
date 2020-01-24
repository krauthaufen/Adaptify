module Test

open LibraryModel
open LibraryModel2
open FSharp.Data.Adaptive

let test () =
    let soup = AdaptiveSoup.Create { important = { name = "hans" }; things = HashMap.empty }
    let res : amap<int, AdaptiveThing> = soup.things
    let res : AdaptiveThing = soup.important
    ()
