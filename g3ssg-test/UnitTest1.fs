module g3ssg_test

open NUnit.Framework
open Logic

[<TestFixture>]
type AstroMTest () =
    [<DefaultValue>] val mutable _dist : distance

    [<SetUp>]
    member self.Setup () =
        self._dist <- AU 1m

    [<Test>]
    member self.Test_AUVariance () =
        printf "%A %A" self._dist (Fuzzy.vp 10 (distance.extract self._dist))
