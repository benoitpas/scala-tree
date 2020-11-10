class MyTreeTest extends org.scalatest.funsuite.AnyFunSuite {

  import MyTree._

  test("Simple Tree") {
    val l = Branch("a", Leaf(), Leaf())
    assert(l.v == "a")

    val lid = addId(l)
    val lid2 = addId2(l)

    assert(getId(lid, -1) == 1)
    assert(getId(lid2, -1) == 1)
  }

  val t = Branch("a",
            Branch("b",
              Branch("e", Leaf(), Leaf()),
              Leaf()),
            Branch("c",
              Leaf(),
              Branch("d", Leaf(), Leaf())))
  test("More complex Tree") {
    assert(t.v == "a")

    val e = Branch(("a", 5),
      Branch(("b", 2),
        Branch(("e", 1), Leaf(), Leaf()),
        Leaf()),
      Branch(("c", 4),
        Leaf(),
        Branch(("d", 3), Leaf(), Leaf())))
    val tid = addId(t)
    assert(tid == e)

    val tid2 = addId2(t)
    assert(tid2 == e)

  }

  val t2 = Branch("aa", t, t)
  test("Double the complex Tree") {
    assert(t2.v == "aa")

    val t2id = addId(t2)
    assert(t2id == Branch(("aa", 11),
      Branch(("a", 5),
        Branch(("b", 2),
          Branch(("e", 1), Leaf(), Leaf()),
          Leaf()),
        Branch(("c", 4), Leaf(), Branch(("d", 3), Leaf(), Leaf()))),
      Branch(("a", 10),
        Branch(("b", 7),
          Branch(("e", 6), Leaf(), Leaf()),
          Leaf()),
        Branch(("c", 9), Leaf(), Branch(("d", 8), Leaf(), Leaf())))))
  }

  val t3 = Branch("a",
    Branch("b",
      Branch("d", Leaf(), Leaf()),
      Branch("e", Leaf(), Leaf())),
    Branch("c",
      Branch("f", Leaf(), Leaf()),
      Branch("g", Leaf(), Leaf())))

  test("Full Tree") {
    assert(t3.v == "a")

    val t3id = addId(t3)
    assert(t3id == Branch(("a", 7),
      Branch(("b", 3),
        Branch(("d", 1), Leaf(), Leaf()),
        Branch(("e", 2), Leaf(), Leaf())),
      Branch(("c", 6),
        Branch(("f", 4), Leaf(), Leaf()),
        Branch(("g", 5), Leaf(), Leaf()))))
  }
}
