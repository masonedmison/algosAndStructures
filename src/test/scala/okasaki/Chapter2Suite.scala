package okasaki

import weaver.SimpleIOSuite
import chapter2.{UnbalancedSet, Tree}

object Chapter2Suite extends SimpleIOSuite {
  val ubs = new UnbalancedSet[String]()

  pureTest("create should be balanced (m = 3).") {
    val out = ubs.create("hello")(3)

    val expected = Tree.Branch(
      Tree.Branch(Tree.E, "hello", Tree.E),
      "hello",
      Tree.Branch(Tree.E, "hello", Tree.E)
    )

    expect.same(expected, out)
  }

  pureTest("create should be balanced (m = 4") {
    val out = ubs.create("hello")(4)

    val expected = Tree.Branch(
      Tree.Branch(
        Tree.Branch(Tree.E, "hello", Tree.E),
        "hello",
        Tree.E
      ),
      "hello",
      Tree.Branch(Tree.E, "hello", Tree.E)
    )

    expect.same(expected, out)
  }

  pureTest("create should be balanced (m = 5") {
    val out = ubs.create("hello")(5)

    expect(ubs.isBalanced(out))
  }
}
