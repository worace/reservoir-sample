package com.factual.reservoir

import scala.util.Random

class ReservoirSampleTest extends munit.FunSuite {
  val alpha = ('a' to 'z').map(_.toString)

  implicit class Freqs[A](v: Seq[A]) {
    def frequencies: Map[A, Int] =
      v.groupBy(el => el)
        .map { case (k,g) => (k, g.size) }
  }

  test("merging small samples concats") {
    val r = Reservoir(3, "a")
    val r2 = Reservoir(3, "b")
    val r3 = r.merge(r2)
    assertEquals(r3.poolSize, 2L)
    assertEquals(r3.samples, Vector("a", "b"))
  }

  test("adding single el") {
    val r = Reservoir(3, "a")
    val r2 = r.add("b")
    assertEquals(r2.poolSize, 2L)
    assertEquals(r2.samples, Vector("a", "b"))
  }

  test("merging above limit accumulates up to desired size") {
    val alpha = ('a' to 'z').map(_.toString)
    val merged = alpha
      .map { letter => Reservoir(5, letter) }
      .reduce(_.merge(_))

    assertEquals(merged.poolSize, 26L)
    assertEquals(merged.samples.size, 5)
    // assert(merged.samples.toSet.subsetOf(jsalpha))
    // println(merged.samples)
  }

  def alphaAddTest: Reservoir[String] =
    alpha.foldLeft(Reservoir.empty[String](3))(_.add(_))

  test("adding elements in turn distribution") {
    val r = alphaAddTest
    assertEquals(r.poolSize, 26L)
    assertEquals(r.sampleSize, 3)

    (0 to 1000)
      .map(_ => alphaAddTest)
      .flatMap(_.samples)
  }

  test("merge scenarios".only) {
    val rand = new Random(1L)

    // val r1 = Reservoir(3, Vector("a", "b", "c"), 3, rand)
    // val r2 = Reservoir(3, Vector("d", "e", "f"), 1000000000L, rand)

    // assertEquals(Set("a", "b", "c"), r1.merge(r2).samples.toSet)

    // val r3 = Reservoir(3, Vector("a", "b", "c"), 3, rand)
    // val r4 = Reservoir(3, Vector("d", "e", "f"), 3, rand)

    // assertEquals(Set("d", "e", "f"), r3.merge(r4).samples.toSet)

    val r5 = Reservoir(3, Vector("a", "b", "c"), 5, rand)
    val r6 = Reservoir(3, Vector("d", "e", "f"), 25, rand)

    assertEquals(Set("b", "c", "f"), r5.merge(r6).samples.toSet)
  }

  test("expected distribution") {
    def alphaTest: Reservoir[String] = {
      alpha
        .take(6)
        .map { letter => Reservoir(5, letter) }
        .reduce(_.merge(_))
    }

    // val pools = (0 to 1000).map(_ => alphaTest).map(_.poolSize).distinct
    // assertEquals(pools, Vector(6L))

    // val sampleSizes = (0 to 1000).map(_ => alphaTest).map(_.samples.size).distinct
    // assertEquals(sampleSizes, Vector(5))

    val freqs = (0 to 10000)
      .map(_ => alphaTest)
      .flatMap(_.samples)
      .frequencies

    freqs
      .toVector
      .sortBy(_._1)
      .foreach(println)
  }
}
// (a,837)
// (b,839)
// (c,832)
// (d,835)
// (e,834)
// (f,595)







