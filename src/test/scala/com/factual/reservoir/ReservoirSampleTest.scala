package com.factual.reservoir

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

  test("expected distribution".only) {
    def alphaTest: Reservoir[String] = {
      alpha
        .take(6)
        .map { letter => Reservoir(5, letter) }
        .reduce(_.merge(_))
    }

    val pools = (0 to 1000).map(_ => alphaTest).map(_.poolSize).distinct
    assertEquals(pools, Vector(6L))

    val sampleSizes = (0 to 1000).map(_ => alphaTest).map(_.samples.size).distinct
    assertEquals(sampleSizes, Vector(5))

    val freqs = (0 to 1000)
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







