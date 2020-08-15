package com.factual.reservoir

import scala.util.Random

// class ArrRes[A](val size: Int) {
//   private val poolSize: Long = 0L
//   private val reservoir: Array[A] = new Array[A](sampleSize)

//   def +(el: A): ArrRes()
// }
// http://erikerlandson.github.io/blog/2015/11/20/very-fast-reservoir-sampling/
// https://florian.github.io/reservoir-sampling/
// https://gist.github.com/mskimm/87e877bd21711dcf1fb8e4a2deed032d
// https://ballsandbins.wordpress.com/2014/04/13/distributedparallel-reservoir-sampling/

case class Reservoir[A](
  sampleSize: Int,
  samples: Vector[A] = Vector(),
  poolSize: Long = 0L,
  rand: Random = new Random()
) {

  def add(elem: A): Reservoir[A] = {
    if (samples.size < sampleSize - 1) {
      copy(samples=samples :+ elem, poolSize=poolSize + 1)
    } else {
      val replacementIndex = (rand.nextDouble() * poolSize).toInt
      if (replacementIndex < samples.size) {
        copy(samples=samples.updated(replacementIndex, elem), poolSize=poolSize + 1)
      } else {
        copy(poolSize=poolSize + 1)
      }
    }
  }

  def merge(other: Reservoir[A]): Reservoir[A] = {
    if ((samples.size + other.samples.size) <= sampleSize) {
      Reservoir(sampleSize, samples ++ other.samples, poolSize + other.poolSize, rand)
    } else {
      val left = samples
      val right = other.samples
      val total = poolSize + other.poolSize

      // prevProb * currProb = k / (m+n)
      // k/m * currProb = k / (m+n)

      // currProb(m) = m / (m + n)

      // if k > m

      // sample size: 5
      // a,b,c

      val leftPrevProb: Double = math.min(sampleSize / poolSize.toDouble, 1.0)
      val leftCurrProb: Double = (sampleSize / total.toDouble) / leftPrevProb
      //                5 / 6
      // val leftPortion = left.size / total.toDouble * 1 /
      //                1 / 1
      // val rightPortion = other.poolSize / total.toDouble

      // println(s"merging $left ($poolSize) - $leftPortion vs $right (${other.poolSize}) - $rightPortion")
      val alt = new scala.collection.mutable.ListBuffer[A]

      val leftPool = Random.shuffle(left)
      val rightPool = Random.shuffle(right)
      var leftIdx = 0
      var rightIdx = 0

      (0 until sampleSize).foreach { _ =>
        if (rand.nextDouble > leftCurrProb && rightIdx < right.size) {
          alt += rightPool(rightIdx)
          rightIdx += 1
        } else if (leftIdx < left.size) {
          alt += leftPool(leftIdx)
          leftIdx += 1
        }
        // if (rand.nextDouble < leftCurrProb && leftIdx < left.size) {
        //   alt += leftPool(leftIdx)
        //   leftIdx += 1
        // } else if (rightIdx < right.size) {
        //   alt += rightPool(rightIdx)
        //   rightIdx += 1
        // }
      }

      return Reservoir(sampleSize, alt.toVector, poolSize + other.poolSize, rand)

      // (0 until sampleSize).toVector.foreach { i =>
      //   if (leftIdx < left.size && rightIdx < right.size) {
      //     val r = rand.nextDouble
      //     // println(s"check $r against weighted ${(leftPortion / left.size)}")
      //     if (rand.nextDouble < (leftPortion / left.size)) {
      //       mergedSample += left(leftIdx)
      //       leftIdx += 1
      //     } else {
      //       mergedSample += right(rightIdx)
      //       rightIdx += 1
      //     }
      //   } else if (leftIdx < left.size) {
      //     mergedSample += left(leftIdx)
      //     leftIdx += 1
      //   } else if (rightIdx < right.size) {
      //     mergedSample += right(rightIdx)
      //     rightIdx += 1
      //   }
      // }

      // Reservoir(sampleSize, mergedSample.toVector, poolSize + other.poolSize, rand)
    }
  }
}
// (a,840)
// (b,833)
// (c,836)
// (d,830)
// (e,856)
// (f,575)


object Reservoir {
  def empty[A](sampleSize: Int): Reservoir[A] = Reservoir(sampleSize)
  def apply[A](sampleSize: Int, element: A): Reservoir[A] = {
    Reservoir(sampleSize, Vector(element), 1)
  }
}
