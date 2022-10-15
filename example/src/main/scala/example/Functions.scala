package example

import scala.annotation.tailrec

def sqrt(x: Double): Double =
  @tailrec
  def sqrtIter(guess: Double): Double =
    if isGoodEnough(guess) then guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double): Boolean =
    abs(square(guess) - x) < 0.001

  def improve(guess: Double): Double = (guess + x / guess) / 2

  def square(x: Double): Double = x * x

  def abs(x: Double): Double = if (x < 0) -x else x

  sqrtIter(1.0)


def factorial(n: Int): Int =

  @tailrec
  def factorialIter(i: Int, plur: Int): Int =
    if i == 0 then plur else factorialIter(i-1, i * plur)

  if n == 0 then 1 else factorialIter(n, 1)

@main def test(): Unit = println(sqrt(2))

@main def test_factorial(): Unit = println(factorial(4))