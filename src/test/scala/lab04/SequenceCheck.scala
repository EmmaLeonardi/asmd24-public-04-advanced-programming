package scala.lab04

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.lab04.Sequences.*
import scala.lab04.Sequences.Sequence.*


object SequenceCheck extends Properties("Sequence"):

  // define a recursive generator of lists, monadically
  def sequenceGen[A: Arbitrary](): Gen[Sequence[A]] = for
    i <- arbitrary[A]
    b <- Gen.prob(0.8)
    s <- if b then sequenceGen().map(s2 => Cons(i, s2)) else Gen.const(Nil())
  yield s

  // define custom arbitrary lists and mappers
  given intSeqArbitrary: Arbitrary[Sequence[Int]] = Arbitrary(sequenceGen[Int]())
  given mapperArbitrary: Arbitrary[Int => Int] = Arbitrary(Gen.oneOf[Int => Int]( _+1, _*2, x => x*x))
  given predicateArbitrary: Arbitrary[Int=>Boolean]= Arbitrary(Gen.oneOf[Int => Boolean]( _%2==0, _%3==0, _==0))

  // check axioms, universally
  property("mapAxioms") =
    forAll: (seq: Sequence[Int], f: Int => Int) =>
      //println(seq); println(f(10)) // inspect what's using
      (seq, f) match
        case (Nil(), f) =>  map(Nil())(f) == Nil()
        case (Cons(h, t), f) => map(Cons(h, t))(f) == Cons(f(h), map(t)(f))

  property("sumAxioms") =
    forAll: (seq: Sequence[Int])=>
      seq match
        case Nil() => seq.sum == 0
        case Cons(h, t) => seq.sum == h + sum(t)

  property("filterAxioms") =
    forAll: (seq: Sequence[Int], pred: Int => Boolean) =>
      (seq, pred) match
        case (Nil(), pred) => seq.filter(pred) == Nil()
        case (Cons(h, t), pred) if pred(h) => seq.filter(pred) == Cons(h, filter(t) (pred))
        case (Cons(h, t), pred) => seq.filter(pred) == filter(t)(pred)

  // how to check a generator works as expected
  @main def showSequences() =
    Range(0,20).foreach(i => println(summon[Arbitrary[Sequence[Int]]].arbitrary.sample))
