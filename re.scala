// Part 1 about Regular Expression Matching
//==========================================

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp   // alternative 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   // sequence
case class STAR(r: Rexp) extends Rexp             // star


// some convenience for typing in regular expressions

import scala.language.implicitConversions    
import scala.language.reflectiveCalls

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

implicit def RexpOps (r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps (s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}




// (1a) Complete the function nullable according to
// the definition given in the coursework; this
// function checks whether a regular expression
// can match the empty string

//@tailrec
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(c) => false
  case ALT(c,d) => nullable(c) || nullable(d)
  case SEQ(c,d) => nullable(c) && nullable(d)
  case STAR(c) => true
}
/*
def nullableT (r: Rexp, list: List[Rexp]) : Boolean = list match {
  case Nil => false
  case x :: xs =>

  case ZERO => false
  case ONE => true
  case CHAR(c) => false
  case ALT(c,d) => nullable(c) || nullable(d)
  case SEQ(c,d) => nullable(c) && nullable(d)
  case STAR(c) => true
}
*/
// (1b) Complete the function der according to
// the definition given in the coursework; this
// function calculates the derivative of a 
// regular expression w.r.t. a character

//@tailrec
def der (c: Char, r: Rexp) : Rexp = r match { //doesnt work for last der
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) =>
    if (c == d) ONE
    else ZERO
  case ALT(r1,r2) => der(c,r1) | der(c,r2)
  case SEQ(r1,r2) =>
    if (nullable(r1)) (der(c,r1) ~ r2) | der(c,r2)
    else der(c,r1) ~ r2 //
  case STAR(s) => der(c,s) ~ r
}

// (1c) Complete the function der according to
// the specification given in the coursework; this
// function simplifies a regular expression;
// however it does not simplify inside STAR-regular
// expressions

//@tailrec
((CHAR('a')|CHAR('b'))|CHAR('c') )  ~ ((CHAR('d')~ZERO) | (STAR(CHAR('e')~ZERO) | ONE))

simp(SEQ(ALT(ALT(CHAR('a'),CHAR('b')),CHAR('c')),ALT(SEQ(CHAR('d'),ZERO),ALT(STAR(SEQ(CHAR('e'),ZERO)),ONE))))


import scala.annotation.tailrec
@tailrec

//1: STACKOVERFLOW
def simp(r: Rexp) : Rexp = r match {
  case SEQ(r1, ZERO) => ZERO
  case SEQ(ZERO, r1) => ZERO
  case SEQ(r1, ONE) => simp(r1)
  case SEQ(ONE, r1) => simp(r1)
  case ALT(r1, ZERO) => simp(r1)
  case ALT(ZERO, r1) => simp(r1)
  case ALT(CHAR(a), CHAR(b)) =>
    if (CHAR(a) == CHAR(b)) CHAR(a)
    else ALT(CHAR(a), CHAR(b))
  case ALT(r1, r2) =>
    val x = simp(r1)
    val y = ALT(simp(r1), simp(r2))
    val z = simp(y)
    if (r1 == r2) x
    else if (ALT(r1, r2) == z) y
    else z
  case SEQ(CHAR(a), CHAR(b)) => SEQ(CHAR(a), CHAR(b))
  case SEQ(r1, r2) =>
    val x = SEQ(simp(r1), simp(r2))
    val y = simp(x)
    if (SEQ(r1, r2) == y) x
    else y
  case other => r
}




//2: STACKOVERFLOW
def simp(r: Rexp) : Rexp = r match {
  case SEQ(r1, ZERO) => ZERO
  case SEQ(ZERO, r1) => ZERO
  case SEQ(r1, ONE) => simp(r1)
  case SEQ(ONE, r1) => simp(r1)
  case ALT(r1, ZERO) => simp(r1)
  case ALT(ZERO, r1) => simp(r1)
  case ALT(CHAR(a), CHAR(b)) =>
    if (CHAR(a) == CHAR(b)) CHAR(a)
    else ALT(CHAR(a), CHAR(b))
  case ALT(r1, r2) =>
    if (r1 == r2) simp(r1)
    else if (ALT(r1, r2) == simp(ALT(simp(r1), simp(r2)))) ALT(simp(r1), simp(r2))
    else simp(ALT(simp(r1), simp(r2)))
  case SEQ(CHAR(a), CHAR(b)) => SEQ(CHAR(a), CHAR(b))
  case SEQ(r1, r2) =>
    if (SEQ(r1, r2) == simp(SEQ(simp(r1), simp(r2)))) SEQ(simp(r1), simp(r2))
    else simp(SEQ(simp(r1), simp(r2)))
  case other => r
}


//3: WORKS BUT NOT FOR:
//simp(SEQ(ALT(ALT(CHAR('a'),CHAR('b')),CHAR('c')),ALT(SEQ(CHAR('d'),ZERO),ALT(STAR(SEQ(CHAR('e'),ZERO)),ONE))))
//FOREVERONGOING

def simp(r: Rexp) : Rexp = r match {
  case SEQ(r1, ZERO) => ZERO
  case SEQ(ZERO, r1) => ZERO
  case SEQ(r1, ONE) => simp(r1)
  case SEQ(ONE, r1) => simp(r1)
  case ALT(r1, ZERO) => simp(r1)
  case ALT(ZERO, r1) => simp(r1)
  case ALT(CHAR(a), CHAR(b)) =>
    if (CHAR(a) == CHAR(b)) CHAR(a)
    else ALT(CHAR(a), CHAR(b))
  case ALT(r1, r2) =>
    if (r1 == r2) simp(r1)
    else simp(ALT(simp(r1), simp(r2))) //
  case SEQ(CHAR(a), CHAR(b)) => SEQ(CHAR(a), CHAR(b))
  case SEQ(r1, r2) =>
    else simp(SEQ(simp(r1), simp(r2))) //
  case other => r
}

/*
def simpT(r: Rexp, ) : Rexp = r match {
  case SEQ(r1, ZERO) => ZERO
  case SEQ(ZERO, r1) => ZERO
  case SEQ(r1, ONE) => simp(r1)
  case SEQ(ONE, r1) => simp(r1)
  case ALT(r1, ZERO) => simp(r1)
  case ALT(ZERO, r1) => simp(r1)
  case ALT(r1, r2) =>
    if (r1 == r2) simp(r1)
    else ALT(simp(r1),simp(r2))  //
  case SEQ(r1, r2) =>
    SEQ(simp(r1), simp(r2))
  case other => r
}
*/

//simp(ALT(ALT(CHAR('a'),ZERO),SEQ(CHAR('a'),ONE)))
//simp(ALT(ALT(CHAR('a'),ZERO),SEQ(CHAR('b'),ONE)))




// (1d) Complete the two functions below; the first 
// calculates the derivative w.r.t. a string; the second
// is the regular expression matcher taking a regular
// expression and a string and checks whether the
// string matches the regular expression

@tailrec
def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c :: cs => ders(cs, simp(der(c, r)))
}

def matcher(r: Rexp, s: String): Boolean = {
  nullable(ders (s.toList, r))
}

// (1e) Complete the function below: it searches (from the left to 
// right) in string s1 all the non-empty substrings that match the 
// regular expression -- these substrings are assumed to be
// the longest substrings matched by the regular expression and
// assumed to be non-overlapping. All these substrings in s1 are replaced
// by s2.

def replace(r: Rexp, s1: String, s2: String): String = {
  replaceT(r, s1, s2, s1.length, "")
}
import scala.annotation.tailrec
@tailrec
def replaceT(r: Rexp, s1: String, s2: String, endIndex: Int, stringToReturn: String): String = {
  if (matcher(r, s1.substring(0, endIndex))) {
    if (endIndex == s1.length) { //final case
      stringToReturn + s2
    }
    else {
      replaceT(r, s1.substring(endIndex, s1.length), s2, s1.length - endIndex, stringToReturn + s2)
    }
  }
  else { //does not match
    if (endIndex == 1) { //cannot be made shorter
      replaceT(r, s1.substring(1, s1.length), s2, s1.length - 1, stringToReturn + s1.substring(0, 1))
    }
    else {
      replaceT(r, s1, s2, endIndex - 1, stringToReturn)
    }
  }
}

// some testing data
// the supposedly 'evil' regular expression (a*)* b
/*
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
println(matcher(EVIL, "a" * 1000 ++ "b"))
println(matcher(EVIL, "a" * 1000))


def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}

for (i <- 1 to 5000001 by 500000) {
  println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL, "a" * i))))
}
*/


