package ru.primetalk.typed.expressions

import scala.language.{higherKinds, implicitConversions}

/**
 * Parsers that can match a substring of a stream of words.
 *
 * @author zhizhelev, 21.10.14.
 */
trait Parsers extends Numerals4 {

  sealed trait ParseResult[+T] {
    def isSuccess: Boolean

    def isFailure = !isSuccess

    def orElse[U >: T](other: => ParseResult[U]): ParseResult[U]

    def value: T

    def flatMap[T2](f: (T) => ParseResult[T2]): ParseResult[T2]

    def map[T2](f: T => T2): ParseResult[T2]

    def tail: LemmaStream

    def next[E](parser: Parser[E])(implicit ev: T <:< List[E]): ParseResult[List[E]]
  }

  case class Success[+T](value: T, tail: LemmaStream) extends ParseResult[T] {
    def isSuccess = true

    def orElse[U >: T](other: => ParseResult[U]): ParseResult[U] = this

    def reason: String = throw new IllegalStateException("Success have no reason to fail")

    def flatMap[T2](f: (T) => ParseResult[T2]): ParseResult[T2] = f(value)

    def map[T2](f: T => T2): ParseResult[T2] = Success(f(value), tail)

    def next[E](parser: Parser[E])(implicit ev: T <:< List[E]): ParseResult[List[E]] =
      parser(tail).map(r => r :: value)
  }

  case class Failure[+T]() extends ParseResult[T] {
    def isSuccess = false

    def orElse[U >: T](other: => ParseResult[U]): ParseResult[U] = other

    def value: T = throw new IllegalStateException("No value")

    def tail: LemmaStream = throw new IllegalStateException("No tail")

    def flatMap[T2](f: (T) => ParseResult[T2]): ParseResult[T2] = Failure()

    def map[T2](f: T => T2): ParseResult[T2] = Failure()

    def next[E](parser: Parser[E])(implicit ev: T <:< List[E]): ParseResult[List[E]] = Failure()
  }

  object ParseResult {
    def success[T](value: T, tail: LemmaStream): ParseResult[T] = Success(value, tail)

    def fail[T]: ParseResult[T] = Failure()
  }

  /** A parser takes an expression and a stream of lemmas. Returns the
    * result of matching the expression over the beginning of the input stream.
    * tparam U - result type
    */
  type Parser[U] = LemmaStream => ParseResult[U]
  type ParserFactory[U] = Expression[LemmaStream, U] => Parser[U]
  type SimpleParser[T] = String => T

  /** checks that the stream starts with the prefix and return the
    * tail of the stream. */
  def startsWithAndTail(prefix: LemmaStream, it: LemmaStream): ParseResult[Boolean] =
    if (prefix.isEmpty) Success(true, it)
    else if (it.isEmpty || prefix.head != it.head) Failure()
    else
      startsWithAndTail(prefix.tail, it.tail)

  def startsWith[T](prefix: Iterable[T], it: Iterable[T]): Boolean =
    prefix.isEmpty || (
      it.nonEmpty &&
        prefix.head == it.head &&
        startsWith(prefix.tail, it.tail)
      )


  // combines the given arguments according to sequencer
  type SequencerHandler[U] = (Any) => (U, U) => U

  def defaultSequencerHandler(sequencer: Transformer[_, Long]): (Any => Long) = sequencer match {
    case ModSplit(_) => {
      case (l: Long, r: Long) => l + r
    }
    case OrderSplit(order) => {
      case (l: Long, o: Long) => l * o
    }
  }

  /** Helper for pattern matching different classes that represent alternatives. */
  object Alternatives {
    def unapply(a: Expression[_, _]): Option[Stream[Expression[_, _]]] = a match {
      case MapAlternative(lst) => Some(lst.toStream)
      case BooleanAlternative(_, e1, e2) => Some(Stream(e1, e2))
      case _ => None
    }
  }

  object Sequences {
    def unapply(a: Expression[_, _]): Option[List[Expression[_, _]]] = a match {
      case Pair(e1, e2) => Some(List(e1, e2))
      case _ => None
    }
  }

  def backTrackingParser[U](e: Expression[LemmaStream, U]): Parser[U] = {
    implicit def uncheckedGenerics[T[_], O](t: T[_]): T[O] = t.asInstanceOf[T[O]]
    implicit def uncheckedGenerics2[T[_, _], O, P](t: T[_, _]): T[O, P] = t.asInstanceOf[T[O, P]]

    def backTrackingParser0(e: Expression[_, _]): Parser[_] = e match {
      case Epsilon(u) => (s:LemmaStream) => Success(u, s)
      case ConstExpression(l, u) => (s: LemmaStream) => startsWithAndTail(l.asInstanceOf[LemmaStream], s).map(t => u)
      case Labelled(_, expr) => backTrackingParser0(expr)
      case Alternatives(expressions) =>
        val parsers = expressions.map(backTrackingParser0)
        (s: LemmaStream) =>
          parsers.
            map(parser => parser(s)).
            dropWhile(_.isFailure).
            headOption.getOrElse(Failure())
      case Sequences(expressions) =>
        val parsers = expressions.map(backTrackingParser0)
        (s: LemmaStream) =>
          parsers.
            foldLeft(ParseResult.success(List[U](), s))(_.next(_)).
            map {
            lst =>
              (lst.tail.head, lst.head).asInstanceOf[U]
          }
      case Transformed(innerExpression: Expression[_, _], t) =>
        val innerParser = backTrackingParser0(innerExpression)
        val converter = defaultSequencerHandler(t.asInstanceOf[Transformer[Any,Long]])
        (s: LemmaStream) =>
          innerParser(s).map(converter)
      case _ => throw new IllegalArgumentException(s"backTrackingParser0 is not implemented for expression $e")
    }
    uncheckedGenerics[Parser, U](backTrackingParser0(e))
  }

  def wordToLemma(word: String): Lemma

  implicit def toSimpleParser[T](p: Parser[T]): SimpleParser[T] = (text: String) => {
    val res = p(text.split(" ").map(wordToLemma))
    if (res.tail.nonEmpty)
      throw new IllegalArgumentException(s"Cannot parse '$text'")
    res.value
  }

}
