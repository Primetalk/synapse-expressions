Synapse expressions
===================

(the translation of the original article: https://habrahabr.ru/company/primetalk/blog/241567/ (Russian). The original article source is in https://github.com/Primetalk/synapse-expressions/blob/master/synapse-typed-expressions/docs/article_ru.html)

The library allows to create bidirectional transformations of a formal/natural language expressions between different abstraction levels. In particular - it is possible to create both a parser and a synthesizer from a single expression.

Famous ParserCombinator-s and Parboiled are only allow to parse formal languages. However, it seems to be reasonable to be able to parse natural language and also to be able to create phrases based on their semantics. It would be convenient to describe language constructs along with abstraction/concretization rules.

For instance,

* convert a numeral to a number ("ten" -> 10:Int)
* and vise versa (10:Int -> "ten", "tenth", "dicker")
* convert a numeral with a unit of service ("ten millimeters" <-> NumberWithMeasurement(10, MilliMeter))
* convert part of an address ("Apple street" <-> Address(street = "Apple"))
* convert in-town address ("Apple street, building 123, flat 45" <-> Address(street = "Apple", building=123, flat=45))
* convert phone number (256-00-21 ("two hundred fifty six, double O, twenty one") <-> NumericalSequence(256,"00", 21))

We also would like to have the following system's properties:

* single rule for both parsing and synthesizing;
* typesafety of the semantics at any abstraction level;
* direct ambiguity representation with a way to select one of the forms;
* word form concordance (for Russian, i.e.);
* composability of the patterns so that we would be able to construct secondary structures from the original rules.

In this README we describe numerals, however, the approach works for all of the above mentioned formal constructions.

Abstraction levels
------------------

The same semantics (content information) can take different forms at different abstraction levels. Let's see how a number 1234 can be presented:

* 1234:Int — at the level of program logic;
* Number(1234L)
* TwoClassNumber(Number(1L), Order(1000), Number(234L)) — separate thousands from ones
* Seq(Number(1L), Order(1000), Number(234L))
* Seq(Number(1L), Order(1000), TwoRangesNumber(Number(200L),100,Number(34L)))
* Seq(Number(1L), Order(1000), TwoRangesNumber(Number(200L),100,TwoRangesNumber(Number(30L),10,Number(4L))))
* Seq(Number(1L), Order(1000), TwoRangesNumber(Number(2L), Number(100L), 100,TwoRangesNumber(Number(30L),10,Number(4L))))
* Seq(Word(1L), Word(1000L), Word(2L), Word(100L), Word(30L), Word(4L)) — the sequence of number words identifiers
* Seq(one thousand, two, hundreds, thirty, four) — word form concordance rules applied
* «one thousand two hundreds thirty four» — final text.

This example illustrates the process of concretization of 1234:Int down to text. Every single step can be described by a substitution rule.

For parser implementation it is necessary to making systematic matching attempts against the same rules using either backtracking or CKY (CYK) parser, for instance.

The content or semantics stays the same at all levels. Only the representation form changes. And we have a selected direction on the axis of abstraction/concretization.

We may make the following observation. On the one hand, When we move upward (increasing abstraction level, parse), we throw away some details (word form, division of the number into parts, etc.). On the other hand, when we move downwards (synthesize, concretize), we need to augment the semantics with the absent details (the numerical form - ordinal or quantitative, word form, etc.). Where we get that information?

* partially we can use some hints in semantic along with the rules that handle the hints;
* partially we can use some sort of context and make the choise based on it;
* partially we can use nonlocal rules that consider adjucent elements to select the word form.

Grammar rules for numerals
--------------------------

Let's define the grammar rules for different number ranges:

    "The number from 1 to 9"                    := ID1   or  ID2   or  ... ID9
    "The number from 10 to 19"                  := ID10  or  ID11  or  ... ID19
    "The number from 1 to 19"                   := ID1   or  ID2   or  ... ID19
    "Tens from 20 to 90"                        := ID20  or  ID30  or  ... ID90
    "The one word number"                       := "Tens from 20 to 90" or "The number from 1 to 19"

The above rules work for numbers in the set {1, 2,… 20, 30,… 90} in a single word identifier. Identifiers are also marked with numbers and word form is not given. Our rules do not work for all numbers. When fed with inappropriate number we would get a failure. Also our rules cannot work with a few words yet.

Let's add rules that match two words:

    "Two word number" :=  ("Tens from 20 to 90" then "The number from 1 to 9")

To simplify further speculations lets shorten our rules:

* The numbers in range from 10 to 90 with step 10 we will represent with `[10..90/10]`.
* Instead of "or" we will use sign "|"
* Instead of "then" we will use sign "~"
* Before the rue name we will write keyword "val" and the rule definition after equals sign
* Instead of ID20 we'll use functional form id(20)

Thus we may represent our rules more concise:

	val `[0]`            = id(0L)
	val `[1..9]`         =   (1L to 9).map(id)
	val `[0..9]`         =  `[1..9]` | `[0]`
	val `[1..19]`        =   (1L to 19).map(id)
	val `[10..19]`       =   (10L to 19).map(id)
	val `[20..90/10]`    =   (20L to 90 by 10).map(id)
	val singleWordNumber =  `[20..90/10]` | `[1..19]`
	val twoWordNumber    =  `[20..90/10]` ~ `[1..9]`
        val `[1..99]`        = twoWordNumber | singleWordNumber

How to deal with the numerals that are composed from two words? From the syntactic point of view these are simply two expressions joined with `~`. However, from the abstraction point of view we have two ways of joining the numbers - addition ("twenty" and "one" = 20 + 1 = 21) and multiplication ("five" and "thousand" = 5 * 1000 = 5000). On the concretization level we may use division with reminder (21%10 = 1, 21-1=20) and division (5000/1000 = 5). Let's represent the translation between the abstraction levels with an additional operator ^^, that describes the way of transformation between the abstraction levels. For instance, we may represent numerals in the following way: 

	val `[20..99] without 20..90/10` = `[20..90/10]` ~ `[1..9]` ^^ ModSplit(10L)

Here we miss single word numerals. They differ a bit because the second part of the pair (ones) is missed and can be modelled with 0. In grammars the missed part can be represented with Epsilon, that matches an empty stream. On the higher abstraction level we replace the empty stream with the constant 0L. Let's denote this with a symbol `|?` :

	val `[20..99]` = `[20..90/10]` ~ (`[1..9]` |? 0L) ^^ ModSplit(10L)


The epression matches all numerals in the range from 20 to 99, and during synthesis allows to get one or two words.

To represent the range from 1 to 99 we may combine the expressions with `|`. However, what part should be selected during synthesis? To select proper part let's add a selector `LessThanSelector(20L)`, that chooses the right expression when the actual number is less than the threshold.

	val `[1..99]` = `[20..99]` | `[1..19]` selectBy LessThanSelector(20L)

To represent hundreds we need to split the single number into 3 parts - the amount of hundreds, word "hundred", and the number that is less than a hundred. To represent the operation we use `OrderSplit(100L)` marker. 

	val `[100..900/100]` = `[1..9]` ~ `[100]` ^^ OrderSplit(100L)
	val `[100..999]`     = `[100..900/100]` ~ (`[1..99]` |? 0L) ^^ ModSplit(100L)
	val `[1..999]`          = `[100..999]` | `[1..99]` selectBy LessThanSelector(100L)

Аналогичным образом представляются числа в диапазоне от 1 до 999.

	val `[100..999]` = `[100..900/100]` ~ (`[1..99]` |? 0L) ^^ ModSplit(100L)
	val `[1..999]`   = `[100..999]` | `[1..99]` selectBy LessThanSelector(100L) labelled "1..999"

The same applies to numbers greater than thousand:

	val `[1 000..999 000/1000]` = `[1..999]` ~ `[1 000]` ^^ OrderSplit(1000L)
	val `[1 000..999 999]`      = `[1 000..999 000/1000]` ~ (`[1..999]` |? 0L) ^^ ModSplit(1000L)
	val `[1..999 999]`          = `[1 000..999 999]` | `[1..999]` selectBy LessThanSelector(1000L)


To represent numbers in arbitraty range we may use the following recursive function:

    def range1To999Order(order:Long):NE = order match {
      case 1L => `[1..999]`
      case o if o>= 1000 =>
        val lower = range1To999Order(order/1000)
        val ordNE = order:NE
        val upper = `[1..999]` ~* ordNE
        ((upper ~+ lower) | upper selectBy OrderSplit(order)) | lower selectBy RangeSelector(order)
    }

As an `order` we put 1,000; 1,000,000; 1,000,000,000; etc..
 
Abstraction level translation rules
-----------------------------------

Different forms of the semantics can be represented with different data types. Thus we have to have two types of the expression that binds two abstraction levels. Concrete type (L — Lower) and abstract type (U — Upper):

    sealed trait Expression[L, U]

This type represents a grammar that corresponds to part of an L stream from the one side and an object U from the other side. We can consider the expression as a syntatic component of a rule. Subsequent transformation of the object to higher abstraction level is perfirmed by a function:

    sealed trait Transformer[M, U]

(it is a bidirectional function that can be considered Iso[M,U])

Combination of an expression with the transformer is also an expression:

    case class Transformed[L, M, U](e: Expression[L, M], t: Transformer[M, U]) extends Expression[L, U]

The transformer function brings in some semantic interpretation of the expression. That's why we call Transformer a semantic rule part.

When we try to do conversion one of the rule can fail. Thus the conversion function should be able to notify that the rule doesn't fit. For this purpose we may use one of the patterns Result/Option/Try/Either:

    sealed trait ParseResult[T]
    case class Success[T](value: T, tail:LemmaStream) extends ParseResult[T]
    case class Failure[T](reason: String) extends ParseResult[T]

The rules themselves are represented with a collection of Case-classes (Pair, BooleanAlternative, MapAlternative, ConstExpr, etc.) that are constructed by the above DSL.

Vocabulary representation
-------------------------

Parsing and synthesis require slightly different properties for the vocabulary. For parsing (increasing abstraction level) we bind with the word only that information that is necessary on higher abstraction levels. In particular, in most cases we only need the numerical value of the word.
However, for the synthesis purposes we need to know the morphological attributes of the word, so that we can find matching word. The morphological attributes can be presented with some grammatical categories like gender, number, case.

The grammatic category (https://en.wikipedia.org/wiki/Grammatical_category) and it's values («Grammemes») can be modelled in the following way:

    case object Gender extends GrammarCategory {
      val default = Masculine
      case object Masculine extends Grammem
      case object Femini extends Grammem
      case object Neuter extends Grammem
    }

Type Grammem is inner type of GrammarCategory, that has the advantage of compile-time type safety. In particular, whenever we need a grammem of the Neuter gender we simply declare expected type to be Genger.Grammem. Simultaneously we may use Gender as a key in the list of grammar categories.

The vocabulary is as simple as a collection of pairs — (textual form of the word, Grammatic form). The vocabulary is itself constructed with a tiny DSL that allows to capture the required details in a concise way:

  lazy val allWordForms = {
    associate("три четыре пять шесть семь восемь девять",
      3L to 9, new WordFormDescription(Masculine, Cardinal, Nominative, Ones)) :::
      associate("ноль",
        List(0L), new WordFormDescription(Cardinal, Nominative, Ones)) :::
      associate("одна две",
        List(1, 2), new WordFormDescription(Femini, Cardinal, Nominative, Ones)) :::
      associate("один два",
        List(1, 2), new WordFormDescription(Masculine, Cardinal, Nominative, Ones)) :::
      associate("одно",
        List(1), simpleNumericalWordForm(Ones, Neuter)) :::
      associate("десять одиннадцать двенадцать тринадцать четырнадцать " +
        "пятнадцать шестнадцать семнадцать восемнадцать девятнадцать",
        10L to 19, simpleNumericalWordForm(Teens)) :::
   ...
      associate("нулевого первого второго третьего четвёртого пятого шестого седьмого восьмого девятого",
        0L to 9, new WordFormDescription(Masculine, Ordinal, Genetive, Ones)) :::
   ...
      associateOrder("миллион миллиона миллионов", 1000000L) :::
   ...
      associate("минус", List(-1L), WordFormDescription.empty)

Text parsing
------------

The grammar can be either directly used to parse with some interpreter, or we can construct a derived parser by converting the grammar to the parser expressions. For parsing natural language we may use a probabilistic parser like CKY (CYK). For more formal languages we may work with backtracking parser.

### Backtracking parser

The piece of text is matched against the pattern given by the above rules. Is the pattern returns Success then we continue the parsing. However, if the rule fails we do "backtracking" step to find other alternative matches. When we use immutable data structures the latter step is pretty easy to do. We just need to ignore the failure, take the next parallel rule and continue the parsing. We do not need to fix mutable state.

### CKY-parser

For probabilistic CKY (CYK)-parser we need to convert grammar rules to a Chomsky normal form. The grammar in this case is a binary tree and it is feasible to iterate through all possible matches of the grammar with the word sequence. Different matches will be assigned some probabilities (that allows us to parse texts with replacement errors). It is useful to combine equivalent terminals (words) into "preterminals" (word sets).

Backtracking parser implementation
----------------------------------

For the purposes of the article we are fine with a backtracking-algorithm. Parser is a function that takes a stream of lemmas LemmaStream and return either parsed value and the stream tail or a failure. The tail is used for further parsing. If the match fails the backtracking algorithm throws the function and it's result away and tryes another parser. The back-tracking step is very easy to implement due to immutable data structures and availability of tail at every level.

The result of matching can be represented with type ParseResult[T] having two descendants: Success and Failure.

We can convert expressions to back-tracking parser with the function `backTrackingParser`:

    def backTrackingParser[U](e: Expression[LemmaStream, U]): Parser[U] = {
      implicit def uncheckedGenerics[T[_], O](t: T[_]): T[O] = t.asInstanceOf[T[O]]
      implicit def uncheckedGenerics2[T[_, _], O, P](t: T[_, _]): T[O, P] = t.asInstanceOf[T[O, P]]

      def backTrackingParser0(e: Expression[_, _]): Parser[_] = e match {
        case Epsilon(u) => s => Success(u, s)
        case ConstExpression(l: LemmaStream, u) => (s: LemmaStream) => startsWithAndTail(l, s).map(t => u)
        case Labelled(_, expr) => backTrackingParser0(expr)
        case Alternatives(expressions) =>
          val parsers = expressions.map(backTrackingParser0)
          (s: LemmaStream) =>
            parsers.
              map(parser => parser(s)).
              dropWhile(_.isFailure).
              headOption.getOrElse(Failure())
        case Pair(e1, e2) =>
          val parsers = List(e1, e2).map(backTrackingParser0)
          (s: LemmaStream) =>
            val res = parsers.foldLeft(Success[List[U]](Nil, s): ParseResult[List[U]])(_.next(_))
            res.map { lst => val list = lst.reverse; (list.head, list.tail.head).asInstanceOf[U]}
        case Transformed(innerExpression: Expression[_, _], t) =>
          val innerParser = backTrackingParser0(innerExpression)
          val converter = defaultSequencerHandler(t)
          (s: LemmaStream) =>
            innerParser(s).map(converter)
        case _ => throw new IllegalArgumentException(s"backTrackingParser0 is not implemented for expression $e")
      }
      uncheckedGenerics(backTrackingParser0(e))
    }

The parser takes lemma stream on input. To parse plain text we can convert the parser as follows:

  implicit def toSimpleParser[T](p:Parser[T]):SimpleParser[T] = (text:String) => {
    val res = p(text.split(" ").map(wordToLemma))
    if(res.tail.nonEmpty)
      throw new IllegalArgumentException(s"Cannot parse \"$text\"")
    res.value
  }


After this conversion we may use it directly to parse numerals.

	assert(parse("twenty seven millions three thousands two handred forty five") === 27003245L)


Text generation
---------------

To generate text from the same patterns we need to convert the patterns to text generator. We again generate not the final text but a lemma stream. After that we apply some concordance rules to select a word form.

Generator is a function that converts a value of type U to lemma stream. Convertion of patterns to generators is in the following method:

    def constructGenerator[U](tGen: TransformerGenerator[U], selGen: BooleanSelectorGenerator[U])(e: Expression[LemmaStream, U]): Generator[U] = {
      implicit def uncheckedGenerics[T[_], O](t: T[_]): T[O] = t.asInstanceOf[T[O]]
      implicit def uncheckedGenerics2[T[_, _], O, P](t: T[_, _]): T[O, P] = t.asInstanceOf[T[O, P]]
      def constructGenerator0(e: Expression[_, _]): Generator[Any] = e match {
        case ConstExpression(l: LemmaStream, u) => (t) => l
        case Labelled(_, e1) => constructGenerator0(e1)
        case Epsilon(_) => (t) => Iterable()
        case Pair(e1, e2) =>
          val g1 = constructGenerator0(e1)
          val g2 = constructGenerator0(e2)
          (u:(_, _)) => g1(u._1) ++ g2(u._2)
        case BooleanAlternative(sel: SemanticSelector[U], e1, e2) =>
          val selector = selGen(sel).asInstanceOf[Any => Boolean]
          val g1 = constructGenerator0(e1)
          val g2 = constructGenerator0(e2)
          (u) =>
            if (selector(u))
              g2(u)
            else
              g1(u)
        case MapAlternative(lst) =>
          val map = lst.map(c => (c.upper, c.lower)).toMap: Map[Any, LemmaStream]
          (u) =>
            map.getOrElse(u, throw new IllegalArgumentException(s"Cannot generate text for $u by $e"))
        case Transformed(innerExpression, t) =>
          val innerGen = constructGenerator0(innerExpression)
          val transformer = tGen(t)
          (u: U) => innerGen(transformer(u))
        case _ => throw new IllegalArgumentException(s"constructGenerator is not implemented for expression $e")
      }
      constructGenerator0(e)
    }


Word form selection
-------------------

To select proper word form for numeral it is enough to have a context of the length 1 (both right and left context). So the signature for the form selection function is as follows:

    def betterForm(lemma:LemmaInfo, left:Option[LemmaInfo], right:Option[LemmaInfo]):WordFormAssociation

The concordance rules are defined with pattern-matching over the triple — current lemma, right context, left context.

Conclusion
----------

Declarative grammar allows to describe patterns of a formal language elements like:

* numerals,
* addresses,
* phone numbers,
* codes of different forms,
* dates, times, intervals;
* etc.

Based on the declarative description we can derive parser and generator that perform the direct conversion of semantics to natural language text and vise versa.

P.S. See Numerals4.scala with examples for this article (in Russian, however).

