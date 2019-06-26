package org.francesco.asmlambda.compiler

import scala.annotation.{switch, tailrec}

import Syntax._
import org.francesco.asmlambda.compiler.{ImmArray => IA}

sealed trait SeqKind {
  val name: String
}
object SeqKind {
  case object List extends SeqKind {
    val name: String = "list"
  }
  case object Vector extends SeqKind {
    val name: String = "vector"
  }
  case object Map extends SeqKind {
    val name: String = "map"
  }
  case object Pair extends SeqKind {
    val name: String = "pair"
  }
}

sealed trait Sexp
sealed trait Atom extends Sexp
object Sexp {
  case class Var(v: String) extends Atom
  case class Scalar(v: Syntax.Scalar) extends Atom
  case class Seq(kind: SeqKind, els: ImmArray[Sexp]) extends Sexp

  private def unapplyKindIA(kind: SeqKind, sexp: Sexp): Option[ImmArray[Sexp]] = sexp match {
    case Seq(`kind`, els) => Some(els)
    case _ => None
  }

  private def unapplyKindSeq(kind: SeqKind, sexp: Sexp): Option[scala.collection.Seq[Sexp]] =
    unapplyKindIA(kind, sexp).map(_.toSeq)

  object List {
    def unapply(sexp: Sexp): Option[ImmArray[Sexp]] = unapplyKindIA(SeqKind.List, sexp)
  }
  object ListSeq {
    def unapplySeq(sexp: Sexp): Option[scala.collection.Seq[Sexp]] =
      unapplyKindSeq(SeqKind.List, sexp)
  }

  object Vector {
    def unapply(sexp: Sexp): Option[ImmArray[Sexp]] = unapplyKindIA(SeqKind.Vector, sexp)
  }
  object VectorSeq {
    def unapplySeq(sexp: Sexp): Option[scala.collection.Seq[Sexp]] =
      unapplyKindSeq(SeqKind.Vector, sexp)
  }

  object Map {
    def unapply(sexp: Sexp): Option[ImmArray[Sexp]] = unapplyKindIA(SeqKind.Map, sexp)
  }
  object MapSeq {
    def unapplySeq(sexp: Sexp): Option[scala.collection.Seq[Sexp]] =
      unapplyKindSeq(SeqKind.Map, sexp)
  }

  object Pair {
    def unapply(sexp: Sexp): Option[ImmArray[Sexp]] = unapplyKindIA(SeqKind.Pair, sexp)
  }
  object PairSeq {
    def unapplySeq(sexp: Sexp): Option[scala.collection.Seq[Sexp]] =
      unapplyKindSeq(SeqKind.Pair, sexp)
  }
}

case class ReaderError(
    cursor: Int,
    topLevel: ImmArray[Sexp],
    context: List[(SeqKind, ImmArray[Sexp])],
    msg: String)
    extends Throwable(msg)

final private class Reader(input: String, allowDollarInIdentifier: Boolean = false) {
  private var cursor: Int = 0
  private val topLevel: IA.Builder[Sexp] = IA.newBuilder
  private var context: List[(SeqKind, IA.Builder[Sexp])] = List()

  @tailrec
  def eatWhitespace(): Unit = {
    if (cursor < input.length) {
      val codePoint = input.codePointAt(cursor)
      // we treat commas as whitespace, like in clojure.
      if (Character.isWhitespace(codePoint) || codePoint == ',') {
        cursor += Character.charCount(codePoint)
        eatWhitespace()
      }
    }
  }

  @tailrec
  def eatUntilNewline(): Unit = {
    if (cursor < input.length) {
      val ch = input.charAt(cursor)
      if (ch != '\n') {
        cursor += 1
        eatUntilNewline()
      } else {
        // consume the newline char
        cursor += 1
      }
    }
  }

  def fail(s: String) =
    throw ReaderError(cursor, topLevel.result(), context.map {
      case (lk, sexps) => (lk, sexps.result())
    }, s)

  def assertNoEnd(what: String): Unit = {
    if (cursor >= input.length) {
      fail(s"Unexpected end of input, expected $what")
    }
  }

  def expect(ch1: Char): Unit = {
    assertNoEnd("'" + ch1 + "'")
    val ch2 = input.charAt(cursor)
    if (ch1 != ch2) {
      fail("Expected char '" + ch1 + "', got " + ch2)
    }
    cursor += 1
  }

  def expectOptional(ch1: Char): Boolean = {
    if (cursor < input.length) {
      val ch2 = input.charAt(cursor)
      if (ch1 == ch2) {
        cursor += 1
        true
      } else {
        false
      }
    } else {
      false
    }
  }

  def satisfy(what: String, f: Char => Boolean): Char = {
    assertNoEnd(what)
    val ch = input.charAt(cursor)
    if (!f(ch)) {
      fail("Expected " + what + ", got '" + ch + "'")
    }
    cursor += 1
    ch
  }

  def satisfyOptional(f: Char => Boolean): Option[Char] = {
    if (cursor < input.length) {
      val ch = input.charAt(cursor)
      if (f(ch)) {
        cursor += 1
        Some(ch)
      } else {
        None
      }
    } else {
      None
    }
  }

  def close(acceptedKind: SeqKind): Unit = {
    context match {
      case Nil =>
        fail(s"Got closing character for one of ${acceptedKind.name} at the top level")
      case (kind, els) :: rest =>
        if (acceptedKind == kind) {
          context = rest
          push(Sexp.Seq(kind, els.result()))
        } else {
          fail(s"Got closing character for ${acceptedKind.name}, but I am in a ${kind.name}")
        }
    }
  }

  def enter(kind: SeqKind): Unit = {
    val oldContext = context
    context = (kind, IA.newBuilder[Sexp]) :: oldContext
  }

  def push(sexp: Sexp): Unit = context match {
    case Nil =>
      topLevel += sexp
      ()
    case (_, els) :: _ =>
      els += sexp
      ()
  }

  def isDigit(ch: Char): Boolean = ch >= '0' && ch <= '9'
  def isHex(ch: Char): Boolean =
    (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')

  def string(): String = {
    expect('"')
    val stringBuilder = new StringBuilder

    val escapable = Map('"' -> '"', '\\' -> '\\', 'n' -> '\n', 't' -> '\t')

    @tailrec
    def loop(): Unit = {
      // work with unicode codepoints rather than chars
      if (cursor < input.length) {
        val codePoint = input.codePointAt(cursor)
        cursor += Character.charCount(codePoint)
        if (codePoint == '"') {
          return
        }
        if (codePoint == '\\') {
          val escaped = satisfy("escaped character", ch => escapable.contains(ch) || ch == 'u')
          if (escaped == 'u') {
            val u1 = satisfy("hexadecimal digit for unicode sequence", isHex)
            val u2 = satisfy("hexadecimal digit for unicode sequence", isHex)
            val u3 = satisfy("hexadecimal digit for unicode sequence", isHex)
            val u4 = satisfy("hexadecimal digit for unicode sequence", isHex)
            stringBuilder ++= Character.toChars(
              Integer.parseInt(Array(u1, u2, u3, u4).mkString, 16))
          } else {
            stringBuilder += escapable(escaped)
          }
        } else {
          stringBuilder ++= Character.toChars(codePoint)
        }
        loop()
      }
    }
    loop()

    stringBuilder.mkString
  }

  def number(): Syntax.Scalar = {
    val stringBuilder = new StringBuilder()

    @tailrec
    def eatDigits(): Unit = {
      satisfyOptional(isDigit) match {
        case None => ()
        case Some(ch) =>
          stringBuilder += ch
          eatDigits()
      }
    }

    val digitOrSign = satisfy("digit or sign", ch => isDigit(ch) || ch == '+' || ch == '-')
    stringBuilder += digitOrSign
    if (digitOrSign == '+' || digitOrSign == '-') {
      stringBuilder += satisfy("digit", isDigit)
    }
    eatDigits()

    if (expectOptional('.')) {
      stringBuilder += '.'
      stringBuilder += satisfy("digit", isDigit)
      eatDigits()
      Scalar.F64(stringBuilder.mkString.toDouble)
    } else {
      Scalar.I64(stringBuilder.mkString.toLong)
    }
  }

  val identifierInitial: Set[Char] =
    Set('a' to 'z': _*) ++
      Set('A' to 'Z': _*) ++
      Set('!', '%', '&', '*', '/', '<', '=', '>', '?', '~', '_', '^', '+', '-') ++
      (if (allowDollarInIdentifier) { Set('$') } else { Set.empty })
  def identifier(): String = {
    val stringBuilder = new StringBuilder()
    stringBuilder += satisfy("initial identifier character", identifierInitial.contains)

    @tailrec
    def loop(): Unit = {
      satisfyOptional(ch => identifierInitial.contains(ch) || isDigit(ch)) match {
        case None => ()
        case Some(ch) =>
          stringBuilder += ch
          loop()
      }
    }
    loop()

    stringBuilder.mkString
  }

  def identifierCheckScalar(): Atom = {
    identifier() match {
      case "true" => Sexp.Scalar(Scalar.Bool(true))
      case "false" => Sexp.Scalar(Scalar.Bool(false))
      case s => Sexp.Var(s)
    }
  }

  val atomCategories: Set[Byte] = Set(
    Character.UPPERCASE_LETTER,
    Character.LOWERCASE_LETTER,
    Character.TITLECASE_LETTER,
    Character.MODIFIER_LETTER,
    Character.OTHER_LETTER,
    Character.NON_SPACING_MARK,
    Character.DECIMAL_DIGIT_NUMBER,
    Character.LETTER_NUMBER,
    Character.OTHER_NUMBER,
    Character.CONNECTOR_PUNCTUATION,
    Character.DASH_PUNCTUATION,
    Character.OTHER_PUNCTUATION,
    Character.MATH_SYMBOL,
    Character.CURRENCY_SYMBOL,
    Character.MODIFIER_LETTER,
    Character.OTHER_SYMBOL,
  )
  def atom(): Atom = {
    val stringBuilder = new StringBuilder()

    @tailrec
    def loop(): Unit = {
      if (cursor < input.length) {
        val codePoint = input.codePointAt(cursor)
        val category = Character.getType(codePoint)
        if (!(codePoint == ',' || codePoint == ';') && atomCategories.contains(category.toByte)) {
          stringBuilder ++= Character.toChars(codePoint)
          cursor += Character.charCount(codePoint)
          loop()
        }
      }
    }
    loop()

    val s = stringBuilder.mkString
    s match {
      case "true" => Sexp.Scalar(Scalar.Bool(true))
      case "false" => Sexp.Scalar(Scalar.Bool(false))
      case _ =>
        try {
          Sexp.Scalar(Scalar.I64(s.toLong))
        } catch {
          case _: NumberFormatException =>
            try {
              Sexp.Scalar(Scalar.F64(s.toDouble))
            } catch {
              case _: NumberFormatException => Sexp.Var(s)
            }
        }
    }
  }

  @tailrec
  def loop(): Unit = {
    eatWhitespace()
    if (cursor < input.length) {
      val ch = input.charAt(cursor)
      cursor += 1
      (ch: @switch) match {
        case '(' =>
          enter(SeqKind.List)
        case ')' =>
          close(SeqKind.List)
        case '[' =>
          enter(SeqKind.Vector)
        case ']' =>
          close(SeqKind.Vector)
        case '{' =>
          enter(SeqKind.Map)
        case '}' =>
          close(SeqKind.Map)
        case '<' =>
          enter(SeqKind.Pair)
        case '>' =>
          close(SeqKind.Pair)
        case '"' =>
          cursor -= 1
          push(Sexp.Scalar(Scalar.Text(string())))
        case ';' =>
          eatUntilNewline()
        case _ =>
          cursor -= 1
          push(atom())
      }
      loop()
    } else {
      if (context.isEmpty) {
        ()
      } else {
        val toClose = context.map {
          case (kind, _) =>
            kind match {
              case SeqKind.List => ')'
              case SeqKind.Vector => ']'
              case SeqKind.Map => '}'
              case SeqKind.Pair => '>'
            }
        }.mkString
        fail(s"Unexpected end of input, still need to close $toClose")
      }
    }
  }
}

object Reader {
  def apply(input: String, allowDollarInIdentifier: Boolean = false): ImmArray[Sexp] = {
    val parser = new Reader(input, allowDollarInIdentifier)
    parser.loop()
    parser.topLevel.result()
  }
}
