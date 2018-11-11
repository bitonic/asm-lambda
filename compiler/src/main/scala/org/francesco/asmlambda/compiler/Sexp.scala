package org.francesco.asmlambda.compiler

import java.util.ArrayList

import scala.annotation.{switch, tailrec}
import scala.collection.JavaConverters._
import scala.collection.mutable

import Syntax._

sealed trait ListKind {
  val name: String
}
object ListKind {
  case object Normal extends ListKind {
    val name: String = "list"
  }
  case object Vector extends ListKind {
    val name: String = "vector"
  }
  case object Map extends ListKind {
    val name: String = "map"
  }
  case object Set extends ListKind {
    val name: String = "set"
  }
}

sealed trait Sexp
sealed trait Atom extends Sexp
object Sexp {
  case class Var(v: String) extends Atom
  case class Scalar(v: Syntax.Scalar) extends Atom
  case class List(kind: ListKind, els: java.util.List[Sexp]) extends Sexp
}

case class ParseError(cursor: Int, topLevel: ArrayList[Sexp], context: List[(ListKind, ArrayList[Sexp])], msg: String)
    extends Throwable(msg)

final private class Reader(input: String) {
  private var cursor: Int = 0
  private val topLevel: ArrayList[Sexp] = new ArrayList[Sexp]()
  private var context: List[(ListKind, ArrayList[Sexp])] = List()

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

  def fail(s: String) = throw ParseError(cursor, topLevel, context, s)

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

  def close(acceptedKinds: Array[ListKind]): Unit = {
    val acceptedStr = acceptedKinds.map(_.name).mkString(" or ")
    context match {
      case Nil =>
        fail(s"Got closing character for $acceptedStr at the top level")
      case (kind, els) :: rest =>
        if (acceptedKinds.contains(kind)) {
          context = rest
          push(Sexp.List(kind, els))
        } else {
          fail(s"Got closing character for $acceptedStr, but I am in a ${kind.name}")
        }
    }
  }

  def enter(kind: ListKind): Unit = {
    val oldContext = context
    context = (kind, new ArrayList[Sexp]()) :: oldContext
  }

  def push(sexp: Sexp): Unit = context match {
    case Nil =>
      topLevel.add(sexp)
      ()
    case (_, els) :: _ =>
      els.add(sexp)
      ()
  }

  def isDigit(ch: Char): Boolean = ch >= '0' && ch <= '9'
  def isHex(ch: Char): Boolean = (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')

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
            stringBuilder ++= Character.toChars(Integer.parseInt(Array(u1, u2, u3, u4).mkString, 16))
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
        Set('!', '$', '%', '&', '*', '/', '<', '=', '>', '?', '~', '_', '^', '+', '-')
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
      case "nil" => Sexp.Scalar(Scalar.Nil)
      case "true" => Sexp.Scalar(Scalar.Bool(true))
      case "false" => Sexp.Scalar(Scalar.Bool(false))
      case s => Sexp.Var(s)
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
          enter(ListKind.Normal)
        case ')' =>
          close(Array(ListKind.Normal))
        case '[' =>
          enter(ListKind.Vector)
        case ']' =>
          close(Array(ListKind.Vector))
        case '{' =>
          enter(ListKind.Map)
        case '}' =>
          close(Array(ListKind.Map, ListKind.Set))
        case '#' =>
          expect('{')
          enter(ListKind.Set)
        case '"' =>
          cursor -= 1
          push(Sexp.Scalar(Scalar.Text(string())))
        case ':' =>
          val s = identifier()
          push(Sexp.Scalar(Scalar.Symbol(s)))
        case ';' =>
          eatUntilNewline()
        case _ =>
          if (isDigit(ch)) {
            cursor -= 1
            push(Sexp.Scalar(number()))
          } else if (ch == '+' || ch == '-') {
            // if there is a digit next it's a number, otherwise it's an identifier
            if (cursor < input.length) {
              val nextCh = input.charAt(cursor)
              if (isDigit(nextCh)) {
                cursor -= 1
                push(Sexp.Scalar(number()))
              } else {
                cursor -= 1
                push(identifierCheckScalar())
              }
            } else {
              push(Sexp.Var(ch + ""))
            }
          } else {
            cursor -= 1
            push(identifierCheckScalar())
          }
      }
      loop()
    } else {
      if (context.isEmpty) {
        ()
      } else {
        val toClose = context.map {
          case (kind, _) => kind match {
            case ListKind.Normal => ')'
            case ListKind.Vector => ']'
            case ListKind.Map => '}'
            case ListKind.Set => '}'
          }}.mkString
        fail(s"Unexpected end of input, still need to close $toClose")
      }
    }
  }
}

object Reader {
  def apply(input: String): mutable.ArraySeq[Sexp] = {
    val parser = new Reader(input)
    parser.loop()
    mutable.ArraySeq(parser.topLevel.asScala: _*)
  }
}