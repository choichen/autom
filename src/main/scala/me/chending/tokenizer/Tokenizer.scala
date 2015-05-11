package me.chending.tokenizer

import me.chending.auto.{Status, StatusType}

import scala.language.implicitConversions
import scala.util.matching.Regex

object Tokenizer {

    case class Token(literal: String, lineno: Int, index: Int) {
        def this(literal: String) = this(literal, -1, -1)

        def combine(another: Token): Token =
            new Token(literal + another.literal, lineno, index)
    }

    object TokenPattern extends Enumeration {
        type TokenPattern = Value

        protected case class Val(regex: Regex) extends super.Val

        implicit def value2TokenPatternVal(v: Value): Val = v.asInstanceOf[Val]

        implicit def tokenPattern2String(tokenPattern: Val): String = tokenPattern.regex.toString()

        val IDENTIFIER = Val( """\A[a-zA-Z_][a-zA-Z0-9_]*\z""".r)
        val HEX_NUMBER = Val( """\A[-+]?0[xX][0-9a-fA-F]+\z""".r)
        val OCT_NUMBER = Val( """\A[-+]?0[0-7]+\z""".r)
        val DEC_NUMBER = Val( """\A[-+]?(?:(?:0(?:[.][0-9]+)?)|(?:[1-9][0-9]*(?:[.][0-9]+)?))\z""".r)
        val SPACES = Val( """\s+""".r)
        val LINE_SEP = Val( """\r\n|\r|\n|\n\r""".r)
        val EQ = Val( """\A==\z""".r)
        val BIGGER = Val( """\A>\z""".r)
        val BIGGER_EQ = Val( """\A>=\z""".r)
        val LOWER = Val( """\A<\z""".r)
        val LOWER_EQ = Val( """\A<=\z""".r)
        val STRING_LITERAL = Val( """\A\".*\"|'.*'\z""".r)
    }

    def onAccept(char: Char, current: StatusType[Char], target: StatusType[Char]) =
        printf("Input is %c, current status: %s, target: %s\n", char, current.name, target.name)

    val numbers = ('0' to '9').toSet
    val numbersWithDot = numbers + '.'
    val letters = ('a' to 'z').toSet ++ ('A' to 'Z').toSet
    val separator_punctuations = "(){}[]\"';:~*`!@ \t%$^#|?.,".toSet

    implicit def string2Status(name: String): StatusType[Char] = Status[Char](name)

    // State: `Ready` accepts every possible input
    "Ready" accept numbers withHandler onAccept goto "Number"
    "Ready" accept letters + '_' withHandler onAccept goto "Identifier"
    "Ready" accept separator_punctuations withHandler onAccept goto "Ready"

    // end of state `Ready`

    // State: `Number`
    "Number" accept numbersWithDot withHandler onAccept goto "Number"

    // end of state `Number


    def isSeparator(char: Char) = """(){}[]\t :;>=<'"\""" contains char

    def isLetter(char: Char) = (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')

    def isDigit(char: Char) = char >= '0' && char <= '9'

    def isIdentifierChar(char: Char) = isDigit(char) || isLetter(char) || char == '_'

    def isSpace(char: Char) = "\r\n\t\f" contains char

}
