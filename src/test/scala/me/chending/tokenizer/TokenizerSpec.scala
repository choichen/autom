package me.chending.tokenizer

import me.chending.UnitSpec
import me.chending.tokenizer.Tokenizer.TokenPattern._

class TokenizerSpec extends UnitSpec {
    "IDENTIFIER" should "match all identifier formats" in {
        List("a", "a1", "_", "a_", "_a", "A", "a2_a3", "AbCde_333_2_d") foreach {
            _ matches IDENTIFIER should be(right = true)
        }
    }

    "IDENTIFIER" should "not match incorrect identifier formats" in {
        List("1a", "$x", "A1$2", "aAbB_@a") foreach {
            _ matches IDENTIFIER should be(right = false)
        }
    }

    "HEX_NUMBER" should "match all correct hex number formats" in {
        List("0x1a", "0X1a", "0x1A", "0x01", "0xdeadbeef") foreach {
            _ matches HEX_NUMBER should be(right = true)
        }
    }

    "HEX_NUMBER" should "not match incorrect hex number formats" in {
        List("0x1a~", "0X_1a", "0xx1A", "0Xx01", "00xdead_f") foreach {
            _ matches HEX_NUMBER should be(right = false)
        }
    }

    "OCT_NUMBER" should "match all correct oct number formats" in {
        List("00", "01", "077", "010") foreach {
            _ matches OCT_NUMBER should be(right = true)
        }
    }

    "OCT_NUMBER" should "not match incorrect oct number formats" in {
        List("0", "01x", "07_7", "08") foreach {
            _ matches OCT_NUMBER should be(right = false)
        }
    }

    "DEC_NUMBER" should "match all correct dec number formats" in {
        List("1", "+1", "-1", "0.33", "-0.33", "1021.122") foreach {
            _ matches DEC_NUMBER should be(right = true)
        }
    }

    "DEC_NUMBER" should "not match incorrect dec number formats" in {
        List("00", "+00", "-00", "+01", "-01", ".0123", "+.123", "211d3") foreach {
            _ matches DEC_NUMBER should be(right = false)
        }
    }

    "SPACES" should "match all correct spaces formats" in {
        List(" ", "\t", "\r", "\n", "\r\n\t") foreach {
            _ matches SPACES should be(right = true)
        }
    }

    "SPACES" should "not match incorrect spaces formats" in {
        List(" ( ", ",_") foreach {
            _ matches SPACES should be(right = false)
        }
    }

    "LINE_SEP" should "match all correct line separators" in {
        List("\r\n", "\r", "\n", "\n\r") foreach {
            _ matches LINE_SEP should be(right = true)
        }
    }

    "LINE_SEP" should "not match incorrect line separators" in {
        List("\r \n", " \r\n", "\r\n\n", "\n\r\n") foreach {
            _ matches LINE_SEP should be(right = false)
        }
    }

    "STRING_LITERAL" should "match all correct string literal formats" in {
        List("''", "\"\"", "'ab\"c'") foreach {
            _ matches STRING_LITERAL should be(right = true)
        }
    }

    "STRING_LITERAL" should "not match incorrect string literal formats" in {
        List("'", "\"'", "'", "\"", "\"\r\n\"") foreach {
            _ matches STRING_LITERAL should be(right = false)
        }
    }
}
