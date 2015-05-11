package me.chending.tokenizer

import java.io.{File, FileNotFoundException}

import me.chending.{InputHelper, UnitSpec}
import InputHelper.SourceWrapper

import scala.io.Source
import scala.language.postfixOps

class InputHelperSpec extends UnitSpec {
    val resource = "/source.n"
    "InputHelper" should "return Source object from with InputStream" in {
        val source = InputHelper.from(getClass.getResourceAsStream(resource))
        source.isInstanceOf[SourceWrapper] should be(right = true)
    }

    "InputHelper" should "throw FileNotFoundException with non-exist file" in {
        a[FileNotFoundException] should be thrownBy {
            InputHelper.from("/non-exist")
        }
    }

    "InputHelper" should "return Source object with file URL" in {
        val source = InputHelper.from(getClass.getResource(resource))
        source.isInstanceOf[SourceWrapper] should be(right = true)
    }

    "InputHelper" should "return Source object with file path" in {
        val source = InputHelper.from(getClass.getResource(resource).getPath)
        source.isInstanceOf[SourceWrapper] should be(right = true)
    }

    "InputHelper" should "return Source object with file object" in {
        val source = InputHelper.from(getClass.getResource(resource).getFile)
        source.isInstanceOf[SourceWrapper] should be(right = true)
    }

    "InputHelper" should "throw FileNotFoundException with file object which refers to non-exist file" in {
        a[FileNotFoundException] should be thrownBy {
            val source = InputHelper.from(new File("/non-exist"))
        }
    }
}
