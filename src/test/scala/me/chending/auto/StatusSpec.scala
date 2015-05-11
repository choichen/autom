package me.chending.auto

import me.chending.UnitSpec

import scala.language.experimental.macros
import scala.language.implicitConversions

class StatusSpec extends UnitSpec {
    def CStatus = Status[Char] _

    "InitStatus[T]" should "be an instance of Status[T]" in {
        CStatus("Init").isInstanceOf[Status.Type[Char]] should be(right = true)
    }

    "Status change" should "be good" in {
        def CStatus = Status[Char] _
        implicit def str2status(name: String): StatusType[Char] = Status[Char](name)

        "Init" accept Set('(', '[') withHandler { (ch, c, t) => Unit } goto "ST1"
        "ST1" accept Set(')', ']') withHandler { (ch, c, t) => Unit } goto "End"

        StatusUtility.printTree(CStatus("Init"))

        CStatus("Init").isInstanceOf[Status.Type[Char]] should be(right = true)

    }
}
