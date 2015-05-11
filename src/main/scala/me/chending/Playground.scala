package me.chending

import me.chending.ini.INIParser
import scala.actors._, Actor._

object Playground extends App {
    val parser: INIParser = new INIParser(InputHelper.from(getClass.getResource("/test.ini")))
    parser.start()

    println(parser.getValueMap)

    val badActor = actor {
        receive {
            case msg => println(msg)
        }
    }

    badActor ! "Hi!"
}
