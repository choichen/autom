package me.chending

import me.chending.ini.INIParser

object Playground extends App {
    val parser: INIParser = new INIParser(InputHelper.from(getClass.getResource("/test.ini")))
    parser.start()

    println(parser.getValueMap)
}
