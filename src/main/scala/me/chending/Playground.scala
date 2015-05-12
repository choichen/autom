package me.chending

import me.chending.ini.INIParser

import scala.io.Source

object Playground extends App {
    val parser: INIParser = new INIParser(Source.fromURL(getClass.getResource("/test.ini")))
    parser.start()

    println(parser.getValueMap)
}
