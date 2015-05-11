package me.chending

import java.io.{File, InputStream}
import java.net.URL

import scala.io.Source

object InputHelper {

    class SourceWrapper(val source: Source) {
        private var index2read = 0
        private val content = source.getLines().mkString("\n")

        def hasNext: Boolean = index2read < content.length

        def next(): Char = if (hasNext) {
            index2read += 1
            content.charAt(index2read - 1)
        } else '\0'
    }

    def from(source: Source) = new SourceWrapper(source)

    def from(url: URL): SourceWrapper = from(Source.fromURL(url))

    def from(filePath: String): SourceWrapper = from(Source.fromFile(filePath))

    def from(file: File): SourceWrapper = from(Source.fromFile(file))

    def from(stream: InputStream): SourceWrapper = from(Source.fromInputStream(stream))

}
