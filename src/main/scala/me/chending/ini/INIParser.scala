package me.chending.ini

import me.chending.InputHelper.SourceWrapper
import me.chending.auto.{Status, StatusType}

import scala.collection.{immutable, mutable}
import scala.language.{implicitConversions, postfixOps}

class INIParser(val inputSource: SourceWrapper) {
    protected val mInputSource = inputSource
    protected val mBuffer = new StringBuilder
    protected val init = "Init"
    protected val tagName = "TagName"
    protected val end = "End"
    protected val waitForPropertyName = "WaitForPropertyName"
    protected val readPropertyName = "ReadPropertyName"
    protected val waitForPropertyValue = "WaitForPropertyValue"
    protected val readPropertyValue = "ReadPropertyValue"

    type CStatus = StatusType[Char]
    type Handler = StatusType[Char]#OnAcceptInputHandler

    protected val mIniMap: mutable.HashMap[String, mutable.HashMap[String, String]] = new mutable.HashMap()

    def start() = {
        Parser.start()
    }

    def getValueMap = immutable.HashMap(mIniMap map {
        tuple =>
            (tuple._1, immutable.HashMap(tuple._2.toList: _*))
    } toList: _*)

    object Parser {
        private var currentTag: String = null
        private var currentProp: String = null

        implicit def str2status(name: String): StatusType[Char] = Status[Char](name)

        def start() = {
            def _internal_loop(currentState: CStatus, input: Char): Unit = {
                currentState match {
                    case StatusType(`end`) =>
                    case _ =>
                        _internal_loop(currentState.onAccept(input), mInputSource.next())
                }
            }
            _internal_loop(init, mInputSource.next())
        }

        protected val onTagNameStart: Handler =
            (char: Char, from: CStatus, to: CStatus) => {
                mBuffer.clear()
            }

        protected val onReadTagName: Handler =
            (char: Char, from: CStatus, to: CStatus) => {
                mBuffer append char
            }

        protected val onTagNameEnd: Handler =
            (char: Char, from: CStatus, to: CStatus) => {
                currentTag = mBuffer.toString()
                mBuffer.clear()
            }

        protected val onPropertyNameStart: Handler =
            (char: Char, from: CStatus, to: CStatus) => {
                mBuffer.clear()
                mBuffer append char
            }

        protected val onReadPropertyName: Handler =
            (char: Char, from: CStatus, to: CStatus) => {
                mBuffer append char
            }

        protected val onPropertyNameEnd: Handler =
            (char: Char, from: CStatus, to: CStatus) => {
                currentProp = mBuffer.toString()
                mBuffer.clear()
            }

        protected val onPropertyValueStart: Handler =
            (char: Char, from: CStatus, to: CStatus) => {
                mBuffer.clear()
                mBuffer append char
            }

        protected val onReadPropertyValue: Handler =
            (char: Char, from: CStatus, to: CStatus) => {
                mBuffer append char
            }

        protected val onPropertyValueEnd: Handler =
            (char: Char, from: CStatus, to: CStatus) => {
                val value = mBuffer.toString()
                assert(currentTag != null)
                assert(currentProp != null)

                val map = mIniMap.getOrElseUpdate(currentTag, new mutable.HashMap())
                map.update(currentProp, value)
                mBuffer.clear()
            }

        protected val onInputTerminated: Handler =
            (char: Char, from: CStatus, to: CStatus) => {
                from match {
                    case StatusType(`waitForPropertyName`) =>
                    case StatusType(`readPropertyValue`) =>
                        onPropertyValueEnd(char, from, to)
                    case _ =>
                        throw new IllegalStateException
                }
                println("Parse end")
            }

        protected val onError: Handler =
            (char: Char, from: CStatus, to: CStatus) => {
                printf("Error: input: %c, from Status[%s] to Status[%s]\n", char, from.name, to.name)
            }

        protected val onSkip: Handler =
            (char: Char, from: CStatus, to: CStatus) => {}

        val identifier = ('a' to 'z').toSet ++ ('A' to 'Z').toSet ++ ('0' to '9').toSet + '_'
        val lineBreaks = "\r\n".toSet
        val spaces = " \t\f".toSet
        val spacesAndLineBreaks: Set[Char] = " \t\r\n\f".toSet
        val terminate = '\0'


        init accept '[' withHandler onTagNameStart goto tagName
        init accept spacesAndLineBreaks withHandler onSkip goto init
        init acceptDefaultWithHandler onError goto end

        tagName accept identifier withHandler onReadTagName goto tagName
        tagName accept ']' withHandler onTagNameEnd goto waitForPropertyName
        tagName acceptDefaultWithHandler onError goto end

        waitForPropertyName accept identifier withHandler onPropertyNameStart goto readPropertyName
        waitForPropertyName accept '[' withHandler onTagNameStart goto tagName
        waitForPropertyName accept spacesAndLineBreaks withHandler onSkip goto waitForPropertyName
        waitForPropertyName accept terminate withHandler onInputTerminated goto end
        waitForPropertyName acceptDefaultWithHandler onError goto end

        readPropertyName accept identifier ++ spaces withHandler onReadPropertyName goto readPropertyName
        readPropertyName accept '=' withHandler onPropertyNameEnd goto waitForPropertyValue
        readPropertyName acceptDefaultWithHandler onError goto end

        waitForPropertyValue accept spaces withHandler onSkip goto waitForPropertyValue
        waitForPropertyValue accept lineBreaks withHandler onPropertyValueEnd goto waitForPropertyName
        waitForPropertyValue acceptDefaultWithHandler onPropertyValueStart goto readPropertyValue

        readPropertyValue acceptDefaultWithHandler onReadPropertyValue goto readPropertyValue
        readPropertyValue accept lineBreaks withHandler onPropertyValueEnd goto waitForPropertyName
        readPropertyValue accept terminate withHandler onInputTerminated goto end
    }

}
