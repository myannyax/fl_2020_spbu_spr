import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer

class GrammarErrorListener : BaseErrorListener() {
    companion object {
        @JvmStatic
        val INSTANCE = GrammarErrorListener()
    }

    override fun syntaxError(
        recognizer: Recognizer<*, *>?,
        offendingSymbol: Any?,
        line: Int,
        charPositionInLine: Int,
        msg: String?,
        e: RecognitionException?
    ) {
        throw ParserException("$line.$charPositionInLine : $msg")
    }
}