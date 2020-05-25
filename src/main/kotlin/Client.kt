import org.antlr.v4.runtime.CharStream
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.nio.file.NoSuchFileException
import kotlin.system.exitProcess

fun parse(charStream: CharStream): AST {
    val lexer = GrammarLexer(charStream)
    lexer.removeErrorListeners()
    lexer.addErrorListener(GrammarErrorListener.INSTANCE)

    val commonTokenStream = CommonTokenStream(lexer)

    val parser = GrammarParser(commonTokenStream)
    parser.removeErrorListeners()
    parser.addErrorListener(GrammarErrorListener.INSTANCE)

    val parseTree = parser.program()
    val visitor = MyVisitor()
    return visitor.visit(parseTree)
}

class Client {
    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            if (args.isEmpty()) {
                print("Pass filename as arg")
                exitProcess(1)
            }
            val file = args[0]
            val charStream: CharStream?
            try {
                charStream = CharStreams.fromFileName(file)
            } catch (e: NoSuchFileException) {
                println("No such file")
                exitProcess(1)
            }
            try {
                val ast = parse(charStream)
                println("Parse result:\n$ast")
                println()
            } catch (e: ParserException) {
                println("Parser error at " + e.message)
            }
        }
    }
}