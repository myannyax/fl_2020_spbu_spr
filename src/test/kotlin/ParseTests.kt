import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.junit.Test

class ParseTests {
    private val FILENAME = "src/test/resources/test"

    @Test
    fun `base test!`() {
        try {
            testHelper(FILENAME)
        } catch (e: Exception) {
            assertTrue(e.message, false)
        }
    }

    @Test
    fun `test 2`() {
        val ast = testHelper("${FILENAME}2")
        assertEquals(Program(Relations(emptyList()), null), ast)
    }

    @Test
    fun `test 3`() {
        try {
            testHelper("${FILENAME}3")
        } catch (e: Exception) {
            return
        }
        assert(false)
    }

    @Test
    fun `test 4`() {
        try {
            testHelper("${FILENAME}4")
        } catch (e: Exception) {
            return
        }
        assert(false)
    }

    @Test
    fun `test 5`() {
        val ast = testHelper("${FILENAME}5")
        assertEquals(
            Program(
                Relations(
                    listOf(
                        Relation(
                            Head(
                                Atom(
                                    Ident("eval"),
                                    Args(listOf(Var("St"), Atom(Ident("x"), Args(listOf(Var("X"))))))
                                )
                            ),
                            Body(listOf(Atom(Ident("elem"), Args(listOf(Var("X"), Var("St"))))))
                        ),
                        Relation(
                            Head(
                                Atom(
                                    Ident("nand"),
                                    Args(listOf(Atom(Ident("x"), null), Atom(Ident("y"), null)))
                                )
                            ),
                            null
                        )
                    )
                ),
                Body(listOf(Atom(Ident("eval"), Args(listOf(Atom(Ident("x"), null))))))
            ),
            ast
        )
    }

    private fun testHelper(filename: String): AST {
        val charStream = CharStreams.fromFileName(filename)

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
}