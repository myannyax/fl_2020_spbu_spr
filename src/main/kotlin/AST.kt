import org.antlr.v4.runtime.misc.ParseCancellationException

sealed class AST

data class Program(val relations: Relations, val body: Body?) : AST() {
    override fun toString(): String {
        return buildString {
            append(relations)
            append("?-")
            body?.let { append(it) }
            append('.')
        }
    }
}

data class Relations(val relations: List<Relation>) : AST() {
    override fun toString(): String {
        return buildString {
            for (r in relations) {
                append(r)
                append("\n")
            }
        }
    }
}

data class Relation(val head: Head, val body: Body?) : AST() {
    override fun toString(): String {
        return buildString {
            append(head)
            body?.let { append(" :- $it") }
            append(".")
        }
    }
}

data class Head(val atom: Atom) : AST() {
    override fun toString(): String {
        return atom.toString()
    }
}

data class Body(val atoms: List<Atom>) : AST() {
    override fun toString(): String {
        return buildString {
            for (r in atoms) {
                append(r)
                append(", ")
            }
        }.dropLast(2)
    }
}

data class Atom(val name: Ident, val args: Args?) : Argg() {
    override fun toString(): String {
        return buildString {
            append(name)
            args?.let { append(it) }
        }
    }
}

data class Args(val args: List<Argg>) : AST() {
    override fun toString(): String {
        return buildString {
            append('(')
            for (arg in args) {
                append(arg)
                append(", ")
            }
        }.dropLast(2) + ')'
    }
}


sealed class Argg : AST()

data class Ident(val name: String) : AST() {
    override fun toString(): String {
        return name
    }
}

data class Var(val name: String) : Argg() {
    override fun toString(): String {
        return name
    }
}


class ParserException(message: String) : ParseCancellationException(message)