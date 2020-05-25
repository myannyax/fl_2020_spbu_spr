import GrammarParser.*
import org.antlr.v4.runtime.tree.TerminalNodeImpl

class MyVisitor : GrammarBaseVisitor<AST>() {
    override fun visitProgram(ctx: ProgramContext?): AST {
        if (ctx == null) throw ParserException("visitProgram: context is empty")
        return if (ctx.childCount == 3) {
            val relations = visitRelations(ctx.getChild(0) as RelationsContext) as Relations
            Program(relations, null)
        } else {
            val relations = visitRelations(ctx.getChild(0) as RelationsContext) as Relations
            val body = visitBody(ctx.getChild(2) as BodyContext) as Body
            Program(relations, body)
        }
    }

    override fun visitRelations(ctx: RelationsContext?): AST {
        if (ctx == null) throw ParserException("visitRelations: context is empty")
        val lst = mutableListOf<Relation>()
        if (ctx.children == null) return Relations(emptyList())
        for (r in ctx.children) {
            val token = when (r) {
                is RelationContext -> visitRelation(r) as Relation
                is TerminalNodeImpl -> null
                else -> throw ParserException("visitRelations: unknown type in Relations parsing\n${r.text}")
            }
            token?.let { lst.add(it) }
        }
        return Relations(lst)
    }

    override fun visitRelation(ctx: RelationContext?): AST {
        if (ctx == null) throw ParserException("visitRelation: context is empty")
        return if (ctx.childCount == 2) {
            val head = visitHead(ctx.getChild(0) as HeadContext) as Head
            Relation(head, null)
        } else {
            val head = visitHead(ctx.getChild(0) as HeadContext) as Head
            val body = visitBody(ctx.getChild(2) as BodyContext) as Body
            Relation(head, body)
        }
    }

    override fun visitHead(ctx: HeadContext?): AST {
        if (ctx == null) throw ParserException("visitHead: context is empty")
        val atom = visitAtom(ctx.getChild(0) as AtomContext) as Atom
        return Head(atom)
    }

    override fun visitBody(ctx: BodyContext?): AST {
        if (ctx == null) throw ParserException("visitBody: context is empty")
        val lst = mutableListOf<Atom>()
        for (r in ctx.children) {
            val token = when (r) {
                is AtomContext -> visitAtom(r) as Atom
                is TerminalNodeImpl -> null
                else -> throw ParserException("visitBody: unknown type in Body/Goal parsing\n${r.text}")
            }
            token?.let { lst.add(it) }
        }
        return Body(lst)
    }

    override fun visitAtom(ctx: AtomContext?): AST {
        if (ctx == null) throw ParserException("visitAtom: context is empty")
        return if (ctx.childCount == 1) {
            val ident = visitIdent(ctx.getChild(0) as IdentContext) as Ident
            Atom(ident, null)
        } else {
            val ident = visitIdent(ctx.getChild(0) as IdentContext) as Ident
            val args = visitArgs(ctx.getChild(2) as ArgsContext) as Args
            Atom(ident, args)
        }
    }

    override fun visitArgs(ctx: ArgsContext?): AST {
        if (ctx == null) throw ParserException("visitArgs: context is empty")
        val lst = mutableListOf<Argg>()
        for (r in ctx.children) {
            val token = when (r) {
                is AtomContext -> visitAtom(r) as Atom
                is VarContext -> visitVar(r) as Var
                is TerminalNodeImpl -> null
                else -> throw ParserException("visitArgs: unknown type in Args parsing\n${r.text}")
            }
            token?.let { lst.add(it) }
        }
        return Args(lst)
    }

    override fun visitIdent(ctx: IdentContext?): AST {
        return Ident(ctx?.text ?: "")
    }

    override fun visitVar(ctx: VarContext?): AST {
        return Var(ctx?.text ?: "")
    }
}