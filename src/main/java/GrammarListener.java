// Generated from Grammar.g4 by ANTLR 4.7
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link GrammarParser}.
 */
public interface GrammarListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link GrammarParser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(GrammarParser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(GrammarParser.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#relations}.
	 * @param ctx the parse tree
	 */
	void enterRelations(GrammarParser.RelationsContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#relations}.
	 * @param ctx the parse tree
	 */
	void exitRelations(GrammarParser.RelationsContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#relation}.
	 * @param ctx the parse tree
	 */
	void enterRelation(GrammarParser.RelationContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#relation}.
	 * @param ctx the parse tree
	 */
	void exitRelation(GrammarParser.RelationContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#head}.
	 * @param ctx the parse tree
	 */
	void enterHead(GrammarParser.HeadContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#head}.
	 * @param ctx the parse tree
	 */
	void exitHead(GrammarParser.HeadContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#body}.
	 * @param ctx the parse tree
	 */
	void enterBody(GrammarParser.BodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#body}.
	 * @param ctx the parse tree
	 */
	void exitBody(GrammarParser.BodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#atom}.
	 * @param ctx the parse tree
	 */
	void enterAtom(GrammarParser.AtomContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#atom}.
	 * @param ctx the parse tree
	 */
	void exitAtom(GrammarParser.AtomContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#args}.
	 * @param ctx the parse tree
	 */
	void enterArgs(GrammarParser.ArgsContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#args}.
	 * @param ctx the parse tree
	 */
	void exitArgs(GrammarParser.ArgsContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#ident}.
	 * @param ctx the parse tree
	 */
	void enterIdent(GrammarParser.IdentContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#ident}.
	 * @param ctx the parse tree
	 */
	void exitIdent(GrammarParser.IdentContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrammarParser#var}.
	 * @param ctx the parse tree
	 */
	void enterVar(GrammarParser.VarContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrammarParser#var}.
	 * @param ctx the parse tree
	 */
	void exitVar(GrammarParser.VarContext ctx);
}