package traverser

import org.eclipse.lsp4j.SemanticTokenTypes
import org.eclipse.lsp4j.SemanticTokenModifiers
import play.twirl.parser.TreeNodes.PosString

object Emitter {
  private def getDeltaPos(curr: Position, prev: Position): Position =
    /**
      * @TODO there is still some LSP delta encoding logic that is yet to be
      * completed here; please read the LSP docs on what that particular edge
      * case was and add something here (or whereever otherwise required) to
      * prevent this from tripping myself up in the future.
      */
    Position(
      line = curr.line - prev.line,
      column = curr.column - prev.column,
    )

  private def resolveTokens(
    state: State,
    pos: Position,
    str: String,
    tokenType: String,
    tokenModifier: String,
  ): State = {
    val delta = getDeltaPos(
      curr = pos,
      prev = state.getPrevPos,
    )
    /** @note
      *   there are some warnings abuot behaviour for deltaLine that I have not yet implemented ---
      *   read LSP docs about it and make sure that it is eventually done so that you don't trip
      *   over it later and wonder why it is not working...
      */
    val thisToken = TwirlSemanticToken(
      deltaLine = delta.line,
      deltaStart = delta.column,
      length = str.length,
      tokenType = SemanticTokensService.types.resolve(tokenType),
      tokenModifiers = SemanticTokensService.modifiers.resolve(tokenModifier),
    )

    State(
      prevToken = thisToken,
      tokens = state.tokens.appended(thisToken),
    )
  }

  def emitHtml(
    state: State,
    pos: Position,
    str: String,
  ): State = {
    println(s"HTML emitted: [$str].")
    resolveTokens(
      state, pos, str,
      tokenType = SemanticTokenTypes.String,
      tokenModifier = SemanticTokenModifiers.Static,
    )
  }

  def emitScala(
    state: State,
    pos: Position,
    str: String,
  ): State = {
    println(s"Scala emitted: [$str].")
    resolveTokens(
      state, pos, str,
      tokenType = SemanticTokenTypes.Method,
      tokenModifier = SemanticTokenModifiers.Async,
    )
  }

  def emitComment(
    state: State,
    pos: Position,
    str: String,
  ): State = 
    resolveTokens(
      state, pos, str,
      tokenType = SemanticTokenTypes.Comment,
      tokenModifier = SemanticTokenModifiers.Documentation,
    )

  /**
    * Used for template imports.
    * @example {{{
    * @import java.net.URLEncoder
    * imports=[ArrayBuffer(Simple(import java.net.URLEncoder))]
    * }}}
    * @note This will internally call `emitScala(...)` on the specified tokens.
    */
  def emitImports(
    state: State,
    pos: Position,
    str: String,
  ): State = emitScala(state, pos, str)

  /**
    * This is the details that are provided in the `@this(...)` expression in Twirl.
    * @note This will internally call `emitScala(...)` on the specified tokens.
    * @example
    * {{{
    * @this(main: main)
    * constructor=[Some(Constructor(None,(main: main)))]
    * }}}
   */
  def emitConstructor(
    state: State,
    pos: Position,
    str: String,
  ): State = 
    resolveTokens(
      state, pos, str,
      tokenType = SemanticTokenTypes.Comment,
      tokenModifier = SemanticTokenModifiers.Documentation,
    )

  /**
    * Emits template header params.
    */
  def emitParams(
    state: State,
    pos: PosString,
    str: String,
  ): State = {
    resolveTokens(
      state, 
      pos = Position(
        line = pos.pos.line,
        column = pos.pos.column,
      ),
      str,
      tokenType = SemanticTokenTypes.Comment,
      tokenModifier = SemanticTokenModifiers.Documentation,
    )
  }
}
