package traverser

/** A case class designed to make it easier to make it easier to construct and convert semantic
  * tokens to/from their raw representation that is expected by LSP.
  *
  * @param deltaLine
  *   token line number, relative to the start of the previous token
  * @param deltaStart
  *   token start character, relative to the start of the previous token (relative to `0` or the
  *   previous token’s start if they are on the same line)
  * @param length
  *   length of the token
  * @param tokenType
  *   will be looked up in `SemanticTokensLegend.tokenTypes`. We currently ask that `tokenType` <
  *   `65536`.
  * @param tokenModifiers
  *   each set bit will be looked up in `SemanticTokensLegend.tokenModifiers`
  */
case class TwirlSemanticToken(
  deltaLine: Int,
  deltaStart: Int,
  length: Int,
  tokenType: Int,
  tokenModifiers: Int,
) {
  def toList: List[Int] = List(deltaLine, deltaStart, length, tokenType, tokenModifiers)
  def toPos: Position   = Position(deltaLine, deltaStart)
}
