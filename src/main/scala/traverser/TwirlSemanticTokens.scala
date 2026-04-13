package traverser

/**
  * A constructed semantic token that contains no delta encoding as of right now.
  * This token can be encoded by its source position later to delta encoding and ordered
  * appropriately. This is largely because I am too stupid to figure out how to do everything
  * in a single pass. Perhaps someone smarter can come up with something that is a
  * less expensive solution, preferrably only one pass.
  *
  * @param length           The length of the token.
  * @param tokenType        The type of the token (use the semantic tokens service to retrieve this value).
  * @param tokenModifiers   Ditto.
  * @param line             The source position of the line of the input text.
  * @param column           Ditto.
  */
case class SourceTwirlSemanticToken(
  length: Int,
  tokenType: Int,
  tokenModifiers: Int,
  line: Int,
  column: Int,
) {
  def getSrcPos: Position   = Position(line = line, column = column)
  def encode(deltaLine: Int, deltaStart: Int): DeltaEncodedTwirlSemanticToken = {
    DeltaEncodedTwirlSemanticToken(
      deltaLine = deltaLine, deltaStart = deltaStart, length = length, tokenType = tokenType, tokenModifier = tokenModifiers
    )
  }
}

/** A case class designed to make it easier to make it easier to construct and convert semantic
  * tokens to/from their raw representation that is expected by LSP.
  *
  * @note
  * **THIS CLASS SHOULD NOT BE INSTANTIATED DIRECTLY.** The `encode(...)` method should be used
  * to convert a `SourceTwirlSemanticTokens` to a `DeltaEncodedTwirlSemanticTokens`.
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
case class DeltaEncodedTwirlSemanticToken(
  deltaLine: Int,
  deltaStart: Int,
  length: Int,
  tokenType: Int,
  tokenModifier: Int,
) {
  def toList: List[Int] = List(deltaLine, deltaStart, length, tokenType, tokenModifier)

  /**
    * This method will return a `Position` using the delta values defined
    * on the class. It should be made clear that this will not be the source
    * position.
    */
  def toDeltaPos: Position   = Position(deltaLine, deltaStart)
}
