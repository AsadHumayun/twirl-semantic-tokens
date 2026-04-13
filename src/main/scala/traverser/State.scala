package traverser

/** The idea is that this class will hold the state for the recursive logic when it comes to
  * "jumping" between the ASTs.
  *
  * @param prevToken
  *   The previous `SourceTwirlSemanticToken`
  * @param tokens
  *   All of the collected semantic tokens thus far
  */
case class State(
  prevToken: SourceTwirlSemanticToken,
  tokens: Seq[SourceTwirlSemanticToken],
) {
  def getPrevPos: Position = prevToken.getSrcPos
}
