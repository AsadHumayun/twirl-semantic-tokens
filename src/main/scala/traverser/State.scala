package traverser

/** The idea is that this class will hold the state for the recursive logic when it comes to
  * "jumping" between the ASTs.
  *
  * @param prevToken
  *   The previous `TwirlSemanticToken`
  * @param tokens
  *   All of the collected semantic tokens thus far
  */
case class State(
  prevToken: TwirlSemanticToken,
  tokens: Seq[TwirlSemanticToken],
) {
  def getPrevPos: Position = prevToken.toPos
}
