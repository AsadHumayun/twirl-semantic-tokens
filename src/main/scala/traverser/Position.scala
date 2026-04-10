package traverser

/** This class stores delta information for a token. This contains the raw position of these
  * tokens and not the relative delta tokens that are expected by the LSP (i.e. VSCode)
  *
  * @param line
  *   The line number of the token
  * @param column
  *   The column number of the token
  */
case class Position(
  line: Int,
  column: Int,
)
