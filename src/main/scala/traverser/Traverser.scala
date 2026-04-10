package traverser

import org.eclipse.lsp4j.*
import java.nio.file.Path
import play.twirl.parser.TreeNodes.*
import javax.swing.tree.MutableTreeNode

/** @author
  *   Asad Humayun
  *
  * Methods added to this class should only be relevant to the LSP implementation of this protocol,
  * as most of this file may be lifted and brought into Metals via a potential PR. This is the main
  * cocnept behind this repository - try and get the semantic tokens working on my own first, and
  * then send them off to Metals to get added. Then there will be more time required to add in the
  * TS implementation of this solution.
  */
class Traverser {

  private object Paths {

    val successCase: Path = Util
      .getBaseDirectory
      .resolve("out")
      .resolve("TwirlSuccessCase.txt")

    val errorCase: Path = Util
      .getBaseDirectory
      .resolve("out")
      .resolve("TwirlErrorCase.txt")

  }

  /** Opens a file and extracts its contents to be parsed by `TwirlParser`.
    *
    * The resultant AST will then be traversed and SemanticTokens will be sent to the extension.
    *
    * [REMOVE WHEN SUBMITTING PR]
    */
  private def openFile(path: String): String = {
    import java.io.File
    import java.net.URI
    import scala.io.Source

    println(s"[openFile] Attempting to read file [$path]")

    // TODO: This doesn't feel like a very robust implementation...
    Source.fromFile(new File(path)).mkString("")
  }

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
    tokens   : Seq[TwirlSemanticToken],
  ) {
    def getPrevPos: Position = prevToken.toPos
  }

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
    deltaLine     : Int,
    deltaStart    : Int,
    length        : Int,
    tokenType     : Int,
    tokenModifiers: Int,
  ) {
    def toList: List[Int] = List(deltaLine, deltaStart, length, tokenType, tokenModifiers)
    def toPos: Position   = Position(deltaLine, deltaStart)
  }

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

  def traverseReassignment() = {}

  def matchNode(node: TemplateTree, state: State): State = {
    def getDeltaPos(curr: Position, prev: Position): Position =
      Position(
        line = curr.line - prev.line,
        column = curr.column - prev.column,
      )

    /** I tried to get this into its own function or suchlike, but I was unable to, since in
      * order to extract the `pos`, I need to extract the types using the pattern matching
      * extractors.
      *
      * Maybe I could do something like this by just manually supplying the `pos` attrs as a
      * property in the function, but I have not tried this yet in the spike here. Actually now
      * that I think about it, if I manually make things that rely on `pos` and then have them
      * passed to a function, then it's only really going to have one line in the resulting
      * function which I'm not sure about the value of having a function for something.... that
      * small. (i.e. just making a case class)
      *
      * The @ symbol makes it so that the whole object/class is extracted from the pattern
      * match, as opposed to just the string (since we need to get access to its other
      * attrs/props).
      *
      * Also not sure as to how I'm going to be able to map over the list and keep doing it
      * "over" the previous item if you get me. I'm sure that something like `fold` or something
      * will be able to accommodate this.
      */

    node match {
      case comment @ Comment(msg)       =>
        val pos   = Position(comment.pos.line, comment.pos.column)
        val delta = getDeltaPos(
          curr = pos,
          prev = state.getPrevPos
        )

        val thisToken = TwirlSemanticToken(
          deltaLine = delta.line,
          deltaStart = delta.column,
          length = msg.length,
          // TODO: Actually understand what `tokenType` and `tokenModifiers`
          // are expected to be by LSP and what they should be/represent.
          tokenType = 1,
          tokenModifiers = 111,
        )

        val tokens = state.tokens.appended(thisToken)

        State(
          prevToken = thisToken,
          tokens,
        )

      case display @ Display(exp)       =>
        // Scala source
        val delta = getDeltaPos(
          curr = Position(display.pos.line, display.pos.column),
          prev = prevToken.toPos,
        )

        TwirlSemanticToken(
          deltaLine = delta.line,
          deltaStart = delta.column,
          length = exp.parts.length,
          // ======================
          // Still need to actually figure out and fix this part
          // ======================
          tokenType = 1,
          tokenModifiers = 111,
        )
      case plain @ Plain(text)          =>
        val delta = getDeltaPos(
          curr = Position(plain.pos.line, plain.pos.column),
          prev = prevToken.toPos,
        )

        TwirlSemanticToken(
          deltaLine = delta.line,
          deltaStart = delta.column,
          length = text.length,
          // ======================
          // Still need to actually figure out and fix this part
          // ======================
          tokenType = 1,
          tokenModifiers = 111,
        )
      case reassign @ Reassignment(ref) =>
        /** I don't really know how to go about recuring like this... */
        // New plan: make a different function to traverse the Reassignment side,
        // and just get it to pass around state
        ???
      case simpleCode @ Simple(code)    =>
        // Triggered by ScapaExpPart...
        val delta = getDeltaPos(
          curr = Position(simpleCode.pos.line, simpleCode.pos.column),
          prev = prevToken.toPos,
        )

        TwirlSemanticToken(
          deltaLine = delta.line,
          deltaStart = delta.column,
          length = code.length,
          // ======================
          // Still need to actually figure out and fix this part
          // ======================
          tokenType = 1,
          tokenModifiers = 111,
        )

      // case blockCode  @ Block(whitespace, args, contents)        =>

      case scalaExp @ ScalaExp(parts) =>
        parts.map()
      case _: TemplateTree            => ???
    }
  }

  /** This function should loop over the nodes and then send them back.
    *
    * Use foldLeft for the folding
    */
  def provideSemanticTokens(nodes: scala.collection.Seq[TemplateTree]) =
    nodes

  def getTwirlTemplateSemanticTokens(params: SemanticTokensParams) = {
    import play.twirl.parser.{ TreeNodes, TwirlParser }

    val document = params.getTextDocument
    val content  = openFile(document.getUri)
    val parser   = new TwirlParser(shouldParseInclusiveDot = true)
    val Success  = parser.Success
    val Error    = parser.Error

    case class TwirlOutput(
      template: TreeNodes.Template,
      input: parser.Input,
      errors: Option[List[TreeNodes.PosString]],
    ) {
      override def toString(): String = s"""
                                           |Template output:
                                           |${Util.caseClassToString(template)}
                                           |--- END TEMPLATE ---
                                           |
                                           |Input:
                                           |${Util.caseClassToString(input)}
                                           |--- END INPUT ---
                                           |
                                           |Errors:
                                           |${errors.getOrElse("No errors received.")}
                                           |--- END ERRORS ---
			""".stripMargin

      def write(file: String) =
        Util.writeFile(
          file,
          s"""
             |[TWIRL TEMPLATE PARSER RESULT]
             |${this.toString}
				""".stripMargin,
        )
    }

    /** name: PosString, constructor: Option[Constructor], comment: Option[Comment], params:
      * PosString, topImports: collection.Seq[Simple], imports: collection.Seq[Simple], defs:
      * collection.Seq[Def], sub: collection.Seq[Template], content: collection.Seq[TemplateTree]
      */

    println(s"[getTwirl] Attempting to parse Twirl template source...")

    /** TODO: Make it so that when printed, the case class will also show the keys associated with
      * each value, eg input=[...] and so on for clarity and assisstance of debugging.
      */

    parser.parse(content) match {
      case Success(template, input)       =>
        provideSemanticTokens(template.content)
        TwirlOutput(template, input, Option.empty).write(Paths.successCase.toString)
      case Error(template, input, errors) =>
        println(s"[getTwirl] Error parsing template")
        TwirlOutput(template, input, Option(errors)).write(Paths.errorCase.toString)
    }
  }

}
