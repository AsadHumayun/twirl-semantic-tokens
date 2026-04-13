package traverser

import traverser.{ Position, State, SourceTwirlSemanticToken, DeltaEncodedTwirlSemanticToken }
import traverser.Emitter.*

import org.eclipse.lsp4j.{ SemanticTokenTypes, SemanticTokenModifiers, SemanticTokensParams }
import java.nio.file.Path
import play.twirl.parser.TreeNodes.*

object Paths {
  val successCase: Path = Util
    .getBaseDirectory
    .resolve("out")
    .resolve("TwirlSuccessCase.txt")

  val errorCase: Path = Util
    .getBaseDirectory
    .resolve("out")
    .resolve("TwirlErrorCase.txt")
}

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

  /** This method's purpose is to traverse and identify the Twirl template's metadata.
    *
    * @param state
    * @param template
    * @param pos
    * @return
    */
  def matchTemplate(state: State, template: BaseTemplate, pos: Position): State =
    template match
      case BlockTemplate(imports, members, sub, nodes)                                      =>
        val theseTokens = List()

        ???
      case SubTemplate(declaration, name, params, imports, members, sub, nodes)             => ???
      case Template(constructor, comment, params, topImports, imports, members, sub, nodes) => ???

  def matchNode(node: TemplateTree, state: State): State = {

    /** The below is a pseudocode-esque function that indicates that we now have basic scala source,
      * and can delegate the generation of tokens to existing infrastructure present in Metals. Take
      * the tokens from there, bring them in, then spit them out in the Twirl function. This might
      * require some restructuring on Metals' codebase when I try and add it ---- depends on what
      * things look like on their code's side when I try and get an merge-able branch set up.
      */
    def fetchSemanticTokensFromMetalsLS = ???

    def traverseReassignment(state: State, ref: Either[SubTemplate, Var]): State =
      ref match
        case Left(template) =>
          provideSemanticTokens(template, template.content)
        case Right(var_)    => 
          emitScala(
            state, pos = Position(
              line = var_.pos.line, column = var_.pos.column,
            ), 
            str,
          )

          // emitScala(
          //   state, Position(
          //     line = var_.pos.line, column = var_.pos.column
          //   ),
          //   var_.
          // )

    def traverseScalaExp(state: State, scalaExp: ScalaExp): State =
      ???
    def traverseBlock(state: State, block: Block): State          = {
      val theseTokens = List()
      // matchTemplate on the template's meta info
      // matchNode on the template's tree
      ???
    }

    def traverseScalaExpPart(state: State, part: ScalaExpPart): State =
      part match
        case simple @ Simple(code)                     =>
          /** Hand off to the Scala Metals implementation of semantic tokens for this, capture the
            * resultant tokens and then emit them.
            *
            * Not sure how to go about mimicing that kind of behaviour for this now - I think we
            * will just manually write it as something here.
            *
            * @TODO
            */

          // pseudocode-like method showing what should be done here AS OPPOSED
          // to doing what I currently am, and resolving the whole thing to a singple token.
          fetchSemanticTokensFromMetalsLS
        case block @ Block(whitespace, args, template) =>
          traverseBlock(state, block)

    /** I tried to get this into its own function or suchlike, but I was unable to, since in order
      * to extract the `pos`, I need to extract the types using the pattern matching extractors.
      *
      * Maybe I could do something like this by just manually supplying the `pos` attrs as a
      * property in the function, but I have not tried this yet in the spike here. Actually now that
      * I think about it, if I manually make things that rely on `pos` and then have them passed to
      * a function, then it's only really going to have one line in the resulting function which I'm
      * not sure about the value of having a function for something.... that small. (i.e. just
      * making a case class)
      *
      * The @ symbol makes it so that the whole object/class is extracted from the pattern match, as
      * opposed to just the string (since we need to get access to its other attrs/props).
      *
      * Also not sure as to how I'm going to be able to map over the list and keep doing it "over"
      * the previous item if you get me. I'm sure that something like `fold` or something will be
      * able to accommodate this.
      */

    node match {
      case comment @ Comment(msg)       =>
        emitComment(
          state,
          pos = Position(comment.pos.line, comment.pos.column),
          str = msg,
          tokenType = SemanticTokenTypes.Comment,
          tokenModifier = SemanticTokenModifiers.Documentation,
        )
      case plain @ Plain(text)          =>
        emitHtml(
          state,
          pos = Position(
            line = plain.pos.line,
            column = plain.pos.column,
          ),
          str = text,
          // TODO: Hand this off to an HTML parser instead?
          tokenType = SemanticTokenTypes.Label,
          tokenModifier = SemanticTokenModifiers.Abstract,
        )
      case display @ Display(exp)       => traverseScalaExp(state, exp)
      case reassign @ Reassignment(ref) => traverseReassignment(state, ref)
      // for scalaExp, foldLeft onto it
      case scalaExp @ ScalaExp(parts)   => ???
      case _: TemplateTree              => ???
    }
  }

  /** This function should loop over the nodes and then send them back.
    *
    * Use foldLeft for the folding
    */
  def provideSemanticTokens(template: BaseTemplate, nodes: scala.collection.Seq[TemplateTree]) =
    // note to self: also needs to call matchTemplate on this template...
    // also initialise state? no this should be done before this and state passed in
    ???

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
        // provideSemanticTokens(template, template.content)
        TwirlOutput(template, input, Option.empty).write(Paths.successCase.toString)
      case Error(template, input, errors) =>
        println(s"[getTwirl] Errors parsing template")
        TwirlOutput(template, input, Option(errors)).write(Paths.errorCase.toString)
    }
  }

}
