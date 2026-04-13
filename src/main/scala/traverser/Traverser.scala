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
  def matchTemplate(state: State, template: BaseTemplate, pos: Position): State = {

    /** Matches common template metadata. This applies to all templates that we might receive and
      * therefore have to match against & process tokens for.
      *
      * Matches and processes `[state, imports, sub, nodes]` of a template.
      *
      * @param state
      *   The state to use.
      * @param imports
      *   The imports that are used in this template.
      * @param members
      *   The members of this template.
      * @param sub
      *   The subtemplates of this template.
      * @param nodes
      *   The nodes of this tree.
      * @return
      *   State -- the new State for this template.
      */
    def matchCommonTemplateMeta(
      state: State,
      imports: collection.Seq[Simple],
      members: collection.Seq[LocalMember],
      sub: collection.Seq[SubTemplate],
      nodes: collection.Seq[TemplateTree],
    ): State                                                           = {
      val importedStates   = imports.foldLeft(state) { (state, import_) =>
        emitScala(state = state, Position(import_.pos.line, import_.pos.column), import_.code)
      }
      val membersState     = members.foldLeft(importedStates) { (state, member) =>
        emitScala(
          state = state,
          pos = Position(
            line = member.pos.line,
            column = member.pos.column,
          ),
          str = member.code.code,
        )
      }
      val subTemplateState = sub.foldLeft(membersState) { (state, sub) =>
        matchTemplate(
          state = state,
          template = sub,
          pos = Position(
            line = sub.pos.line,
            column = sub.pos.column,
          ),
        )
      }
      val finalState       =
        nodes.foldLeft(subTemplateState)((state, node) => matchNode(node = node, state = state))

      State(
        prevToken = finalState.prevToken,
        tokens = finalState.tokens,
      )
    }
    /*for testing, remove later*/
    val params                                                         =
      """
        |@this(
        |  // a comment
        |  someParam: T,
        |  @* a comment *@
        |  s: D[T],
        |  /* idk some comment */
        |)
        |""".stripMargin
      /*end testing region*/
    def getCommentNodes(text: String): (Position, String) | EmptyTuple = {
      val sources: (Position, String) | EmptyTuple = Tuple()
      var pos                                      = 0
      while (pos < text.length) {
        val substr = text.substring(pos)
        if (text.startsWith("//", pos)) {
          // "//" will leave the rest of the line as a comment, so we can skip
          // to the end of the line here.
          val endIndex = text.indexOf("\n", pos + 2)
          if (endIndex == -1) pos = text.length()
        } else {}
      }

      sources
    }

    template match
      case BlockTemplate(imports, members, sub, nodes)                                      =>
        val theseTokens = List()

        ???
      case SubTemplate(declaration, name, params, imports, members, sub, nodes)             => ???
      case Template(constructor, comment, params, topImports, imports, members, sub, nodes) => ???

  def matchNode(node: TemplateTree, state: State): State = {
    def traverseReassignment(state: State, ref: Either[SubTemplate, Var]): State =
      ref match
        case Left(template) =>
          provideSemanticTokens(template, template.content)
        case Right(var_)    =>
          // Just emit everything as Scala and then let Metals provide
          // the semantic tokens for this - I could go and do it myself
          // but there is not really much of a point in doing that if Metals
          // can just go ahead and do it for us anyway
          emitScala(
            state = state,
            pos = Position(
              line = var_.pos.line,
              column = var_.pos.column,
            ),
            str = var_.code.toString,
          )
    def traverseScalaExp(state: State, scalaExp: ScalaExp): State                =
      scalaExp.parts.foldLeft(state)(traverseScalaExpPart)
    def traverseBlock(state: State, block: Block): State                         = {
      val tokens =
        matchTemplate(state, block.contents, Position(block.pos.line, block.pos.column)).tokens
      State(
        prevToken = state.prevToken,
        tokens,
      )
    }

    def traverseScalaExpPart(state: State, part: ScalaExpPart): State =
      part match
        case simple @ Simple(code)                     =>
          emitScala(
            state = state,
            pos = Position(simple.pos.line, simple.pos.column),
            str = code,
          )
        case block @ Block(whitespace, args, template) => traverseBlock(state, block)

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
      case scalaExp @ ScalaExp(parts)   => traverseScalaExp(state = state, scalaExp = scalaExp)
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
