package traverser

import org.eclipse.lsp4j.*
import java.nio.file.Path
import play.twirl.parser.TreeNodes.*
import javax.swing.tree.MutableTreeNode

/**
 * @author Asad Humayun
 *
 * Methods added to this class should only be relevant to the LSP
 * implementation of this protocol, as most of this file may be 
 * lifted and brought into Metals via a potential PR. This is the
 * main cocnept behind this repository - try and get the semantic 
 * tokens working on my own first, and then send them off to Metals
 * to get added. Then there will be more time required to add in the
 * TS implementation of this solution.
 *
 */
class Traverser {
	private object Paths {
		val successCase = Util.getBaseDirectory
													.resolve("out")
													.resolve("TwirlSuccessCase.txt")

		val errorCase = Util.getBaseDirectory
												.resolve("out")
												.resolve("TwirlErrorCase.txt")
	}

	/**
	 * Opens a file and extracts its contents to be
	 * parsed by `TwirlParser`.
	 *
	 * The resultant AST will then be traversed and
	 * SemanticTokens will be sent to the extension.
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

	def provideSemanticTokens(tree: scala.collection.Seq[TemplateTree]) = {

		/**
		  * A case class designed to make it easier to make it easier to construct
			* and convert semantic tokens to/from their raw representation that is
			* expected by LSP.
		  *
		  * @param deltaLine 			token line number, relative to the start of the previous token
		  * @param deltaStart			token start character, relative to the start of the previous
			* 											token (relative to `0` or the previous token’s start if they are on the same line)
		  * @param length					length of the token
		  * @param tokenType			will be looked up in `SemanticTokensLegend.tokenTypes`. We currently ask that
			* 											`tokenType` < `65536`.
		  * @param tokenModifiers each set bit will be looked up in `SemanticTokensLegend.tokenModifiers`
		  */
		case class TwirlSemanticToken(
			val deltaLine				: Int,
			val deltaStart			: Int,
			val length					: Int,
			val tokenType				: Int,
			val tokenModifiers	: Int
		) {
			def toList: List[Int] = List(deltaLine, deltaStart, length, tokenType, tokenModifiers)
		}

		def getLength(token: TemplateTree): Int = {
			token match {
				case Comment(msg)				=> msg.length
				case Display(exp)				=> exp.parts.length
				case Plain(text) 				=> text.length
				case Reassignment(ref)  =>
					ref match {
						case Left(subTemplate: SubTemplate) => 
							
						case Right(var_: Var) => 
					}
				case ScalaExp(parts) 		=>
			}
		}
	}

	def getTwirlTemplateSemanticTokens(params: SemanticTokensParams) = {
		import play.twirl.parser.{TreeNodes, TwirlParser}

		val document = params.getTextDocument
		val content	 = openFile(document.getUri)
		val parser	 = new TwirlParser(shouldParseInclusiveDot = true)
		val Success  = parser.Success
		val Error    = parser.Error

		case class TwirlOutput(
			val template: TreeNodes.Template,
			val input		: parser.Input,
			val errors	: Option[List[TreeNodes.PosString]]
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

			def write(file: String) = {
				Util.writeFile(file, s"""
				|[TWIRL TEMPLATE PARSER RESULT]
				|${this.toString}
				""".stripMargin)
			}
		}

		/**
		 *
		 * name: PosString,
		 * constructor: Option[Constructor],
		 * comment: Option[Comment],
		 * params: PosString,
		 * topImports: collection.Seq[Simple],
		 * imports: collection.Seq[Simple],
		 * defs: collection.Seq[Def],
		 * sub: collection.Seq[Template],
		 * content: collection.Seq[TemplateTree]
		 *
		 */

		println(s"[getTwirl] Attempting to parse Twirl template source...")

		/**
		  * TODO: Make it so that when printed, the case class will also show
			* the keys associated with each value, eg input=[...] and so on for
			* clarity and assisstance of debugging.
		  */

		parser.parse(content) match {
			case Success(template, input) =>
				provideSemanticTokens(template.content)
				TwirlOutput(template, input, Option.empty).write(Paths.successCase.toString)
			case Error(template, input, errors) =>
				println(s"[getTwirl] Error parsing template")
				TwirlOutput(template, input, Option(errors)).write(Paths.errorCase.toString)
		}
	}
}
