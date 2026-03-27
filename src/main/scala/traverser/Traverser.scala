package traverser

import org.eclipse.lsp4j.*
import java.nio.file.Path

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
													.toString

		val errorCase = Util.getBaseDirectory
												.resolve("out")
												.resolve("TwirlErrorCase.txt")
												.toString
	}

	/**
	 * Opens a file and extracts its contents to be
	 * parsed by `TwirlParser`.
	 *
	 * The resultant AST will then be traversed and
	 * SemanticTokens will be sent to the extension.
	 */
	private def openFile(path: String): String = {
		import java.io.File
		import java.net.URI
		import scala.io.Source

		println(s"[openFile] Attempting to read file [$path]")

		// TODO: This doesn't feel like a very robust implementation...
		Source.fromFile(new File(URI.create(path))).mkString("")
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
			Template output:
			${template}
			--- END TEMPLATE ---

			Input:
			${input}
			--- END INPUT ---

			Errors:
			${errors.getOrElse("No errors received.")}
			--- END ERRORS ---
			"""

			def write(file: String) = {
				Util.writeFile(file, s"""
				[TWIRL TEMPLATE PARSER RESULT]
				${this.toString}
				""")
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

		parser.parse(content) match {
			case Success(template, input) =>
				TwirlOutput(template, input, Option.empty).write("../../out/")
			case Error(template, input, errors) =>
				TwirlOutput(template, input, Option(errors)).write("../../out/TwirlErrorCase.txt")
		}
	}
}
