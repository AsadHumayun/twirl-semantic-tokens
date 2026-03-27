package traverser

import org.eclipse.lsp4j.*

class Traverser {
	/**
	  * @author Asad Humayun
	  */

	/**
	 * Opens a file and extracts its contents to be
	 * parsed by `TwirlParser`. The resultant AST will
	 * then be traversed and SemanticTokens will be sent
	 * to the extension.
	 */ 
	private def openFile(path: String): String = {
		import java.io.File
		import java.net.URI
		import scala.io.Source

		// TODO: This doesn't feel like a very robust implementation...
		Source.fromFile(new File(URI.create(path))).mkString("")
	}

	def getTwirlTemplateSemanticTokens(params: SemanticTokensParams) = {
		import play.twirl.parser.TwirlParser

		val document = params.getTextDocument
		val content	 = openFile(document.getUri)
		val parser	 = new TwirlParser(shouldParseInclusiveDot = true)
		val Success  = parser.Success
		val Error    = parser.Error

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
			case Error(template, input, errors) =>
		}

	// 	parser.parse(content) match {
	// 		case parser.Success(template, input) => 
	// 			template match {
	// 				case template.name => 
	// 					template.name.pos
	// 			}
	// 		case parser.Error(template, input, errors) => s"generic error here"
	// 	}
	}
}
