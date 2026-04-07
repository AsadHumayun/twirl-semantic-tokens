import traverser.*

import java.nio.file.Path
import org.eclipse.lsp4j.*

@main def main() = {
  val traverser = Traverser()

  val twirlPath = Util
    .getBaseDirectory
    .resolve("in")
    .resolve("Twirl.scala.html")
    .toString

  val params = SemanticTokensParams(TextDocumentIdentifier(twirlPath))

  traverser.getTwirlTemplateSemanticTokens(params)
}
