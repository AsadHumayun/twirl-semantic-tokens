package traverser

/** This case object is 'infrastructure' that is likely already present somewhere in their codebase,
  * so this is really to try and assert that things are working locally, and this is not what things
  * are likely to look like.
  *
  * This will likely be something that is either addressed in the PR to merge this into Metals
  * (should things go well...) but yeah this will require discussion with some reviewers/maintainers
  * as I just don't know enough about the Metals codebase to properly assert something like this.
  */
case object SemanticTokensService {
  case object types {
    private val typesList = List(
      "type",
      "class",
      "enum",
      "interface",
      "struct",
      "typeParameter",
      "parameter",
      "variable",
      "property",
      "enumMember",
      "event",
      "function", // 12
      "method",
      "macro",
      "keyword",
      "modifier",
      "comment", // 17
      "string",
      "number",
      "regexp", // 20
      "operator",
      "decorator",
      "label", // 23
    )

    private val typesMap: Map[String, Int] = typesList.zipWithIndex.toMap

    def resolve(searchTerm: String): Int =
      typesMap.get(searchTerm) match {
        case Some(value) => value

        /** ========================
          * This could cause issues later down the line - how do I want to
          * go about handling errors like these?
          * ========================
          */
        case None => 0
      }
  }

  case object modifiers {
    private val modifiersList                  = List(
      "declaration",
      "definition",
      "readonly",
      "static",
      "deprecated",
      "abstract",
      "async",
      "modification",
      "documentation",
      "defaultLibrary",
    )
    private val modifiersMap: Map[String, Int] = modifiersList.zipWithIndex.toMap
    def resolve(searchTerm: String): Int       =
      modifiersMap.get(searchTerm) match {
        case Some(value) => value
        case None        => 0
      }
  }
}
