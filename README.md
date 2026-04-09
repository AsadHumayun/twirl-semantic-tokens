## Twirl Semantic Tokens

The purpose for this project is to make an attempt to use Play Framework's 
`TwirlParser` to parse Twirl (`*.scala.html`) files in order to attempt to
obtain semantic tokens. These semantic tokens will then be used to provide
tools to improve developer experience, namely syntax highlighting and fur
ther styling. 

Should this be successful, this will represent a massive leap forward for 
engineers working on this type of technology stack in terms of developer
tooling and ease of access.

This is more of a spike project to attempt to extract and provide semanic 
tokens in a standalone project first, and is the intended to be added to
the Metals language server repository. The language server will be responsible
to perform the actions in this repository (mainly Traverser.scala) and then
provide these to the IDE.
