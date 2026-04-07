package traverser

import java.nio.charset.StandardCharsets
import java.nio.file.{ Paths, Path, Files }

object Util {

  /** This utility method will write output to the specified destination file.
    *
    * @param path
    *   The path where the file is located (or will be created in)
    * @param content
    *   The content to write
    */
  def writeFile(path: String, content: String) = {
    println(s"[writFile] Attempting to write file [$path]")
    Files.write(Paths.get(path), (content + "\n\r").getBytes(StandardCharsets.UTF_8))
  }

  /** Returns the base directory for this project (`user.dir`) */
  def getBaseDirectory: Path = Path.of(System.getProperty("user.dir"))

  /** This method will iterate over the fields of a case class and present them in a more
    * easy-to-digest form.
    *
    * @example
    *   {{{ case class Example(e: String)
    *
    * val example = Example("hi!")
    *
    * caseClassToString(example) -> "e=[hi!]" }}}
    *
    * @see
    *   {@link https://stackoverflow.com/a/23129035}
    *   - Posted by serejja, modified by community. See post 'Timeline' for change history
    *     - Retrieved 2026-03-28, License - CC BY-SA 3.0
    */
  def caseClassToString(c: AnyRef): String = {
    val fields = (Map[String, Any]() /: c.getClass.getDeclaredFields) { (a, f) =>
      f.setAccessible(true)
      a + (f.getName -> f.get(c))
    }

    fields map {
      case (k, v) =>
        s"$k=[$v]"
    } mkString "\n\r"
  }

}
