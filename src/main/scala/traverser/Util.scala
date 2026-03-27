package traverser

import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}

object Util {
	/** This utility method will write output to the specified destination
	  * file. 
		* 
	  * @param path    		The path where the file is located (or will be created in)
	  * @param content    The content to write
	  */
	def writeFile(path: String, content: String) = {
		Files.write(Paths.get(path), (content + "\n\r").getBytes(StandardCharsets.UTF_8))
	}
}
