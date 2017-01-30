package soundwave

import java.io.File
import java.nio.file.{Paths, Files}

case class SwFile(path: String, contents: String)

object SwFileReader {

  def resourcePath(path: String*) =
    (Seq(BuildInfo.baseDirectory, "resources") ++ path).mkString(File.separator)

  def apply(p: String): Either[SwCompileError, SwFile] =
    for {
      path <- absPath(p).right
      contents <- readFile(path).right
    } yield SwFile(path, contents + "\n") // Add a newline at the end so we can concatenate files easily

  def absPath(path: String): Either[SwCompileError, String] =
    if (Files.exists(Paths.get(path))) {
      // First we try treating the expression as an absolute path or a path relative to the current
      // directory
      Right(path)
    } else if (Files.exists(Paths.get(resourcePath(path)))) {
      // If that failed, we look in resources
      Right(resourcePath(path))
    } else {
      Left(new SwFileError(s"Cannot open file '$path'."))
    }

  def readFile(path: String): Either[SwCompileError, String] =
    try {
      Right(io.Source.fromFile(path).getLines.mkString("\n"))
    } catch {
      case err: java.io.IOException => Left(new SwFileError(err.getMessage))
    }
}
