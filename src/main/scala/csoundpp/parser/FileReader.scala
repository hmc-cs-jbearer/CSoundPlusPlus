package cspp

import java.io.File
import java.nio.file.{Paths, Files}

object CsppFileReader {

  def apply(p: String): Either[CsppCompileError, String] =
    for {
      path <- absPath(p).right
      contents <- readFile(path).right
    } yield contents + "\n" // Add a newline at the end so we can concatenate files easily

  def absPath(path: String): Either[CsppCompileError, String] =
    if (Files.exists(Paths.get(path))) {
      // First we try treating the expression as an absolute path or a path relative to the current
      // directory
      Right(path)
    } else if (Files.exists(Paths.get("resources" + File.separator + path))) {
      // If that failed, we look in resources
      Right("resources" + File.separator + path)
    } else {
      Left(new CsppFileError(s"Cannot open file '$path'."))
    }

  def readFile(path: String): Either[CsppCompileError, String] =
    try {
      Right(io.Source.fromFile(path).getLines.mkString("\n"))
    } catch {
      case err: java.io.IOException => Left(new CsppFileError(err.getMessage))
    }
}
