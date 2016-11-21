package cspp

import java.nio.file.{Path,Paths,Files}
import java.nio.file.attribute.PosixFilePermission._
import scalax.io._

object CsppFileWriter {

  def apply(path: String, lines: Seq[String], exec: Boolean =true): Either[CsppCompileError, Unit] = {
    try {
      val output = Resource.fromFile(path)
      output truncate 0
      output.writeStrings(lines, "\n")

      if (exec) {
        val jpath = Paths.get(path)
        val perms = Files.getPosixFilePermissions(jpath)
        perms.add(OWNER_EXECUTE)
        Files.setPosixFilePermissions(jpath, perms)
      }

      Right(())
    } catch {
      case err: java.io.IOException => Left(new CsppFileError(err.getMessage))
    }
  }
}
