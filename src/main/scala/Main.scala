import com.mosesn.pirate.Pirate

object Main {
  def main(args: Array[String]) {
    val arguments = Pirate("[ -lwc ] [filename]")(args)
    val printme = if (arguments.flags.isEmpty) {
      PrintMe(true, true, true)
    }
    else {
      PrintMe(arguments.flags.contains('l'),
        arguments.flags.contains('w'),
        arguments.flags.contains('c'))
    }
    val file = arguments.strings.get("filename") match {
      case None => io.Source.stdin
      case Some(name) => io.Source.fromFile(name)
    }
    println(getCount(file, printme) mkString(" "))
  }

  case class PrintMe(l: Boolean, w: Boolean, c: Boolean)
  def getCount(file: io.Source, printme: PrintMe): Array[Int] = {
    def countWords(line: String) = if (line.trim() == "") 0 else line.split("\\s+").size
    def opt(bool: Boolean, value: Int) = if (bool) Some(value) else None
    val (lines, words, chars) = file.getLines.foldRight(0, 0, 0){
      case (line, (l, w, c)) => (l + 1, w + countWords(w), c + line.size + 1)
    }
    Array(opt(printme.l, lines), opt(printme.w, words), opt(printme.c, chars)).flatten
  }
}
