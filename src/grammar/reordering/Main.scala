package grammar.reordering

object Main {
  
  case class Config(
      debug: String = "",
      keepalive: Boolean = false)

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("ReorderingGrammar") {
      head("ReorderingGrammar", "0.1")
      opt[String]("debug") required() action { (x, c) =>
        c.copy(debug = x)
      } text("this option is hidden in the usage text")
      note("some notes.\n")
      help("help") text("prints this usage text")
    }
    // parser.parse returns Option[C]
    parser.parse(args, Config()) map { config =>
      // do stuff
    } getOrElse {
      // arguments are bad, error message will have been displayed
    }
  }

}