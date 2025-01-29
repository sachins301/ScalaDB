import scala.io.StdIn

object Db {

  // Main entry point
  def main(args: Array[String]): Unit = {
    repl()
  }

  // Meta-command results
  sealed trait MetaCommandResult
  case object META_COMMAND_SUCCESS extends MetaCommandResult
  case object META_COMMAND_UNRECOGNIZED_COMMAND extends MetaCommandResult

  // Prepare statement results
  sealed trait PrepareResult
  case object PREPARE_SUCCESS extends PrepareResult
  case object PREPARE_UNRECOGNIZED_STATEMENT extends PrepareResult

  // Statement Type
  sealed trait StatementType
  case object STATEMENT_INSERT extends StatementType
  case object STATEMENT_SELECT extends StatementType
  // immutable case class holding statement type
  case class Statement(statementType: StatementType)

  // The main loop of the program
  def repl(): Unit = {
    val inputCommand = readInput()

    if (inputCommand.startsWith(".")) {
      doMetaCommand(inputCommand) match {
        case META_COMMAND_SUCCESS => repl() // Loop again
        case META_COMMAND_UNRECOGNIZED_COMMAND =>
          println(s"Unrecognized command --> ${inputCommand}")
          repl()
      }
    } else {
      prepareStatement(inputCommand) match {
        case Right(statement) =>
          executeStatement(statement)
          println("Executed.")
        case Left(PREPARE_UNRECOGNIZED_STATEMENT) =>
          println(s"Unrecognized keyword at start of '$inputCommand'.")
      }
      repl() // Recursively call to continue the loop
    }
  }

  // Handle meta commands
  def doMetaCommand(input: String): MetaCommandResult = {
    if (input == ".exit") {
      println("Exiting...")
      sys.exit(0)
    } else {
      META_COMMAND_UNRECOGNIZED_COMMAND
    }
  }

  // Prepare statement
  def prepareStatement(input: String): Either[PrepareResult, Statement] = {
    if (input.startsWith("insert")) Right(Statement(STATEMENT_INSERT))
    else if (input == "select") Right(Statement(STATEMENT_SELECT))
    else Left(PREPARE_UNRECOGNIZED_STATEMENT)
  }

  // Execute statement (placeholder for now)
  def executeStatement(statement: Statement): Unit = {
    statement.statementType match {
      case STATEMENT_INSERT  => println("This is where we would do an insert.")
      case STATEMENT_SELECT  => println("This is where we would do a select.")
    }
  }


  // Print the prompt
  def printPrompt(): Unit = {
    print("db > ")
  }

  // Read the user input from the console
  def readInput(): String = {
    printPrompt() // Print the prompt before reading input
    StdIn.readLine().trim
  }
}
