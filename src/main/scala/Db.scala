import scala.io.StdIn

object Db {

  // Main entry point
  def main(args: Array[String]): Unit = {
    repl(Table(0, Vector.fill(MAX_PAGES)(None)))
  }

  // Meta-command results
  sealed trait MetaCommandResult
  case object META_COMMAND_SUCCESS extends MetaCommandResult
  case object META_COMMAND_UNRECOGNIZED_COMMAND extends MetaCommandResult

  // Prepare statement results
  sealed trait PrepareResult
  case object PREPARE_SUCCESS extends PrepareResult
  case object PREPARE_UNRECOGNIZED_STATEMENT extends PrepareResult
  case object PREPARE_SYNTAX_ERROR extends PrepareResult

  // Execution results
  sealed trait ExecuteResult
  case object EXECUTE_SUCCESS extends ExecuteResult
  case object EXECUTE_TABLE_FULL extends ExecuteResult

  // Statement Type
  sealed trait StatementType
  case object STATEMENT_INSERT extends StatementType
  case object STATEMENT_SELECT extends StatementType

  case class Row(id: Int, username: String, email: String)

  // immutable case class holding statement type
  sealed trait Statement
  case class InsertStatement(row: Row) extends Statement
  case class SelectStatement() extends Statement


  val PAGE_SIZE = 4096 // Each page is 4KB
  val ROW_SIZE = 291 // Each row is 291 bytes
  val ROWS_PER_PAGE: Int = PAGE_SIZE / ROW_SIZE
  val MAX_PAGES = 100 // Max number of pages (arbitrary limit)

  case class Table(numRows: Int, pages: Vector[Option[Vector[Row]]]) {
    def insertRow(row: Row): Table = {
      if (numRows >= MAX_PAGES * ROWS_PER_PAGE) {
        println("Error: Table full.")
        return this
      }

      val pageIndex = numRows / ROWS_PER_PAGE
      val updatedPage = pages(pageIndex) match {
        case Some(existingRows) => Some(existingRows :+ row) // create a new vector by adding in a new row
        case None => Some(Vector(row)) //if page is empty, add a new vector of row
      }
      val updatedPages = pages.updated(pageIndex, updatedPage)
      Table(numRows + 1, updatedPages)
    }

    def selectRows(): Unit = {
      pages.flatten.flatten.foreach(row => println(s"(${row.id}, ${row.username}, ${row.email})"))
    }
  }


  // The main loop of the program
  def repl(table: Table): Unit = {
    val inputCommand = readInput()

    if (inputCommand.startsWith(".")) {
      doMetaCommand(inputCommand) match {
        case META_COMMAND_SUCCESS => repl(table) // Loop again
        case META_COMMAND_UNRECOGNIZED_COMMAND =>
          println(s"Unrecognized command --> ${inputCommand}")
          repl(table)
      }
    } else {
      val updatedTable = prepareStatement(inputCommand) match {
        case Right(statement) =>
          executeStatement(statement, table) match
            case Right(updatedTable) =>
              println("Executed.")
              updatedTable
            case Left(EXECUTE_TABLE_FULL) =>
              println("Table Full!")
              table
            case _ =>
              println("Execute Statement Error")
              table
        case Left(PREPARE_UNRECOGNIZED_STATEMENT) =>
          println(s"Unrecognized keyword at start of '$inputCommand'.")
          table
        case Left(PREPARE_SYNTAX_ERROR) =>
          println(s"Syntax error at >${inputCommand}")
          table
      }
      repl(updatedTable) // Recursively call to continue the loop
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
    val tokens = input.split(" ")
    tokens match {
      case Array("insert", id, username, email) if id.forall(_.isDigit) =>
        Right(InsertStatement(Row(id.toInt, username, email)))
      case Array("insert", _*) =>
        Left(PREPARE_SYNTAX_ERROR) // Wrong syntax or missing arguments
      case Array("select") =>
        Right(SelectStatement())
      case _ =>
        Left(PREPARE_UNRECOGNIZED_STATEMENT)
    }
  }

  // Execute insert statement
  def executeInsert(insertStatement: InsertStatement, table: Table): Either[ExecuteResult, Table] = {
    table.insertRow(insertStatement.row) match
      case updatedTable: Table => Right(updatedTable)
      case _ => Left(EXECUTE_TABLE_FULL)
  }

  // Execute statement and return new Table state
  def executeStatement(statement: Statement, table: Table): Either[ExecuteResult, Table] = {
    statement match {
      case InsertStatement(row) =>
        executeInsert(InsertStatement(row), table)
      case SelectStatement() =>
        table.selectRows()
        Right(table)
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
