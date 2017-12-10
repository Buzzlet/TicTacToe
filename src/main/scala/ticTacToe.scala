// Tic Tac Toe!
import Array._

object Player {
	private var count = 0
	private def incr = {count += 1; count}
	private def setMarker: Char = {
		if(count == 1){
			'x'	
		}
		else{
			'o'
		}}
}

class Player {
	val pid = Player.incr
	val marker = Player.setMarker
}


class Board {
	
	var moves = ofDim[Char](3,3)
	for(i <- 0 to 2){
		for(j <- 0 to 2){
			moves(i)(j) = ' ';
		}
	}

	// draws the board with moves
	def draw = {
		println(" " + moves(0)(0).toString + " | " + moves(0)(1).toString + " | " + moves(0)(2).toString)
		println(" ---------")
		println(" " + moves(1)(0).toString + " | " + moves(1)(1).toString + " | " + moves(1)(2).toString)
		println(" ---------")
		println(" " + moves(2)(0).toString + " | " + moves(2)(1).toString + " | " + moves(2)(2).toString)
	}

	// returns True if the move makes the player win
	def makeMove(player:Player, row:Int, column:Int): Boolean = {
		var success = true
		if(moves(row)(column) != ' ')
		{
			success = false
		}
		else{
			moves(row)(column) = player.marker
		}
		success		
	}

	// returns True if the player has won
	def checkOver(player:Player): Boolean = {
		var win = false
		var gameComplete = false
		// Check all the rows for the player's character
		for(i <- 0 to 2){
			if(checkRow(i, player) || checkColumn(i, player) || checkDiagonals(player))
			{
				win = true
			}
		}

		if(win == false)
		{
			gameComplete = checkCompletion
		}

		win || gameComplete

	}
		
	// returns True if the player has won in the given row
	private def checkRow(row:Int, player:Player): Boolean = {
		player.marker == moves(row)(0) &&
		moves(row)(0) == moves(row)(1) &&
		moves(row)(1) == moves(row)(2)

	}

	// returns True if the player has won in the given column
	private def checkColumn(column:Int, player:Player): Boolean = {
		player.marker == moves(0)(column) &&
		moves(0)(column) == moves(1)(column) &&
		moves(1)(column) == moves(2)(column)
	}

	// returns True if the player has won in a diagonal
	private def checkDiagonals(player:Player): Boolean = {
		(player.marker == moves(0)(0) &&
		moves(0)(0) == moves(1)(1) &&
		moves(1)(1) == moves(2)(2)) ||
		(player.marker == moves(2)(0) &&
		moves(2)(0) == moves(1)(1) &&
		moves(1)(1) == moves(0)(2))
	}

	private def checkCompletion = {
		var complete = true
		for(i <- 0 to 2){
			for(j <- 0 to 2){
				if((moves(i)(j) == 'x' || moves(i)(j) == 'o') == false){
					complete = false
				}
			}
		}
		complete
	}
} // end Board

object TicTacToe {
	def main(args: Array[String]): Unit = {
		val player1 = new Player()
		val player2 = new Player()
		val gameBoard = new Board()

		gameBoard.draw
		var currentPlayer = player1
		while((gameBoard.checkOver(player1) || gameBoard.checkOver(player2)) == false){
			if(currentPlayer == player1){
				currentPlayer = player2
			}
			else{
				currentPlayer = player1
			}

			var (row, column) = getMove
			while(gameBoard.makeMove(currentPlayer, row, column) == false){
				println("Invalid move!")
				val temp = getMove
				row = temp._1
				column = temp._2
			}
			
			gameBoard.draw
		}
		println("Game Over!")
	}

	private def input(str: String): String = {
		println(str)
		scala.io.StdIn.readLine()
	}

	private def getMove: (Int, Int) = {
		var row = input("Enter row of move (1-3): ").toInt
		var column = input("Enter column of move (1-3): ").toInt
		while(row < 1 || row > 3 || column < 1 || column > 3 ){
			row = input("Enter row of move (1-3): ").toInt
			column = input("Enter column of move (1-3): ").toInt
		}
		(row-1, column-1)
	}
}