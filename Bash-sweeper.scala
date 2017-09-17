import scala.util.Random
import scala.io.StdIn

object Game {

    def clamp(x: Int, lower: Int, upper: Int): Int = (x max lower) min upper


    case class Coord(x: Int, y: Int) 


    case class Tile(val isBomb: Boolean = false) {
        var covered = true
        var marked = false
        var surroundingBombs = 0
        var pos = Coord(0, 0)

        def uncover(): Unit = { 
            covered = false
        }

        def print(): Unit = {
            if(covered)   
                Console.print(if(marked) Console.RED_B+'x' else '█')
            else if(isBomb) 
                Console.print(Console.BOLD + Console.BLACK + 'x')
            
            else if(surroundingBombs == 0)
                Console.print(Tile.colors(0) + '.')
            else
                Console.print(
                    Tile.colors(surroundingBombs) 
                  + surroundingBombs)
        }
    }


    object Tile {
        val colors =
            Array(
                Console.WHITE,
                Console.BLUE,
                Console.GREEN,
                Console.BOLD + Console.YELLOW,
                Console.YELLOW,
                Console.RED,
                Console.BOLD + Console.RED,
                Console.BOLD + Console.RED,
                Console.BOLD + Console.UNDERLINED + Console.RED,
                Console.BOLD + Console.UNDERLINED + Console.RED,
                Console.BOLD + Console.UNDERLINED + Console.RED,
            )
    }


    class Board {
        private val m_width = 30
        private val m_height = 16
        private val m_bombCount = 99
        private var m_cursorPos = Coord(0, 0)


        private val m_board: List[List[Tile]] = {
            var tempBoard =
                Random.shuffle(
                    List.fill(m_bombCount) { new Tile(isBomb = true) } 
                 ++ List.fill(m_width*m_height - m_bombCount) { new Tile() }
                )
                    .grouped(m_height)
                    .zipWithIndex
                    .map { case (col, x) =>
                        col.zipWithIndex.map { case (tile, y) =>
                            var newTile = tile
                            newTile.pos = Coord(x, y)
                            newTile
                        }
                    }
                    .toList
                    //inverted x and y makes for simpler printing of board
                    .transpose

            tempBoard.flatten foreach { tile =>
                tile.surroundingBombs = 
                    tiles_around(tile, tempBoard) count { _.isBomb }
            }

            tempBoard
        }

        private def tile_at(c: Coord): Tile = m_board(c.y)(c.x)
        private def tile_at(x: Int, y: Int): Tile = tile_at(Coord(x, y))

        private def tiles_around(
            tile: Tile, 
            board: List[List[Tile]] = m_board): List[Tile] = 
        {
            board
                .slice(tile.pos.y-1, tile.pos.y+2)
                .map { _.slice(tile.pos.x-1, tile.pos.x+2) }
                .flatten
        }

        def bombsLeft: Int = {
            m_bombCount - 
                m_board.flatten count { tile => 
                    tile.marked && tile.covered
                }
        }

        def uncover(tile: Tile): Unit = {
            if(!tile.covered) return
                
            tile.uncover()

            if(tile.surroundingBombs == 0)
                uncover_around(tile)
        }

        def uncover_around(tile: Tile): Unit = {
            tiles_around(tile)
               .filterNot { _.marked } 
               .filter { _.covered }
               .foreach { uncover(_) }
        }

        def uncover_current_tile(): Unit = {
            uncover(tile_at(m_cursorPos))
        }

        def uncover_around_cursor(): Unit = {
            uncover_around(tile_at(m_cursorPos))
        }

        def mark_current_tile(): Unit = {
            tile_at(m_cursorPos).marked = true
        }

        def move_cursor(x: Int, y: Int): Unit = {
            val newX = clamp(m_cursorPos.x + x, 0, m_width-1)
            val newY = clamp(m_cursorPos.y + y, 0, m_height-1)

            m_cursorPos = Coord(newX, newY)
        }

        def over = won || lost

        def won: Boolean = {
            m_board.flatten forall { tile => 
                !tile.covered ^ tile.isBomb 
            }
        }

        def lost: Boolean = {
            m_board.flatten exists { tile => 
                !tile.covered && tile.isBomb
            }
        }
 
        def print(): Unit = {
            import scala.sys.process._
            "clear".!

            m_board foreach { row =>
                row foreach { tile =>
                    if(tile.pos == m_cursorPos) {
                        Console.print(Console.REVERSED)
                    }

                    tile.print()
                    Console.print(Console.RESET)
                }

                Console.print('\n')
            }
        }
    }
    

    def main(args: Array[String]): Unit = {
        val game = new Board()
        
        var gameOver = false
        
        while(!gameOver) {
            game.print()
            println("Bombs left: " + game.bombsLeft)

            val commands = StdIn.readLine
            
            commands foreach { c =>
                c match {
                    case 'u' => game.uncover_current_tile()
                    case 'U' => game.uncover_around_cursor()
                    case 'm' => game.mark_current_tile()
                    case 'q' => gameOver = true
                    
                    case 'w' => game.move_cursor(0, -1)
                    case 'a' => game.move_cursor(-1, 0)
                    case 's' => game.move_cursor(0, 1)
                    case 'd' => game.move_cursor(1, 0)

                    case _   => {}
                }
            }

            gameOver = gameOver || game.over
        }

        game.print()

        if(game.won) 
            println("CONGRATULATIONS!")
        else 
            println("GAME OVER")
    }
}
