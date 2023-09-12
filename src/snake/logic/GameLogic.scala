package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game.SnakeGame
import snake.logic.GameLogic._

class GameLogic(val random: RandomGenerator,
                val gridDims: Dimensions) {
  private var currentDirection: Direction = East()
  private var xHeadPosition = 5
  private var yHeadPosition = 3
  private var headPosition = Point(xHeadPosition, yHeadPosition)
  private var applePosition = Point(random.randomInt(gridDims.width - 1), random.randomInt(gridDims.height - 1))
  private var appleCount = 0
  private var snakeBody: List[Point] = List( //Initially define the snake to have a head and two body parts
    headPosition,
    Point(xHeadPosition - 1, yHeadPosition),
    Point(xHeadPosition - 2, yHeadPosition)
  )

  def gameOver: Boolean = false

  def step(): Unit = {
    headPosition = snakeMovement(headPosition, currentDirection)
    xHeadPosition = headPosition.x
    yHeadPosition = headPosition.y

    if (hasAppleBeenEaten()) {
      appleCount += 1
      applePosition = Point(random.randomInt(gridDims.width - 1), random.randomInt(gridDims.height - 1))
      snakeBodyExtender()
    } else {
      snakeBody = headPosition :: snakeBody.dropRight(1) //Adds all the elements to the body except the tail to make space for the head
    }
  }


  // Implement logic to set reverse direction
  def setReverse(r: Boolean): Unit = ()

  def changeDir(d: Direction): Unit = {
    currentDirection = d
  }

  def getCellType(p: Point): CellType = {
    if (headPosition == p) {
      SnakeHead(currentDirection)
    } else if (applePosition == p) {
      Apple()
    } else if (hasAppleBeenEaten()) {
      SnakeHead(currentDirection)
    } else if (snakeBody.contains(p)) {
      SnakeBody(0) //Define color of snakeBody
    } else {
      Empty()
    }
  }

  private def snakeMovement(position: Point, direction: Direction): Point = {
    direction match { //The modulo allows for the wraparound since if it surpasses the gridDims, it redefines based on the remainder
      case East()  => Point((position.x + 1) % gridDims.width, position.y)
      case West()  => Point((position.x - 1 + gridDims.width) % gridDims.width, position.y)
      case North() => Point(position.x, (position.y - 1 + gridDims.height) % gridDims.height)
      case South() => Point(position.x, (position.y + 1) % gridDims.height)
    }
  }

  private def hasAppleBeenEaten (): Boolean = {
    if (headPosition == applePosition) {
      return true
    }
    false
  }

  private def snakeBodyExtender(): Unit = {
    val newBlocks = List.fill(3)(headPosition)
    snakeBody = newBlocks ::: snakeBody //We don't add but make a new list with the new blocks since lists are immutable in Scala
  }
}


/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 5 // change this to increase/decrease speed of game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller

  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultGridDims.width and DefaultGridDims.height
  val DefaultGridDims
  : Dimensions =
  Dimensions(width = 25, height = 25)  // you can adjust these values to play on a different sized board

}




