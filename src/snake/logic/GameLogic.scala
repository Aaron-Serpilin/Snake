package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game.SnakeGame
import snake.logic.GameLogic._

class GameLogic(val random: RandomGenerator,
                val gridDims: Dimensions) {
  private var currentDirection: Direction = East()
  private var xHeadPosition = 2
  private var yHeadPosition = 0
  private var headPosition = Point(xHeadPosition, yHeadPosition)
  private var snakeBody: List[Point] = List (//Initially define the snake to have a head and two body parts
    headPosition,
    Point(xHeadPosition - 1, yHeadPosition),
    Point(xHeadPosition - 2, yHeadPosition)
  )
  var snakeLength = snakeBody.length
  var entireGrid: Seq[Point] = Seq()
  var emptyCells: List[Point] = List()
  private var applePosition = appleGenerator()
  private var gameConcluded = false


  def gameOver: Boolean = gameConcluded

  def step(): Unit = {
    //println(s"The apple location is ${applePosition}")
    if (collisionDetector()) {gameConcluded = true}
    headPosition = snakeMovement(headPosition, currentDirection)
    xHeadPosition = headPosition.x
    yHeadPosition = headPosition.y

    snakeBody = headPosition :: snakeBody.take(snakeLength - 1)

    if (hasAppleBeenEaten()) {
      applePosition = appleGenerator()
      snakeLength += 3 //We increase the snake length so that every step a new element is added to the body. This way the snake increases a block during each step and not all at once
    }

  }

  def setReverse(r: Boolean): Unit = ()

  def changeDir(d: Direction): Unit = {
    if (!oppositeDirectionDetector(d, currentDirection)) {
      currentDirection = d
    }
  }

  private def oppositeDirectionDetector(previousDirection: Direction, currentDirection: Direction): Boolean = {
    (previousDirection, currentDirection) match {
      case (East(), West()) => true
      case (West(), East()) => true
      case (North(), South()) => true
      case (South(), North()) => true
      case _ => false
    }
  }


  def getCellType(p: Point): CellType = {
    if (headPosition == p) {
      SnakeHead(currentDirection)
    } else if (applePosition == p && emptyCells.nonEmpty) {
      Apple()
    } else if (snakeBody.contains(p)) {
      val snakeBodyIndex = snakeBody.indexOf(p)
      val colorIndex = snakeBodyIndex.toFloat / (snakeBody.length - 1)
      SnakeBody(colorIndex) //Define color of snakeBody
    } else {
      Empty()
    }

//    else if (hasAppleBeenEaten()) {
//      SnakeHead(currentDirection)
//    }
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

  private def appleGenerator(): Point = {
    entireGrid = gridDims.allPointsInside
    emptyCells = entireGrid.filterNot(p => snakeBody.contains(p)).toList
    //println(s"The number of empty cells is ${emptyCells.length}")
    if (emptyCells.nonEmpty) { //Applies when there are available free cells
      val randomIndex = random.randomInt(emptyCells.length)
      //println(s"The index is ${randomIndex}")
      emptyCells(randomIndex)
    } else { //Applies when there is no place for the apple to spawn
      //println("-1 invoked")
      Point(-1, -1)
    }
  }


  private def collisionDetector(): Boolean = {
    if (snakeBody.tail.contains(headPosition)) {return true}
    false
  }
}


/** GameLogic companion object */
object GameLogic {
  val FramesPerSecond: Int = 1
  val DrawSizeFactor = 1.0
  val DefaultGridDims
  : Dimensions =
  Dimensions(width = 6, height = 1)
}