package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game.SnakeGame
import snake.logic.GameLogic._

class GameLogic(val random: RandomGenerator, val gridDims: Dimensions) {
  private var currentDirection: Direction = East()
  private var headPosition = Point(2, 0)
  private var snakeBody: List[Point] = List (//Initially define the snake to have a head and two body parts
    headPosition,
    Point(headPosition.x - 1, headPosition.y),
    Point(headPosition.x - 2, headPosition.y)
  )
  var snakeLength = snakeBody.length
  var entireGrid: Seq[Point] = Seq()
  var emptyCells: List[Point] = List()
  private var applePosition = appleGenerator()
  private var gameConcluded = false
  private var validMove = false


  def gameOver: Boolean = gameConcluded

  def step(): Unit = {

    if (!gameConcluded) {

      headPosition = snakeMovement(headPosition, currentDirection)
      snakeBody = headPosition :: snakeBody.take(snakeLength - 1)
      
      if (collisionDetector()) {
        gameConcluded = true
      }

      if (hasAppleBeenEaten()) {
        applePosition = appleGenerator()
        snakeLength += 3 //We increase the snake length so that every step a new element is added to the body. This way the snake increases a block during each step and not all at once
      }
    }

    validMove = false
  }

  def setReverse(r: Boolean): Unit = ()

  def changeDir(d: Direction): Unit = {
      if (!validMove) {
        validMove = true
        oppositeDirectionDetector(d)
      }
  }

  private def oppositeDirectionDetector(direction: Direction): Unit = {
    if (currentDirection != direction.opposite) {
      currentDirection = direction
    } else {
      validMove = false
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
    if (emptyCells.nonEmpty) { //Applies when there are available free cells
      val randomIndex = random.randomInt(emptyCells.length)
      emptyCells(randomIndex)
    } else { //Applies when there is no place for the apple to spawn
      Point(-1, -1)
    }
  }

  private def collisionDetector(): Boolean = {
    if (snakeBody.tail.contains(headPosition)) {return true}
    false
  }
}

object GameLogic {
  val FramesPerSecond: Int = 4
  val DrawSizeFactor = 1.0
  val DefaultGridDims
  : Dimensions =
  Dimensions(width = 10, height = 10)
}