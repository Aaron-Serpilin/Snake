package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game.SnakeGame
import snake.logic.GameLogic._

class GameLogic(val random: RandomGenerator, val gridDims: Dimensions) {
  private var currentDirection: Direction = East()
  private var lastValidDirection: Direction = currentDirection
  private var headPosition = Point(2, 0)
  private var snakeBody: List[Point] = List(
    headPosition,
    Point(headPosition.x - 1, headPosition.y),
    Point(headPosition.x - 2, headPosition.y)
  )
  private var snakeLength = snakeBody.length
  private var entireGrid: Seq[Point] = Seq()
  private var emptyCells: List[Point] = List()
  private var applePosition = appleGenerator()
  private var gameConcluded = false
  private var directionChanged = false

  def gameOver: Boolean = gameConcluded

  def step(): Unit = {

    if (gameConcluded) {return}

    oppositeDirectionDetector()
    headPosition = snakeMovement(headPosition, currentDirection)
    snakeBody = headPosition :: snakeBody.take(snakeLength - 1) // Following the length's increase, body elements are added in each step until the length has been met

    if (collisionDetector()) {
      gameConcluded = true
    }

    if (hasAppleBeenEaten()) {
      applePosition = appleGenerator()
      snakeLength += 3 //We increase the snake length so that every step a new element is added to the body. This way the snake increases a block during each step and not all at once
    }

  }

  def setReverse(r: Boolean): Unit = ()

  def changeDir(d: Direction): Unit = {
    if (!directionChanged && currentDirection != d.opposite) {
      lastValidDirection = d
      directionChanged = true
    }
  }

  private def oppositeDirectionDetector () : Unit = {
    if (directionChanged) { // Triggers when currentDirection != d
      if (currentDirection != lastValidDirection.opposite) {
        currentDirection = lastValidDirection
      }
      directionChanged = false
    }
  }

  private def snakeMovement(position: Point, direction: Direction): Point = {
    direction match { //The modulo allows for the wraparound since if it surpasses the gridDims, it redefines based on the remainder
      case East() => Point((position.x + 1) % gridDims.width, position.y)
      case West() => Point((position.x - 1 + gridDims.width) % gridDims.width, position.y)
      case North() => Point(position.x, (position.y - 1 + gridDims.height) % gridDims.height)
      case South() => Point(position.x, (position.y + 1) % gridDims.height)
    }
  }

  private def hasAppleBeenEaten(): Boolean = {
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
    if (snakeBody.tail.contains(headPosition)) { // The tail method checks if every element except the head contains the head, which would mean a collision of the snake with itself
      return true
    }
    false
  }

  def getCellType(p: Point): CellType = {
    if (headPosition == p) {
      SnakeHead(currentDirection)
    } else if (applePosition == p) { // Needed to check (&& emptyCells.nonEmpty) before, no longer necessary since it is integrated in the appleGenerator()
      Apple()
    } else if (snakeBody.contains(p)) {
      val snakeBodyIndex = snakeBody.indexOf(p)
      val colorIndex = snakeBodyIndex.toFloat / (snakeBody.length - 1)
      SnakeBody(colorIndex) //Define color of snakeBody
    } else {
      Empty()
    }
  }
}

object GameLogic {
  val FramesPerSecond: Int = 4
  val DrawSizeFactor = 1.0
  val DefaultGridDims: Dimensions = Dimensions(width = 10, height = 10)
}

