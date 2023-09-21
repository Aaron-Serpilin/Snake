package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game.SnakeGame
import snake.logic.GameLogic._

// Immutable stack structure given by the Sokoban logic file
case class SStack[A](l : List[A]) {
  def size : Int = l.length
  def isEmpty : Boolean = l.isEmpty
  def top : A = l.head
  def pop : SStack[A] = SStack(l.tail)
  def push(a : A) : SStack[A] = SStack(a :: l)
}

object SStack {
  def apply[A](a: A): SStack[A] = SStack(List(a))
  def apply[A](): SStack[A] = SStack(List())
}

case class GameState(
                      snakeBody: List[Point],
                      snakeLength: Int,
                      applePosition: Point
                    )

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

  // Variables for the gameState and reverse mode
  private var gameStateStack: SStack[GameState] = SStack()
  private var reverseModeEnabled: Boolean = false

  def gameOver: Boolean = gameConcluded

  def setReverse(r: Boolean): Unit = {
    if (r) {
      gameStateStack = gameStateStack.push(GameState(snakeBody, snakeLength, applePosition))
    } else {
      gameStateStack = SStack() // Clear the stack by creating a new empty SStack
    }
    reverseModeEnabled = r
  }

  def rewindGameState(): Unit = {
    if (reverseModeEnabled && !gameStateStack.isEmpty) {
      val prevState = gameStateStack.top
      snakeBody = prevState.snakeBody
      snakeLength = prevState.snakeLength
      applePosition = prevState.applePosition
      gameConcluded = false
      gameStateStack = gameStateStack.pop // Update the stack by removing the top element
    }
  }

  def step(): Unit = {
    if (reverseModeEnabled) {
      rewindGameState()
    }

    if (gameConcluded) {
      return
    }

    oppositeDirectionDetector()
    headPosition = snakeMovement(headPosition, currentDirection)
    snakeBody = headPosition :: snakeBody.take(snakeLength - 1)

    if (collisionDetector()) {
      gameConcluded = true
    }

    if (hasAppleBeenEaten()) {
      applePosition = appleGenerator()
      snakeLength += 3
    }

    if (reverseModeEnabled) {
      gameStateStack = gameStateStack.push(GameState(snakeBody, snakeLength, applePosition))
    }
  }

  def changeDir(d: Direction): Unit = {
    if (!directionChanged && currentDirection != d.opposite) {
      lastValidDirection = d
      directionChanged = true
    }
  }

  private def oppositeDirectionDetector(): Unit = {
    if (directionChanged) {
      if (currentDirection != lastValidDirection.opposite) {
        currentDirection = lastValidDirection
      }
      directionChanged = false
    }
  }

  private def snakeMovement(position: Point, direction: Direction): Point = {
    direction match {
      case East()  => Point((position.x + 1) % gridDims.width, position.y)
      case West()  => Point((position.x - 1 + gridDims.width) % gridDims.width, position.y)
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
    if (emptyCells.nonEmpty) {
      val randomIndex = random.randomInt(emptyCells.length)
      emptyCells(randomIndex)
    } else {
      Point(-1, -1)
    }
  }

  private def collisionDetector(): Boolean = {
    if (snakeBody.tail.contains(headPosition)) {
      return true
    }
    false
  }

  def getCellType(p: Point): CellType = {
    if (headPosition == p) {
      SnakeHead(currentDirection)
    } else if (applePosition == p) {
      Apple()
    } else if (snakeBody.contains(p)) {
      val snakeBodyIndex = snakeBody.indexOf(p)
      val colorIndex = snakeBodyIndex.toFloat / (snakeBody.length - 1)
      SnakeBody(colorIndex)
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
