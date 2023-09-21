package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game.SnakeGame
import snake.logic.GameLogic._

case class GameState(
                      headPosition: Point,
                      snakeBody: List[Point],
                      snakeLength: Int,
                      applePosition: Point,
                      currentDirection: Direction,
                      lastValidDirection: Direction,
                      emptyCells: List[Point],
                      gameConcluded: Boolean,
                      directionChanged: Boolean
                    )

class GameLogic(val random: RandomGenerator, val gridDims: Dimensions) {

  // The only mutable elements. Variables for the gameState and reverse mode
  private var currentGameState: GameState = GameState(
    headPosition = Point(2, 0),
    snakeBody = List(Point(2, 0), Point(1, 0), Point(0, 0)),
    snakeLength = 3,
    applePosition = Point(-1, -1),
    currentDirection = East(),
    lastValidDirection = East(),
    emptyCells = List(),
    gameConcluded = false,
    directionChanged = false
  )
  private var gameStateStack: SStack[GameState] = SStack()
  private var reverseModeEnabled: Boolean = false
  currentGameState = currentGameState.copy(applePosition = appleGenerator())
  gameStateStack = gameStateStack.push(currentGameState.copy())

  def gameOver: Boolean = currentGameState.gameConcluded

  def setReverse(r: Boolean): Unit = {
    reverseModeEnabled = r
  }

  def step(): Unit = {

    if (reverseModeEnabled) {

      if (!gameStateStack.isEmpty) {
        gameStateStack = gameStateStack.pop
        currentGameState = gameStateStack.top
      }


    } else {

      if (currentGameState.gameConcluded) {
        return
      }

      oppositeDirectionDetector()
      currentGameState = currentGameState.copy(headPosition = snakeMovement(currentGameState.headPosition, currentGameState.currentDirection))
      currentGameState = currentGameState.copy(snakeBody = currentGameState.headPosition :: currentGameState.snakeBody.take(currentGameState.snakeLength - 1))

      if (collisionDetector()) {
        currentGameState = currentGameState.copy(gameConcluded = true)
      }

      if (hasAppleBeenEaten()) {
        currentGameState = currentGameState.copy(applePosition = appleGenerator())
        currentGameState = currentGameState.copy(snakeLength = currentGameState.snakeLength + 3)
      }

      gameStateStack = gameStateStack.push(currentGameState.copy())

    }

  }

  def changeDir(d: Direction): Unit = {
    if (!currentGameState.directionChanged && currentGameState.currentDirection != d.opposite) {
      currentGameState = currentGameState.copy(lastValidDirection = d)
      currentGameState = currentGameState.copy(directionChanged = true)
    }
  }

  private def oppositeDirectionDetector(): Unit = {
    if (currentGameState.directionChanged) {
      if (currentGameState.currentDirection != currentGameState.lastValidDirection.opposite) {
        currentGameState = currentGameState.copy(currentDirection = currentGameState.lastValidDirection)
      }
      currentGameState = currentGameState.copy(directionChanged = false)
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
    if (currentGameState.headPosition == currentGameState.applePosition) {
      return true
    }
    false
  }

  private def appleGenerator(): Point = {
    val entireGrid = gridDims.allPointsInside
    currentGameState = currentGameState.copy(emptyCells = entireGrid.filterNot(p => currentGameState.snakeBody.contains(p)).toList)
    if (currentGameState.emptyCells.nonEmpty) {
      val randomIndex = random.randomInt(currentGameState.emptyCells.length)
      currentGameState.emptyCells(randomIndex)
    } else {
      Point(-1, -1)
    }
  }

  private def collisionDetector(): Boolean = {
    if (currentGameState.snakeBody.tail.contains(currentGameState.headPosition)) {
      return true
    }
    false
  }

  def getCellType(p: Point): CellType = {
    if (currentGameState.headPosition == p) {
      SnakeHead(currentGameState.currentDirection)
    } else if (currentGameState.applePosition == p) {
      Apple()
    } else if (currentGameState.snakeBody.contains(p)) {
      val snakeBodyIndex = currentGameState.snakeBody.indexOf(p)
      val colorIndex = snakeBodyIndex.toFloat / (currentGameState.snakeBody.length - 1)
      SnakeBody(colorIndex)
    } else {
      Empty()
    }
  }

}

object GameLogic {
  val FramesPerSecond: Int = 5
  val DrawSizeFactor = 1.0
  val DefaultGridDims: Dimensions = Dimensions(width = 15, height = 15)
}
