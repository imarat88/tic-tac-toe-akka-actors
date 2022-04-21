package com.tictactoe

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import collection.mutable.Set
import scala.collection.mutable
import scala.util.Random

class PlayerO extends Actor{

  val moves: Set[Int] = Set()
  val allmoves = mutable.Set(1,2,3,4,5,6,7,8,9)

  override def receive: Receive = {
    case StateX(step, xmoves: mutable.Set[Int])    => {

      // exclude opponents moves from the set
      xmoves.foreach(m => allmoves.remove(m))

      // select new random move for playerO
      val move = allmoves.toList(Random.nextInt(allmoves.size))

      // remove self move from the set
      allmoves.remove(move)

      //updating set of playerO moves to return it later in a StateO to Main actor
      moves.add(move)
      println(s"${moves.mkString(", ")} PlayerO moves")

      sender() ! StateO(step+1, moves)
    }
  }
}

class PlayerX extends Actor{

  val allmoves = Set(1,2,3,4,5,6,7,8,9)
  val winningCombination = Set(Set(1, 2, 3), Set(4, 5, 6), Set(7, 8, 9), Set(1, 4, 7), Set(2, 5, 8), Set(3, 6, 9), Set(1, 5, 9), Set(3, 5, 7))
  val remainingCombination = Set(Set(1, 2, 3), Set(4, 5, 6), Set(7, 8, 9), Set(1, 4, 7), Set(2, 5, 8), Set(3, 6, 9), Set(1, 5, 9), Set(3, 5, 7))

  val moves: Set[Int]  = Set()

  // selecting a move from available remaining winning moves
  def selectMove(opponentMoves: Set[Int]) = {
    // excluding all winning combination sets that were spoiled by opponent
    opponentMoves.map{ m =>
      remainingCombination.map{s =>
        if(s.contains(m)) {
          remainingCombination.remove(s)
        }
      }
    }
    // ordering sets from least to most.
    // the least number set means that they are already halfway filled by playerX and it has to make fewer moves to win
    val rwc = remainingCombination.toList.sortWith((a,b) => a.size<b.size)
    rwc.size match {
      case s:Int if s>0 => rwc(0)

      // in case of the situation where all winning combinations were spoiled it selects all possible remaining moves
      case s:Int if s < 1 => allmoves
    }
  }

  // marking moves from remaining winning combinations that were already made
  def reduceRemainingCombinations(move: Int) = {
    remainingCombination.foreach{ s =>
      s.remove(move)
    }
  }

  override def receive: Receive = {
    case StateO(step, omoves: mutable.Set[Int])    => {
      // removing moves that were already made by opponent from all possible moves
      omoves.foreach(m => allmoves.remove(m))
      // selecting a move from available remaining winning moves
      val winningMoves = selectMove(omoves)
      // selecting the first move from the possible set selected on a previous step
      val move = winningMoves.head
      // marking moves from remaining winning combinations that were already made
      reduceRemainingCombinations(move)

      // removing moves that were already made by playerX from all possible moves
      allmoves.remove(move)

      //updating set of playerX moves to return it later in a StateX to Main actor
      moves.add(move)
      println(s"${moves.mkString(", ")} PlayerX moves")
      sender() ! StateX(step+1, moves)
    }
  }
}


class Game(playerX: ActorRef, playerO: ActorRef) extends Actor {

  val players = List(playerO,playerX)

  var moveNumber = 0
  val winningCombination = Set(Set(1, 2, 3), Set(4, 5, 6), Set(7, 8, 9), Set(1, 4, 7), Set(2, 5, 8), Set(3, 6, 9), Set(1, 5, 9), Set(3, 5, 7))

  def player(step:Int) = players(step % 2)

  // checking whether the player has won or not
  def hasWon(moves: Set[Int]): Boolean = {
    winningCombination exists ((combination: Set[Int]) => (combination intersect moves).size == 3)
  }

  override def receive: Receive = {
    case StateO(step, moves)      => {
      if (hasWon(moves)) {
        println(s"${player(moveNumber+1)} won and moves are ${moves.mkString(", ")}")
        context.stop(self)
      } else {
        // sending received state to opponent player
        player(moveNumber) ! StateO(step, moves)
      }
      moveNumber += 1
    }
    case StateX(step, moves) => {
      if (hasWon(moves)) {
        println(s"${player(moveNumber+1)} won and moves are ${moves.mkString(", ")}")
        context.stop(self)
      } else {
        // sending received state to opponent player
        player(moveNumber) ! StateX(step, moves)
      }
      moveNumber += 1
    }
  }
}

case class StateO(step: Int, moves: Set[Int])
case class StateX(step: Int, moves: Set[Int])

object TicTacMain extends App{

  val system = ActorSystem("MySystem")

  val playerO = system.actorOf(Props[PlayerO](), "playerO")
  val playerX = system.actorOf(Props[PlayerX](), "playerX")

  val game = system.actorOf(Props(new Game(playerX, playerO)), "game")

  // initiating the first move with empty state
  // playerO moves randomly while playerX responds while algorithmically calculating winning combinations
  game ! StateX(0,Set())
}