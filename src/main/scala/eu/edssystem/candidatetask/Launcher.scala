package eu.edssystem.candidatetask

import java.io.{FileNotFoundException, File}
import java.util.concurrent.{Executors, ExecutorService}

import eu.edssystem.candidatetask.model.Square

import scala.collection.immutable.Queue
import scala.io.Source
;

object Launcher extends App {

  start()

  private def start() {
    if(args.length<1){
      println("no file path specified")
      return
    }

    val inputFile: File = new File(args(0))
    try {
      val squares = parseFile(inputFile)
      findAndPrintCombinations(squares)
    }
    catch {
      case ex: FileNotFoundException => println("No file was found ", ex)
      case ex: NumberFormatException => println("Wrong number format in files. ", ex)
    }
  }

  private def parseFile(inputFile: File): Seq[Square] = {
    val squares = for {
      (line, i) <- Source.fromFile(inputFile).getLines().zipWithIndex
    } yield parseLine(line, i)
    squares.to[List]
  }

  private def parseLine(line: String, lineIndex: Int): Square = {
    val values = for {
      el <- line.split(" ")
    } yield el.toInt
    Square(values(0), values(1), values(2), values(3), lineIndex)
  }

  private def findAndPrintCombinations(squares: Seq[Square]):Unit = findAndPrintCombinations(Queue(), squares)

  private def findAndPrintCombinations(headSqrs: Queue[Square],
                               tailSqrs: Seq[Square]):Unit ={

    if (tailSqrs.isEmpty && verify(headSqrs)) {
      println(formatAnswer(headSqrs))
      return
    }
    for ((square, i) <- tailSqrs.zipWithIndex) {
      val nextHead = headSqrs.enqueue(square)
      val nextTail = tailSqrs.updated(i, tailSqrs.head).tail
      if (verify(nextHead)) {
        findAndPrintCombinations(nextHead, nextTail)
      }
    }

  }

  private def verify(squares: Seq[Square]): Boolean = CombinationVerifier.verify(squares)

  private def formatAnswer(squares: Queue[Square]): String = {
    val stringBuf = new StringBuilder
    squares.foreach(square => stringBuf.append(square.formattedString))
    stringBuf.append(System.lineSeparator())
    stringBuf.toString()
  }


}
