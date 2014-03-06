/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout

import scala.collection.mutable.ArrayBuffer

sealed trait Validation[+E, +A] {

  def ++[EE, B](that: Validation[Seq[EE], B])(implicit ev: E <:< Seq[EE]): Validation[Seq[EE], (A, B)] = 
    (this, that) match {
      case (Success(a), Success(b)) => Success((a, b))
      case (Failure(e), Success(_)) => Failure(e)
      case (Success(_), Failure(e)) => Failure(e)
      case (Failure(eA), Failure(eB)) => Failure(eA ++ eB)
    }

  def assert: A = this match {
    case Success(a) => a
    case Failure(e) => throw new AssertionError(e)
  }

  def fold[X](failure: E => X = identity[E] _, success: A => X = identity[A] _): X = this match {
    case Success(x) => success(x)
    case Failure(x) => failure(x)
  }

  def map[B](f: A => B): Validation[E, B] = this match {
    case Success(a) => Success(f(a))
    case Failure(e) => Failure(e)
  }

  def fail = new FailProjection[E, A](Validation.this)

  def flatMap[EE >: E, B](f: A => Validation[EE, B]): Validation[EE, B] = this match {
    case Success(a) => f(a)
    case Failure(e) => Failure(e)
  }

  def foreach[U](f: A => U): Unit = this match {
    case Success(a) => f(a)
    case Failure(e) =>
  }

  def either: Either[E, A] = this match {
    case Success(a) => Right(a)
    case Failure(e) => Left(e)
  }

  def getOrThrow(implicit ev: E <:< Throwable): A = this match {
    case Success(a) => a
    case Failure(e) => throw e
  }

  def getOrElse[B >: A](f: E => B): B = this match {
    case Success(a) => a
    case Failure(e) => f(e)
  }

  def getOrError(implicit ev: E <:< String): A = getOrElse(sys.error(_))

  def isSuccess: Boolean = this match {
    case Success(_) => true
    case Failure(_) => false
  }

  def isFailure: Boolean = !isSuccess

  def toOption: Option[A] = this match {
    case Success(a) => Some(a)
    case Failure(_) => None
  }
}

final class FailProjection[+E, +A](val validation: Validation[E, A]) extends AnyVal {
  def validationSeq = map[Seq[E]](List(_))

  def map[EE](f: E => EE): Validation[EE, A] = validation match {
    case Success(a) => Success(a)
    case Failure(e) => Failure(f(e))
  }

  def toOption: Option[E] = validation match {
    case Success(_) => None
    case Failure(e) => Some(e)
  }
}

final case class Success[E, A](a: A) extends Validation[E, A]
final case class Failure[E, A](e: E) extends Validation[E, A]

object Validation {
  class Catcher[E <: Throwable : Manifest] {

    def option[U](f: => U): Option[U] =
      try { Some(f) } catch { case _: E => None }

    def validation[U](f: => U): Validation[E, U] =
      try { Success(f) } catch { case e: E => Failure(e) }

  }

  def apply[A](a: A) = Success(a) 

  def checks[E, A](a: A)(fs: PartialFunction[A, E]*): Validation[Seq[E], A] =
    fs.flatMap(PartialFunction.condOpt(a)(_)) match {
      case Seq() => Success(a)
      case xs => Failure(xs)
    }

  def allCatch[U](f: => U): Validation[Throwable, U] =
    try { Success(f) } catch { case e: Throwable => Failure(e) }

  def catching[T <: Throwable : Manifest] = new Catcher[T]

  def flatSequence[E, T](xs: Seq[Validation[Seq[E], T]]): Validation[Seq[E], Seq[T]] = {
    val failures = ArrayBuffer[E]()
    val successes = ArrayBuffer[T]()

    xs.foreach {
      case Failure(x) => failures ++= x
      case Success(x) => if (failures.isEmpty) successes += x
    }

    if (failures.isEmpty) Success(successes)
    else Failure(failures)
  }

  def sequence[E, T](xs: Seq[Validation[E, T]]): Validation[Seq[E], Seq[T]] = {
    val failures = ArrayBuffer[E]()
    val successes = ArrayBuffer[T]()

    xs.foreach {
      case Failure(x) => failures += x
      case Success(x) => if (failures.isEmpty) successes += x
    }

    if (failures.isEmpty) Success(successes)
    else Failure(failures)
  }

  def partition[E, T](xs: Seq[Validation[E, T]]): (Seq[E], Seq[T]) =
    xs.foldLeft((Seq.empty[E], Seq.empty[T])) {
      case ((failures, successes), Success(x)) => failures -> (x +: successes)
      case ((failures, successes), Failure(x)) => (x +: failures) -> successes
    }
}
