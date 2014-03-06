/*
 * Copyright (C) 2013 Time Out Group Limited
 * All rights reserved.
 */

package com.timeout
package common

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

/**
 * This is similar to Executors.DefaultThreadFactory, but it allows us to configure a couple of useful parameters.
 * Another difference is that no thread group is assigned to the created threads.
 */
class NamedThreadFactory(namePrefix: String, daemon: Boolean = false) extends ThreadFactory {

  private val threadNumber = new AtomicInteger(1)

  def newThread(r: Runnable): Thread = {
    val t = new Thread(r, namePrefix + "-" + threadNumber.getAndIncrement)
    t.setDaemon(daemon)
    if (t.getPriority != Thread.NORM_PRIORITY)
      t.setPriority(Thread.NORM_PRIORITY)
    t
  }

}
