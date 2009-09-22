/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import java.util.Collections;

import net.bull.javamelody.Counter;
import net.bull.javamelody.Log4JAppender;
import net.bull.javamelody.LoggingHandler;

import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Test;


/**
 * Test unitaire des classes Log4JAppender et LoggingHandler.
 * @author Emeric Vernat
 */
public class TestLog {
	private Log4JAppender log4jAppender;
	private LoggingHandler loggingHandler;

	/** Initialisation. */
	@Before
	public void setUp() {
		log4jAppender = new Log4JAppender();
		loggingHandler = new LoggingHandler();
	}

	/** Test. */
	@Test
	public void testGetSingleton() {
		assertNotNull("getSingleton not null", Log4JAppender.getSingleton());
		assertSame("getSingleton same", Log4JAppender.getSingleton(), Log4JAppender.getSingleton());
		assertNotNull("getSingleton not null", LoggingHandler.getSingleton());
		assertSame("getSingleton same", LoggingHandler.getSingleton(), LoggingHandler
				.getSingleton());
	}

	/** Test. */
	@SuppressWarnings("unchecked")
	@Test
	public void testRegister() {
		try {
			final int countAppendersBefore = Collections.list(
					Logger.getRootLogger().getAllAppenders()).size();
			log4jAppender.register();
			loggingHandler.register();
			final int countAppendersAfter = Collections.list(
					Logger.getRootLogger().getAllAppenders()).size();
			assertSame("register", countAppendersBefore + 1, countAppendersAfter);
		} finally {
			log4jAppender.deregister();
			loggingHandler.deregister();
		}
	}

	/** Test. */
	@Test
	public void testRequiresLayout() {
		// requiresLayout ne sert à rien sans configurator
		assertFalse("requiresLayout", log4jAppender.requiresLayout());
	}

	/** Test. */
	@Test
	public void testFlush() {
		loggingHandler.flush();
		assertNotNull("flush", loggingHandler);
	}

	/** Test. */
	@SuppressWarnings("unchecked")
	@Test
	public void testDeregister() {
		log4jAppender.register();
		loggingHandler.register();
		final int countAppendersBefore = Collections.list(Logger.getRootLogger().getAllAppenders())
				.size();
		log4jAppender.deregister();
		loggingHandler.deregister();
		final int countAppendersAfter = Collections.list(Logger.getRootLogger().getAllAppenders())
				.size();
		assertSame("register", countAppendersBefore - 1, countAppendersAfter);
	}

	/** Test. */
	@Test
	public void testClose() {
		log4jAppender.close();
		loggingHandler.close();
		assertNotNull("close", loggingHandler);
	}

	/** Test. */
	@Test
	public void testAppend() {
		try {
			log4jAppender.register();
			loggingHandler.register();
			final Counter logCounter = LoggingHandler.getLogCounter();
			logCounter.clear();
			final int requestsCountBefore = logCounter.getRequestsCount();
			Logger.getRootLogger().warn("test 1");
			Logger.getRootLogger().warn("test 1", new IllegalStateException("test"));
			java.util.logging.Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME)
					.warning("test 2");
			java.util.logging.Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME).info(
					"test 3");
			final int requestsCountAfter = logCounter.getRequestsCount();
			assertSame("append", requestsCountBefore + 2, requestsCountAfter);
		} finally {
			log4jAppender.deregister();
			loggingHandler.deregister();
		}
	}
}
