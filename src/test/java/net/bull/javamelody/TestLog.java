/*
 * Copyright 2008-2010 by Emeric Vernat
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

import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.LoggerFactory;

/**
 * Test unitaire des classes Log4JAppender et LoggingHandler.
 * @author Emeric Vernat
 */
public class TestLog {
	private LogbackAppender logbackAppender;
	private Log4JAppender log4jAppender;
	private LoggingHandler loggingHandler;

	/** Initialisation. */
	@Before
	public void setUp() {
		Utils.initialize();
		logbackAppender = new LogbackAppender();
		log4jAppender = new Log4JAppender();
		loggingHandler = new LoggingHandler();
	}

	/** Test. */
	@Test
	public void testGetSingleton() {
		assertNotNull("getSingleton not null", LogbackAppender.getSingleton());
		assertNotNull("getSingleton not null", Log4JAppender.getSingleton());
		assertSame("getSingleton same", Log4JAppender.getSingleton(), Log4JAppender.getSingleton());
		assertNotNull("getSingleton not null", LoggingHandler.getSingleton());
		assertSame("getSingleton same", LoggingHandler.getSingleton(),
				LoggingHandler.getSingleton());
	}

	/** Test. */
	@SuppressWarnings("unchecked")
	@Test
	public void testRegister() {
		try {
			final int countAppendersBefore = Collections.list(
					Logger.getRootLogger().getAllAppenders()).size();
			logbackAppender.register();
			log4jAppender.register();
			loggingHandler.register();
			final int countAppendersAfter = Collections.list(
					Logger.getRootLogger().getAllAppenders()).size();
			assertSame("register", countAppendersBefore + 1, countAppendersAfter);
		} finally {
			logbackAppender.deregister();
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
		logbackAppender.register();
		log4jAppender.register();
		loggingHandler.register();
		final int countAppendersBefore = Collections.list(Logger.getRootLogger().getAllAppenders())
				.size();
		logbackAppender.deregister();
		log4jAppender.deregister();
		loggingHandler.deregister();
		final int countAppendersAfter = Collections.list(Logger.getRootLogger().getAllAppenders())
				.size();
		assertSame("register", countAppendersBefore - 1, countAppendersAfter);
	}

	/** Test. */
	@Test
	public void testClose() {
		logbackAppender.stop();
		log4jAppender.close();
		loggingHandler.close();
		assertNotNull("close", loggingHandler);
	}

	/** Test. */
	@Test
	public void testAppend() {
		try {
			logbackAppender.register();
			log4jAppender.register();
			loggingHandler.register();
			final Counter logCounter = LoggingHandler.getLogCounter();
			logCounter.clear();
			final int requestsCountBefore = logCounter.getRequestsCount();
			LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).info("test info logback");
			LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).warn("test warn logback");
			LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).warn("test warn logback",
					new IllegalStateException("test"));
			Logger.getRootLogger().info("test info log4j");
			Logger.getRootLogger().warn("test warn log4j");
			Logger.getRootLogger().warn("test warn log4j", new IllegalStateException("test"));
			java.util.logging.Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME)
					.warning("test 2");
			java.util.logging.Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME).info(
					"test 3");
			final int requestsCountAfter = logCounter.getRequestsCount();
			assertSame("append", requestsCountBefore + 3, requestsCountAfter);
		} finally {
			logbackAppender.deregister();
			log4jAppender.deregister();
			loggingHandler.deregister();
		}
	}

	/** Test. */
	@Test
	public void testDebugInfoAndWarn() {
		LOG.debug("test debug");
		LOG.debug("test debug", new IllegalStateException("test debug"));
		LOG.info("test info", new IllegalStateException("test info"));
		LOG.warn("test warn", new IllegalStateException("test warn"));
	}
}
