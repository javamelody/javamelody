/*
 * Copyright 2008-2019 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;

import java.util.Collections;

import org.apache.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.LoggerFactory;

import net.bull.javamelody.internal.model.Counter;

/**
 * Test unitaire des classes Log4JAppender, LogbackAppender et LoggingHandler.
 * @author Emeric Vernat
 */
public class TestLog {
	private LogbackAppender logbackAppender;
	private Log4JAppender log4jAppender;
	private Log4J2Appender log4j2Appender;
	private LoggingHandler loggingHandler;

	/** Initialisation. */
	@Before
	public void setUp() {
		Utils.initialize();
		logbackAppender = new LogbackAppender();
		log4jAppender = new Log4JAppender();
		log4j2Appender = new Log4J2Appender();
		loggingHandler = new LoggingHandler();
	}

	/** Test. */
	@Test
	public void testGetSingleton() {
		assertNotNull("getSingleton not null", LogbackAppender.getSingleton());
		assertNotNull("getSingleton not null", Log4J2Appender.getSingleton());
		assertSame("getSingleton same", Log4J2Appender.getSingleton(),
				Log4J2Appender.getSingleton());
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
			final int countAppendersBefore = Collections
					.list(Logger.getRootLogger().getAllAppenders()).size();
			logbackAppender.register();
			log4jAppender.register();
			log4j2Appender.register();
			loggingHandler.register();
			final int countAppendersAfter = Collections
					.list(Logger.getRootLogger().getAllAppenders()).size();
			assertSame("register", countAppendersBefore + 1, countAppendersAfter);
		} finally {
			logbackAppender.deregister();
			log4jAppender.deregister();
			log4j2Appender.deregister();
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
		log4j2Appender.register();
		loggingHandler.register();
		final int countAppendersBefore = Collections.list(Logger.getRootLogger().getAllAppenders())
				.size();
		logbackAppender.deregister();
		log4jAppender.deregister();
		log4j2Appender.deregister();
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
			log4j2Appender.register();
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
			LogManager.getRootLogger().error("test error log4j 2");
			LogManager.getRootLogger().error("test error log4j 2",
					new IllegalStateException("test"));
			java.util.logging.Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME)
					.warning("test 2");
			java.util.logging.Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME)
					.info("test 3");
			final int requestsCountAfter = logCounter.getRequestsCount();
			// cela peut ne pas être égal si un autre thread a loggué des warnings en même temps
			if (requestsCountAfter < requestsCountBefore + 4) {
				fail("testAppend failed, requests : " + logCounter.getRequests());
			}
		} finally {
			logbackAppender.deregister();
			log4jAppender.deregister();
			log4j2Appender.deregister();
			loggingHandler.deregister();
		}
	}
}
