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

import static org.junit.Assert.assertNotNull;

import java.util.Hashtable;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.naming.spi.InitialContextFactory;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Test unitaire de la classe MonitoringInitialContextFactory.
 * @author Emeric Vernat
 */
public class TestMonitoringInitialContextFactory implements InitialContextFactory {
	private static Context initialContext;

	/** Initialisation.
	 * @throws NamingException e */
	@BeforeClass
	public static void init() throws NamingException {
		initialContext = new InitialContext();
	}

	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws NamingException e */
	@Test
	public void test() throws NamingException {
		try {
			Utils.setProperty(Context.INITIAL_CONTEXT_FACTORY, this.getClass().getName());
			try {
				MonitoringInitialContextFactory.init();
				new MonitoringInitialContextFactory().getInitialContext(null);
			} finally {
				MonitoringInitialContextFactory.stop();
			}

			Utils.setProperty(Context.INITIAL_CONTEXT_FACTORY, "xyz.nimportequoi");
			NamingException result = null;
			try {
				MonitoringInitialContextFactory.init();
				new MonitoringInitialContextFactory().getInitialContext(null);
			} catch (final NamingException e) {
				result = e;
			} finally {
				MonitoringInitialContextFactory.stop();
			}
			// namingexception si la classe n'existe pas
			assertNotNull("exception", result);
		} finally {
			Utils.setProperty(Context.INITIAL_CONTEXT_FACTORY, null);
		}
	}

	/** {@inheritDoc} */
	@Override
	public Context getInitialContext(Hashtable<?, ?> environment) throws NamingException { // NOPMD
		return initialContext;
	}
}
