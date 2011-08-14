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
	public Context getInitialContext(Hashtable<?, ?> environment) throws NamingException { // NOPMD
		return initialContext;
	}
}
