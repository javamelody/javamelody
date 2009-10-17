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

import static org.junit.Assert.assertNotNull;

import java.lang.reflect.Method;
import java.util.Map;

import javax.interceptor.InvocationContext;

import org.junit.Test;

/**
 * Test unitaire de la classe MonitoringInterceptor.
 * @author Emeric Vernat
 */
public class TestMonitoringInterceptor {
	static class InvokeContext implements InvocationContext {
		public void setParameters(Object[] params) {
			// rien
		}

		public Object proceed() throws Exception {
			return null;
		}

		public Object getTarget() {
			return null;
		}

		public Object[] getParameters() {
			return null;
		}

		public Method getMethod() {
			try {
				return TestMonitoringInterceptor.class.getMethod("testInvoke", new Class[0]);
			} catch (SecurityException e) {
				throw new IllegalStateException(e);
			} catch (NoSuchMethodException e) {
				throw new IllegalStateException(e);
			}
		}

		public Map<String, Object> getContextData() {
			return null;
		}
	}

	/** Test. */
	@Test
	public void testNewInstance() {
		assertNotNull("new MonitoringInterceptor", new MonitoringInterceptor());
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testInvoke() throws Exception {
		new MonitoringInterceptor().intercept(new InvokeContext());
	}

	/** Test. */
	@Test
	public void testGetEjbCounter() {
		assertNotNull("getEjbCounter", MonitoringInterceptor.getEjbCounter());
	}
}
