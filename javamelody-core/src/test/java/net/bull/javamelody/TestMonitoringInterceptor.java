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
import static org.junit.Assert.assertSame;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.Map;

import javax.interceptor.InvocationContext;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.internal.model.Counter;

/**
 * Test unitaire de la classe MonitoringInterceptor.
 * @author Emeric Vernat
 */
public class TestMonitoringInterceptor {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	static class InvokeContext implements InvocationContext {
		private final boolean throwError;

		InvokeContext(boolean throwError) {
			super();
			this.throwError = throwError;
		}

		@Override
		public void setParameters(Object[] params) {
			// rien
		}

		@Override
		public Object proceed() {
			if (throwError) {
				throw new OutOfMemoryError("test");
			}
			return null;
		}

		@Override
		public Object getTarget() {
			return new TestMonitoringInterceptor();
		}

		@Override
		public Object[] getParameters() {
			return null;
		}

		@Override
		public Method getMethod() {
			try {
				return TestMonitoringInterceptor.class.getMethod("testInvoke");
			} catch (final SecurityException e) {
				throw new IllegalStateException(e);
			} catch (final NoSuchMethodException e) {
				throw new IllegalStateException(e);
			}
		}

		@Override
		public Map<String, Object> getContextData() {
			return null;
		}

		@Override
		public Object getTimer() {
			return null;
		}

		@Override
		public Constructor<?> getConstructor() {
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
		final Counter ejbCounter = MonitoringProxy.getEjbCounter();
		ejbCounter.clear();
		final MonitoringInterceptor interceptor = new MonitoringInterceptor();

		ejbCounter.setDisplayed(false);
		interceptor.intercept(new InvokeContext(false));
		assertSame("requestsCount", 0, ejbCounter.getRequestsCount());

		ejbCounter.setDisplayed(true);
		interceptor.intercept(new InvokeContext(false));
		assertSame("requestsCount", 1, ejbCounter.getRequestsCount());

		ejbCounter.clear();
		try {
			interceptor.intercept(new InvokeContext(true));
		} catch (final Error e) {
			assertSame("requestsCount", 1, ejbCounter.getRequestsCount());
		}
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testMonitoringTarget() throws Exception {
		final Counter ejbCounter = MonitoringProxy.getEjbCounter();
		ejbCounter.clear();
		final MonitoringTargetInterceptor interceptor = new MonitoringTargetInterceptor();

		ejbCounter.setDisplayed(true);
		interceptor.intercept(new InvokeContext(false));
		assertSame("requestsCount", 1, ejbCounter.getRequestsCount());
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testMonitoringCdi() throws Exception {
		final Counter ejbCounter = MonitoringProxy.getEjbCounter();
		ejbCounter.clear();
		final MonitoringCdiInterceptor interceptor = new MonitoringCdiInterceptor();

		ejbCounter.setDisplayed(true);
		interceptor.intercept(new InvokeContext(false));
		assertSame("requestsCount", 1, ejbCounter.getRequestsCount());
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testMonitoringAsynchronousCdi() throws Exception {
		final Counter ejbCounter = MonitoringProxy.getEjbCounter();
		ejbCounter.clear();
		final MonitoringAsynchronousCdiInterceptor interceptor = new MonitoringAsynchronousCdiInterceptor();

		ejbCounter.setDisplayed(true);
		interceptor.intercept(new InvokeContext(false));
		assertSame("requestsCount", 1, ejbCounter.getRequestsCount());
	}

	/** Test. */
	@Test
	public void testGetEjbCounter() {
		assertNotNull("getEjbCounter", MonitoringProxy.getEjbCounter());
	}
}
