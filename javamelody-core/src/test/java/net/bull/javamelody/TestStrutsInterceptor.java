/*
 * Copyright 2008-2012 by Emeric Vernat
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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import org.junit.Before;
import org.junit.Test;

import com.opensymphony.xwork2.ActionInvocation;
import com.opensymphony.xwork2.mock.MockActionProxy;

/**
 * Test unitaire de la classe StrutsInterceptor.
 * @author Emeric Vernat
 */
public class TestStrutsInterceptor {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testGetStrutsCounter() {
		assertNotNull("getStrutsCounter", MonitoringProxy.getStrutsCounter());
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testStruts() throws Exception { // NOPMD
		final Counter strutsCounter = MonitoringProxy.getStrutsCounter();
		strutsCounter.clear();
		final StrutsInterceptor strutsInterceptor = new StrutsInterceptor();
		final ActionInvocation invocation = createNiceMock(ActionInvocation.class);
		final MockActionProxy proxy = new MockActionProxy();
		proxy.setInvocation(invocation);
		proxy.setActionName("test.action");
		proxy.setMethod(null);
		proxy.setNamespace("testnamespace");
		expect(invocation.getProxy()).andReturn(proxy).anyTimes();

		replay(invocation);
		strutsCounter.setDisplayed(false);
		strutsInterceptor.intercept(invocation);
		final String requestsCount = "requestsCount";
		assertSame(requestsCount, 0, strutsCounter.getRequestsCount());

		strutsCounter.setDisplayed(true);
		strutsInterceptor.intercept(invocation);
		assertSame(requestsCount, 1, strutsCounter.getRequestsCount());
		verify(invocation);

		final ActionInvocation invocation2 = createNiceMock(ActionInvocation.class);
		final MockActionProxy proxy2 = new MockActionProxy();
		proxy2.setInvocation(invocation2);
		proxy2.setActionName("test2.action");
		proxy2.setMethod("execute");
		proxy2.setNamespace("testnamespace");
		expect(invocation2.getProxy()).andReturn(proxy2).anyTimes();

		replay(invocation2);
		strutsInterceptor.intercept(invocation2);
		assertSame(requestsCount, 2, strutsCounter.getRequestsCount());
		verify(invocation2);

		final ActionInvocation invocation3 = createNiceMock(ActionInvocation.class);
		final MockActionProxy proxy3 = new MockActionProxy();
		proxy3.setInvocation(invocation3);
		proxy3.setActionName("test3.action");
		proxy3.setMethod("testmethod");
		proxy3.setNamespace("testnamespace");
		expect(invocation3.getProxy()).andReturn(proxy3).anyTimes();
		expect(invocation3.invoke()).andThrow(new UnknownError("test d'erreur")).anyTimes();

		replay(invocation3);
		try {
			strutsInterceptor.intercept(invocation3);
		} catch (final UnknownError e) {
			assertNotNull("ok", e);
		}
		assertSame(requestsCount, 3, strutsCounter.getRequestsCount());
		verify(invocation3);
	}
}
