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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import java.util.HashMap;

import org.junit.Before;
import org.junit.Test;

import com.opensymphony.xwork2.ActionContext;
import com.opensymphony.xwork2.ActionInvocation;

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
		final ActionContext context = new ActionContext(new HashMap<Object, Object>());
		context.setName("test.action");
		expect(invocation.getInvocationContext()).andReturn(context).anyTimes();

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
		final ActionContext context2 = new ActionContext(new HashMap<Object, Object>());
		context2.setName("test2.action");
		expect(invocation2.getInvocationContext()).andReturn(context2).anyTimes();
		expect(invocation2.invoke()).andThrow(new UnknownError("test d'erreur")).anyTimes();

		replay(invocation2);
		try {
			strutsInterceptor.intercept(invocation2);
		} catch (final UnknownError e) {
			assertNotNull("ok", e);
		}
		assertSame(requestsCount, 2, strutsCounter.getRequestsCount());
		verify(invocation2);
	}
}
